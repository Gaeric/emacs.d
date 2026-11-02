;; init-agent-shell-docker.el -*- coding: utf-8; lexical-binding: t; -*-
;;; Code:

(require 'seq)
(require 'map)
(require 'subr-x)

(declare-function agent-shell-get-config "agent-shell")

(defcustom my/agent-shell-docker-projects
  '(("/home/zero/data/packages/sgs" . "goose-sgs")
    ("/home/zero/data/bevy_inbox/" . "goose-bevy_inbox"))
  "Projects whose agents should run inside a container.

Each entry is (DIRECTORY . CONTAINER-NAME).  All subdirectories of
DIRECTORY match; the directory contents must be mounted into the
container at the same absolute path.  A CONTAINER-NAME of nil means the
project still runs locally."
  :type '(alist :key-type directory :value-type (choice string (const :tag "Local" nil)))
  :group 'agent-shell)

(defvar my/agent-shell-docker-force-local nil
  "When non-nil, ignore the list above and run everything locally.
This is an emergency escape hatch.")

(defcustom my/agent-shell-docker-identifiers '(goose)
  "Which agents run inside the container.  Only goose by default; add
claude-code/opencode here if you want them containerized as well."
  :type '(repeat symbol)
  :group 'agent-shell)

(defcustom my/agent-shell-docker-extra-args nil
  "Extra arguments appended to `docker exec', e.g. (\"-e\" \"FOO=bar\")."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom my/agent-shell-docker-auto-start t
  "Whether to automatically `docker start' a container that exists but is
not running."
  :type 'boolean
  :group 'agent-shell)

(defcustom my/agent-shell-docker-verify-mounted t
  "Check before startup whether the project directory exists inside the container.
When the directory is not mounted into the container, goose reports a very
hard-to-understand error; this gives a clear message up front.  Set to nil to
skip the ~50ms check on every startup."
  :type 'boolean
  :group 'agent-shell)

;;; Directory -> container

(defun my/agent-shell-docker--truedir (dir)
  "Normalize DIR to an absolute path with a trailing slash.
Return nil for remote directories."
  (when (and (stringp dir)
             (not (file-remote-p dir)))
    (file-name-as-directory
     (condition-case nil (file-truename dir) (error (expand-file-name dir))))))

(defun my/agent-shell-docker-container-for (buffer)
  "Return the container name for BUFFER's `default-directory', or nil if none."
  (let ((dir (my/agent-shell-docker--truedir
              (buffer-local-value 'default-directory buffer))))
    (when dir
      (seq-some (lambda (spec)
                  (let ((root (my/agent-shell-docker--truedir (car spec))))
                    (and root
                         (string-prefix-p root dir)
                         (cdr spec))))
                my/agent-shell-docker-projects))))

;;; Container state

(defun my/agent-shell-docker--inspect (container format)
  "Run docker inspect -f FORMAT on CONTAINER.
Return the output, or nil on failure."
  (with-temp-buffer
    (when (zerop (process-file "docker" nil t nil "inspect" "-f" format container))
      (string-trim (buffer-string)))))

(defun my/agent-shell-docker--ensure-container (container)
  "Make sure CONTAINER exists and is running, returning non-nil."
  (let ((running (my/agent-shell-docker--inspect container "{{.State.Running}}")))
    (cond
     ((null running)
      (user-error "agent-shell: container %s not found (docker inspect failed)" container))
     ((string= running "true") t)
     (my/agent-shell-docker-auto-start
      (message "agent-shell: starting container %s ..." container)
      (unless (zerop (process-file "docker" nil nil nil "start" container))
        (user-error "agent-shell: failed to start container %s" container))
      t)
     (t (user-error "agent-shell: container %s is not running (docker start %s)" container container)))))

(defun my/agent-shell-docker--dir-visible-p (container dir)
  "Check whether DIR exists and is a directory inside CONTAINER."
  (zerop (process-file "docker" nil nil nil "exec" container "test" "-d" dir)))

;;; Prefix

(defun my/agent-shell-docker--agent-identifier (buffer)
  "Return the agent identifier for BUFFER (goose, claude-code, etc.).
Return nil if it is unavailable."
  (ignore-errors (map-elt (agent-shell-get-config buffer) :identifier)))

(defun my/agent-shell-docker-command-prefix (buffer)
  "Function to use for `agent-shell-command-prefix'.

When BUFFER belongs to a container project, return (\"docker\" \"exec\" \"-i\" ...
CONTAINER); otherwise return nil (run locally)."
  (unless my/agent-shell-docker-force-local
    (when-let* ((container (my/agent-shell-docker-container-for buffer))
                (identifier (my/agent-shell-docker--agent-identifier buffer))
                ((memq identifier my/agent-shell-docker-identifiers)))
      (let ((dir (my/agent-shell-docker--truedir
                  (buffer-local-value 'default-directory buffer))))
        (my/agent-shell-docker--ensure-container container)
        (when (and my/agent-shell-docker-verify-mounted
                   dir
                   (not (my/agent-shell-docker--dir-visible-p
                         container (directory-file-name dir))))
          (user-error
           "agent-shell: directory %s is not mounted into container %s, \
so an agent running in the container cannot see the code.  Check the \
container mounts, or remove this project from `my/agent-shell-docker-projects'"
           (directory-file-name dir) container))
        (append (list "docker" "exec" "-i")
                my/agent-shell-docker-extra-args
                (list container))))))

(defun my/agent-shell-docker-toggle-force-local ()
  "Temporarily toggle everything between local and container execution.
Only newly opened shells are affected."
  (interactive)
  (setq my/agent-shell-docker-force-local
        (not my/agent-shell-docker-force-local))
  (message "agent-shell: newly opened shells will now %s"
           (if my/agent-shell-docker-force-local
               "run everything locally"
             "pick the environment according to `my/agent-shell-docker-projects'")))

;;; Diagnostics

(defun my/agent-shell-docker-describe ()
  "Report which execution environment the current buffer will use."
  (interactive)
  (let* ((container (my/agent-shell-docker-container-for (current-buffer)))
         (dir (buffer-local-value 'default-directory (current-buffer))))
    (message "%s -> %s"
             (abbreviate-file-name dir)
             (if container
                 (format "run in container %s (%s)"
                         container
                         (if (my/agent-shell-docker--inspect container "{{.State.Running}}")
                             "running" "stopped/unknown"))
               "run locally"))))

;;; Wiring

(setq agent-shell-command-prefix #'my/agent-shell-docker-command-prefix)

(provide 'init-agent-shell-docker)
;;; init-agent-shell-docker.el ends here
