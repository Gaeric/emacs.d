;; init-utils.el --- Elisp utils and function -*- coding: utf-8; lexical-binding: t; -*-
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3


;; -------------------------------------------------------------------
;; 加载位于site-lisp下的本地的软件包，pkg以子目录的形式存放于site-lisp目录下
;; 目录名与文件名相同
;; -------------------------------------------------------------------
(defmacro local-require (pkg)
  `(unless (featurep ,pkg)
     (load (expand-file-name
            (cond
             (t
              (format "~/.emacs.d/site-lisp/%s/%s" ,pkg ,pkg))))
           t t)))

;; -------------------------------------------------------------------
;; Handler way to add modes to auto-mode-alist
;; -------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Ad entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;; -------------------------------------------------------------------
;; String utilities missing from core emacs
;; -------------------------------------------------------------------
(defun sanityinc/string-all-matchs (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))


;; -------------------------------------------------------------------
;; Delete the current file
;; -------------------------------------------------------------------
(defun delete-this-file-and-buffer ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited."))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;; -------------------------------------------------------------------
;; Rename the current file
;; -------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Rename both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))


;; -------------------------------------------------------------------
;; Browse current HTML file
;; -------------------------------------------------------------------
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

;; -------------------------------------------------------------------
;; swith to some special buffer
;; -------------------------------------------------------------------
(defun switch-to-scratch-buffer ()
  "Switch to the `*scratch*' buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

(defun switch-to-message-buffer ()
  "Switch to the `*Message*' buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Messages*")))

(defun open-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun gaeric/symbol-at-reg-point ()
  (if (region-active-p)
      (unwind-protect
          (buffer-substring-no-properties (region-beginning) (region-end))
        (pop-mark))
    (thing-at-point 'symbol)))

(provide 'init-utils)
