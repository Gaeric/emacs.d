;; init-clipboard.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;; Use the system clipboard
;; @see https://www.emacswiki.org/emacs/CopyAndPaste
;; So `C-y' could paste from clipboard if you are NOT using emacs-nox
(setq select-enable-clipboard t)
(unless (eq system-type 'window-nt)
  (setq select-enable-primary t))
;; kill-ring and clipboard are same? No, it's annoying!
(setq save-interprogram-paste-before-kill nil)


(require-package 'xclip)
(autoload 'xclip-get-selection "xclip" "" t)
(autoload 'xclip-set-selection "xclip" "" t)

(defun xclip-gclip ()
  "Get clipboard content."
  (let* ((powershell-program (executable-find "powershell.exe")))
    (cond
     ;; win
     
     (powershell-program
      (string-trim-right
       (with-output-to-string
         (with-current-buffer standard-output
           (call-process powershell-program nil t nil "-command" "Get-Clipboard")))))
     (t
      (xclip-get-selection 'clipboard)))))

(defun xclip-pclip (str-val)
  "Put STR-VAL into clipboard."
  (let* ((win64-clip-program (executable-find "clip.exe"))
         ssh-client)
    (cond
     ;; win7 & win10
     ((and win64-clip-program)
      (with-temp-buffer
        (insert str-val)
        (call-process-region (point-min) (point-max) win64-clip-program)))

     (t
       (xclip-set-selection 'clipboard str-val)))))


(defun copy-yank-str (msg &optional clipboard-only)
  (unless clipboard-only (kill-new msg))
  (xclip-pclip msg)
  msg)


(defun cp-filename-of-current-buffer (&optional n)
  "Copy file name (NOT full path) into the yank ring and OS clipboard.
If N is not nil, copy file name and line number."
  (interactive "P")
  (when buffer-file-name
    (let* ((filename (file-name-nondirectory buffer-file-name))
           (s (if n (format "%s:%s" filename (line-number-at-pos)) filename)))
      (copy-yank-str s)
      (message "%s => clipboard&kill-ring" s))))


(defun cp-full-path-of-current-buffer ()
  "Copy full path into the yank ring and OS clipboard"
  (interactive)
  (when buffer-file-name
    (copy-yank-str (file-truename buffer-file-name))
    (message "file full path => clipboard & yank ring")))


(defun clipboard-to-kill-ring ()
  "Copy from clipboard to `kill-ring'"
  (interactive)
  (let* ((warning-minimum-level :emergency))
    (kill-new (xclip-gclip)))
  (message "clipboard => kill-ring"))


(defun kill-ring-to-clipboard ()
  "Copy form `kill-ring' to clipboard."
  (interactive)
  (let ((msg (read-from-kill-ring))
        (hint " => clipboard"))
    ;; cc actual string
    (xclip-pclip msg)
    ;; echo
    (message "%s%s" msg hint)))


(defun copy-to-x-clipboard (&optional num)
  "num control how to copy the string.
1 for down-cased.
2 for capitalized.
3 for up-cased.
4 for indent 4 space."
  (interactive "P")
  (let* ((thing (gaeric/symbol-at-reg-point)))
    (if (region-active-p) (deactivate-mark))
    (cond
     ((not num))
     ((= num 1)
      (setq thing (downcase thing)))
     ((= num 2)
      (setq thing (capitalize thing)))
     ((= num 3)
      (setq thing (upcase thing)))
     ((= num 4)
      (setq thing
            (string-trim-right
             (concat "    "
                     (mapconcat 'identity
                                (split-string thing "\n") "\n    ")))))
     (t
      (message "C-h f `copy-to-x-clipboard' to find right usage")))

    (xclip-pclip thing)
    (if (not (and num (= 4 num)))
        (message "kill-ring => clipboard")
      (message "thing => clipboard!"))))


(defun paste-from-x-clipboard (&optional n)
  "Remove selected text and paste string clipboard.
If N is 1, paste diff hunk whose leading char should be removed.
If N is 2, paste into `kill-ring' too.
If N is 3, converted dashed to camel-cased then paste.
If N is 4, rectangle paste."
  (interactive "P")
  (when (and (functionp 'evil-normal-state-p)
             (functionp 'evil-move-cursor-back)
             (evil-normal-state-p)
             (not (eolp))
             (not (eobp)))
    (forward-char))
  (let* ((str (xclip-gclip))
         (fn 'insert))
    (when (> (length str) (* 256 1024))
      ;; use light weight `major-mode' link `js-mode'
      (when (derived-mode-p 'js2-mode) (js-mode 1))
      ;; turn off syntax highlight
      (font-lock-mode -1))

    ;; past a big string, stop lsp temporarily
    (when  (> (length str) 1024)
      (if (and (boundp 'lsp-mode) lsp-mode)
          (progn
            (lsp-disconnect)
            (run-at-time 300 nil #'lsp-deferred)))
      (if (and (boundp 'eglot) eglot)
          (progn
            (eglot-shutdown)
            (run-at-time 500 nil #'eglot-ensure))))

    (when (region-active-p)
      (delete-region (region-beginning) (region-end)))
    

    ;; paste after the cursor in evil normal state
    (cond
     ((not n)) ; do nothing
     ((= 1 n)
      (setq str (replace-regexp-in-string "^\\(+\\|-\\|@@ $\\)" "" str)))
     ((= 2 n)
      (kill-new str))
     ((= 3 n)
      (setq str (mapconcat (lambda (s) (capitalize s)) (split-string str "-") "")))
     ((= 4 n)
      (setq fn 'insert-rectangle)
      (setq str (split-string str "[\r]?\n"))))
    (funcall fn str)))

(provide 'init-clipboard)
