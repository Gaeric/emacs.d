;; init-minibuffer.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(when (maybe-require-package 'amx)
  (setq-default amx-save-file (expand-file-name ".amx-items" user-emacs-directory)))

;; vertico {
(when (maybe-require-package 'vertico)
  (add-hook 'after-init-hook 'vertico-mode)

  (require-package 'orderless)
  ;; (advice-add 'orderless-regexp :around #'lim-orderless-regexp)
  (with-eval-after-load 'vertico
    (amx-mode 1)
    (require 'orderless)
    (setq completion-styles '(orderless basic)
          completion-category-defaults nil
          completion-category-overrides nil)
    (define-key vertico-map (kbd "M-j") 'vertico-next)
    (define-key vertico-map (kbd "M-k") 'vertico-previous)))
  
(when (maybe-require-package 'embark)
  (with-eval-after-load 'vertico
    (define-key vertico-map (kbd "M-.") 'embark-act))


  (with-eval-after-load 'embark
    (defun embark-consult-goto-grep-other-window (location)
      "Go to LOCATION, which should be a string with a grep match."
      (let ((pos (car (consult--grep-position location))))
        (message "pos is %s" pos)
        (if (and (markerp pos) (marker-buffer pos))
            (progn
              (message "other-windows")
              (push-mark (point) t)
              (switch-to-buffer-other-window (current-buffer) 'norecord)))
        (consult--jump pos))
      (pulse-momentary-highlight-one-line (point)))

    (defvar-keymap embark-consult-grep-map
      :doc "embark keymap for consult-grep/ripgrep"
      "o" #'embark-consult-goto-grep-other-window)
    (push '(consult-grep embark-consult-grep-map) embark-keymap-alist)

    ;; (defun gaeric/embark-goto-xref-other-window (xref)
    ;;   (message "call xref-location-embark")
    ;;   (switch-to-buffer-other-window (current-buffer))
    ;;   (xref-pop-to-location xref 'window))

    ;; (defvar-keymap embark-xref-map
    ;;   :doc "embark keymap for xref-location"
    ;;   "RET" #'gaeric/embark-goto-xref-other-window)
    ;; (push '(xref-location embark-xref-map) embark-keymap-alist)
    )
  )
;; }


(when (maybe-require-package 'consult)
  (recentf-mode 1)
  ;; Only preview for consult-line
  ;; https://github.com/minad/consult/issues/186
  (setq consult-preview-key nil)
  (with-eval-after-load 'consult
    (setq consult-line-start-from-top t)
    (consult-customize
     ;; consult-buffer :group nil
     consult-line :preview-key 'any))
  

  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame))

(when (maybe-require-package 'embark-consult)
  (with-eval-after-load 'embark
    (require 'embark-consult)))

(require-package 'wgrep)

(when (maybe-require-package 'marginalia)
  (add-hook 'after-init-hook 'marginalia-mode)
  (setq-default marginalia-annotators '(marginalia-annotators-heavy)))

(when (macrop 'gaeric-space-leader-def)
  (gaeric-space-leader-def
    ;; use consult-ripgrep grep at current-dir
    "cs" 'consult-ripgrep
    "ci" 'consult-imenu
    "ss" 'consult-line))

(when (maybe-require-package 'which-key)
  (with-eval-after-load 'vertico
    (which-key-mode))
  (with-eval-after-load 'which-key
    (diminish 'which-key-mode)))

(provide 'init-minibuffer)
