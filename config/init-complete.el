;; init-complete.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(when (maybe-require-package 'selectrum)
  (with-eval-after-load 'selectrum
    (recentf-mode)
    (define-key selectrum-minibuffer-map (kbd "M-j") 'selectrum-next-candidate)
    (define-key selectrum-minibuffer-map (kbd "M-k") 'selectrum-previous-candidate))
  (add-hook 'emacs-startup-hook 'selectrum-mode))

(when (maybe-require-package 'selectrum-prescient)
  (with-eval-after-load 'selectrum
    (require 'prescient)
    (prescient-persist-mode 1)
    (selectrum-prescient-mode 1)))

(when (maybe-require-package 'embark)
  ;; use C-h to show help after embark-act-noexit
  (with-eval-after-load 'selectrum
    (define-key selectrum-minibuffer-map (kbd "C-c C-o") 'embark-export)
    (define-key selectrum-minibuffer-map (kbd "C-c C-c") 'embark-act-noexit)))

(when (maybe-require-package 'consult)
  ;; Only preview for consult-line
  (setq consult-preview-key nil)
  (setq consult-config `((consult-line :preview-key any)))

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
    "ss" 'consult-line))

(when (maybe-require-package 'which-key)
  (with-eval-after-load 'which-key
    (diminish 'which-key-mode)))

(provide 'init-complete)
