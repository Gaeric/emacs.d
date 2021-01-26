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
  (add-hook 'after-init-hook 'selectrum-mode))

(when (maybe-require-package 'selectrum-prescient)
  (require 'prescient)
  (prescient-persist-mode 1)
  (selectrum-prescient-mode 1))

(when (maybe-require-package 'embark)
  ;; use C-h to show help after embark-act-noexit
  (define-key selectrum-minibuffer-map (kbd "C-c C-o") 'embark-export)
  (define-key selectrum-minibuffer-map (kbd "C-c C-c") 'embark-act-noexit))

(when (maybe-require-package 'consult)
  (setq consult-preview-buffer nil)
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame))

(when (maybe-require-package 'embark-consult)
  (with-eval-after-load 'embark
    (require 'embark-consult)))

(when (maybe-require-package 'marginalia)
  (add-hook 'after-init-hook 'marginalia-mode)
  (setq-default marginalia-annotators '(marginalia-annotators-heavy)))

(when (macrop 'gaeric-space-leader-def)
  (gaeric-space-leader-def
    "ss" 'consult-line))

(provide 'init-complete)