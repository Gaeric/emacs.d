;; init-complete.el -*- coding: utf-8; lexical-binding: t; -*-
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
  (with-eval-after-load 'vertico
    (amx-mode 1)
    (define-key vertico-map (kbd "M-j") 'vertico-next)
    (define-key vertico-map (kbd "M-k") 'vertico-previous))

  (setq completion-styles '(basic partial-completion orderless))

  (when (maybe-require-package 'embark)
    (with-eval-after-load 'vertico
      (define-key vertico-map (kbd "C-c C-o") 'embark-export)
      (define-key vertico-map (kbd "C-c C-c") 'embark-act))))
;; }

(when (maybe-require-package 'consult)
  (recentf-mode 1)
  ;; Only preview for consult-line
  ;; https://github.com/minad/consult/issues/186
  (setq consult-preview-key nil)
  (setq consult-config `((consult-line :preview-key any)
                         (consult-buffer :title nil)
                         (consult-buffer-other-window :title nil)))

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
    "ss" 'consult-line))

(when (maybe-require-package 'which-key)
  (with-eval-after-load 'which-key
    (diminish 'which-key-mode)))

(provide 'init-complete)
