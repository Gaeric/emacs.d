;; init-langs.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;; eglot
;; https://github.com/joaotavora/eglot/issues/369

;; --------- eglot config start
;; disable some feature such as highlight symbol
;; @see https://github.com/joaotavora/eglot/issues/334
;; (require-package 'eglot)

;; (setq read-process-output-max (* 1024 1024))

;; (when (maybe-require-package 'eglot)
;;   (maybe-require-package 'consult-eglot))

;; (gaeric-comma-leader-def
;;     "en" 'prog-next-error
;;     "ep" 'prog-prev-error)

;; --------- eglot config finish

(setq treesit-extra-load-path '("~/.emacs.d/site-lisp/tree-sitter-module/dist/"))

(maybe-require-package 'markdown-mode)

(when (maybe-require-package 'treesit-auto)
  (require 'treesit-auto)
  (global-treesit-auto-mode))

(when (maybe-require-package 'posframe)
  (require 'lsp-bridge)
  ;; (global-lsp-bridge-mode)
  (define-key acm-mode-map (kbd "M-j") 'acm-select-next)
  (define-key acm-mode-map (kbd "M-k") 'acm-select-prev)
  (define-key acm-mode-map (kbd "M-n") nil)
  (define-key acm-mode-map (kbd "M-p") nil)

  (dolist (hook
           (list
            'c-ts-mode-hook
            'python-ts-mode-hook
            'rust-ts-mode-hook
            ))
    (add-hook hook #'lsp-bridge-mode)))


(when (macrop 'gaeric-comma-leader-def)
  (gaeric-comma-leader-def
    :keymaps lsp-bridge-mode-map
    "en" 'lsp-bridge-diagnostic-jump-next
    "ep" 'lsp-bridge-diagnostic-jump-prev
    "gd" 'lsp-bridge-find-def
    "gr" 'lsp-bridge-find-references
    "go" 'lsp-bridge-find-def-other-window))

(provide 'init-langs)
