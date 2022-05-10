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

;; disable some feature such as highlight symbol
;; @see https://github.com/joaotavora/eglot/issues/334
(require-package 'eglot)

(with-eval-after-load 'eglot
  ;; @see https://github.com/joaotavora/eglot/issues/514
  (setq eldoc-area-use-multiline-p 3) ;; you can experiment with other values, and also `nil
  (setq eldoc-prefer-doc-buffer t)
  (setq eglot-autoshutdown t)
  (setq eglot-ignored-server-capabilites '(:documentHighlightProvider)))

(when (macrop 'gaeric-comma-leader-def)
  (gaeric-comma-leader-def
    "en"  'prog-next-error
    "ep"  'prog-prev-error))

(maybe-require-package 'tree-sitter)
(maybe-require-package 'tree-sitter-langs)

;; todo: lsp-bridge 

(dolist (hook
         '(rust-mode-hook
           css-mode-hook
           web-mode-hook
           js-mode-hook
           c-mode-common-hook
           python-mode-hook))

  (add-hook hook #'tree-sitter-mode))

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(setq tsc-dyn-get-from '(:compilation))

(provide 'init-langs)
