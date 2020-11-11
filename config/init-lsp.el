;; init-lsp.el -*- coding: utf-8; lexical-binding: t; -*-
;; Config for Emacs lsp client
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;; lsp-mode
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

(gaeric-comma-leader-def
  "en"  'prog-next-error
  "ep"  'prog-prev-error)

(provide 'init-lsp)
