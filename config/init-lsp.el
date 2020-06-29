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
(local-require 'eglot)

(with-eval-after-load 'eglot
  (setq eglot-ignored-server-capabilites '(:documentHighlightProvider)))

(gaeric-comma-leader-def
  "cn"  'flymake-goto-next-error
  "cp"  'flymake-goto-prev-error)

(provide 'init-lsp)
