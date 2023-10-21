;; init-rust.el -*- coding: utf-8; lexical-binding: t; -*-
;; The Rust Program Language
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(when (maybe-require-package 'rustic)
  ;; (setq rustic-lsp-setup-p nil)
  (setq rustic-lsp-client 'eglot)
  (remove-hook 'rustic-mode-hook 'flycheck-mode))

;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

(provide 'init-rust)
