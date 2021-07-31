;; init-rust.el -*- coding: utf-8; lexical-binding: t; -*-
;; The Rust Program Language
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(require-package 'rust-mode)

(autoload 'rust-mode "rust-mode" "\
Major mode for Rust code.

\\{rust-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; use eglot for auto-complete and browser
(add-hook 'rust-mode-hook
          (lambda ()
            (setq rust-indent-offset 4)
            (setq tab-width 4)
            (eglot-ensure)))

(provide 'init-rust)
