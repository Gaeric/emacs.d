;; init-rust.el -*- coding: utf-8; lexical-binding: t; -*-
;; The Rust Program Language
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3


(autoload 'rust-mode "rust-mode" "\
Major mode for Rust code.

\\{rust-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; use eglot for auto-complete and browser
(with-eval-after-load 'rust-mode
  (eglot-ensure))


(provide 'init-rust)
