;; init-toml.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3


(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(toml-ts-mode . ("taplo" "lsp" "stdio"))))

(provide 'init-toml)
