;; init-langs.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(maybe-require-package 'tree-sitter)
(maybe-require-package 'tree-sitter-langs)


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
