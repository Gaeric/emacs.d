;; init-wgsl.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(require-package 'wgsl-mode)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(wgsl-mode . ("wgsl-analyzer"))))

(provide 'init-wgsl)
