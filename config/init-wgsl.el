;; init-wgsl.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(require-package 'wgsl-mode)

(with-eval-after-load 'wgsl-mode
  (setq c-basic-offset 4))

(setq wesl-wgsl-tsauto-config
      (make-treesit-auto-recipe
       :lang 'wesl
       :ts-mode 'wesl-ts-mode
       :remap '(wesl-mode wgsl-mode)
       :url "https://github.com/wgsl-tooling-wg/tree-sitter-wesl"
       :revision "main"
       :source-dir "src"
       :ext "\\.wgsl\\'"))

(add-to-list 'treesit-auto-recipe-list wesl-wgsl-tsauto-config)
(add-to-list 'treesit-auto-langs 'wesl)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(wgsl-mode . ("wgsl-analyzer"))))

(provide 'init-wgsl)
