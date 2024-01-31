;; init-lua.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;; use lua-ts-mode

;; @see https://git.sr.ht/~johnmuhl/lua-ts-mode
;; M-x treesit-install-language-grammar RET lua RET y
;; https://github.com/MunifTanjim/tree-sitter-lua RET RET RET RET RET RET
;; M-x package-vc-install RET https://git.sr.ht/~johnmuhl/lua-ts-mode

(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(lua-ts-mode . ("lua-language-server"))))

(provide 'init-lua)
