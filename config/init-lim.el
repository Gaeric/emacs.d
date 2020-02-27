;;; init-lim.el --- lim input method -*- coding: utf-8 lexical-binding: t -*-
;; Author:  Gaeric
;; URL: https://github.com/Gaeric/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Load lim-xixi
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/lim/"))

(autoload 'lim-use-package "lim-xixi" "Lightly input mehtod xixi")

(register-input-method
 "lim-xixi" "euc-cn" 'lim-use-package
 "淅淅" "淅淅顶功输入法" "lim-xixi.txt")
(setq default-input-method "lim-xixi")

(defun lim-active-xixi ()
  (setq lim-punctuation-list (lim-read-punctuation lim-current-scheme))
  (setq lim-translate-function 'lim-punctuation-translate)
  (lim-evil-find-mode))

;; 载入输入法时即加载标点相关控制函数，保证不受影响
(setq lim-load-hook 'lim-active-xixi)

(gaeric-comma-leader-def
  "wc" 'lim-count-words)

(provide 'init-lim)
