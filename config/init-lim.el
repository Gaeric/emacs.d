;;; init-lim.el --- lim input method -*- coding: utf-8 lexical-binding: t -*-
;; Author:  Gaeric
;; URL: https://github.com/Gaeric/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Load lim-xixi
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/lim/"))

(load-file "~/.emacs.d/site-lisp/lim/lim-sbfd.el")

(when (macrop 'gaeric-comma-leader-def)
  (gaeric-comma-leader-def
    "wc" 'lim-count-words))

(provide 'init-lim)
