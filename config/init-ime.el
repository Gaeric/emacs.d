;; init-lim.el --- input method config -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;; (load-file "~/.emacs.d/site-lisp/lim/lim-sbfd.el")

(when (macrop 'gaeric-comma-leader-def)
  (gaeric-comma-leader-def
    "wc" 'lim-count-words))

(when (maybe-require-package 'rime)
  ;; origin key-binding tab-to-tab-stop
  (global-set-key (kbd "M-i") 'toggle-input-method)
  (setq default-input-method "rime"))

(provide 'init-ime)
