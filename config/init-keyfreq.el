;; init-keyfreq.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(require-package 'keyfreq)

(setq keyfreq-file "~/.emacs.d/.emacs.keyfreq")

(add-hook 'after-init-hook 'keyfreq-mode)
(add-hook 'after-init-hook 'keyfreq-autosave-mode)

(provide 'init-keyfreq)
