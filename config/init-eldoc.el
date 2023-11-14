;; init-eldoc.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(when (maybe-require-package 'eldoc-box)
  (when (display-graphic-p)
    (add-hook 'prog-mode-hook #'eldoc-mode)
    (add-hook 'eldoc-mode-hook #'eldoc-box-hover-mode)
    (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)))

(provide 'init-eldoc)
