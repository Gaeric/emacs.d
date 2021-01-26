;; init-diminish.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(when (maybe-require-package 'diminish)
  (with-eval-after-load 'auto-revert-mode
    (diminish 'auto-revert-mode))

  (with-eval-after-load 'prettier-js
    (diminish 'prettier-js-mode))

  (with-eval-after-load 'yasnippet
    (diminish 'yas-minor-mode))

  (with-eval-after-load 'org-roam
    (diminish 'org-roam-mode)))

(provide 'init-diminish)

