;; init-pair.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(local-require 'awesome-pair)

(add-hook 'prog-mode-hook '(lambda () (awesome-pair-mode 1)))
(define-key awesome-pair-mode-map (kbd "M-n") 'awesome-pair-jump-right)
(define-key awesome-pair-mode-map (kbd "M-p") 'awesome-pair-jump-left)

(provide 'init-pair)
