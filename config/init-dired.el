;; init-dired.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3



;;----------------------------------------------------------------------------
;; make dired use the same buffer for viewing directory
;; http://ergoemacs.org/emacs/emacs_dired_tips.html
;;----------------------------------------------------------------------------
(with-eval-after-load 'dired
  (setq dired-kill-when-opening-new-dired-buffer t)
  ;; was dired-advertised-find-file
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  ;; was dired-up-directory
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))))

(when (maybe-require-package 'dirvish)
  (add-hook 'after-init-hook (lambda () (dirvish-override-dired-mode))))


(provide 'init-dired)
