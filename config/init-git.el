;; init-git.el --- Git SCM config -*- coding: utf-8; lexical-binding: t; -*-
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;; Don't use git-blamed because magit provide it

;; git-modes for gitconfig, gitattributes and gitignore
(load "~/.emacs.d/site-lisp/git-modes/git-modes-autoloads.el")

;; git-timemachine need transient
(load "~/.emacs.d/site-lisp/transient/lisp/transient-autoloads.el")
(require 'git-timemachine)

;; magit require with-edtior, libgit, transient and dash
;; TODO: magit startup slowly
(load "~/.emacs.d/site-lisp/magit/lisp/magit-autoloads.el")

(setq-default magit-diff-refine-hunk t) ;show fine diff for current diff hunk only.
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)


;; TODO: magit work with vc
;; (defun sanityinc/magit-or-vc-log-file (&optional prompt)
;;   (inteactive "P")
;;   (if (and (buffer-file-name)
;;            (eq 'Git (vc-backend (buffer-file-name))))
;;       (if prompt
;;           (magit-log-buffer-file-popup)
;;         (magit-log-buffer-file t))
;;     (vc-print-log)))

;; (with-eval-after-load 'vc
;;   (define-key vc-prefix-map (kbd "l") 'sanityinc/magit-or-vc-log-file))

(with-eval-after-load 'magit
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up))

;; magit-todos
(local-require 'magit-todos)
(add-hook 'after-init-hook
          (lambda ()
            (global-hl-todo-mode)
            (magit-todos-mode 1)))

;; TODO:magit-todos ignore special dir

;; fullframe 
(local-require 'fullframe)
(with-eval-after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

;; (with-eval-after-load 'vc
;;   (define-key vc-prefix-map (kbd "f") 'vc-git-grep))

(provide 'init-git)
