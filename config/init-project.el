;; init-project.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(defun gaeric/project-find-regexp ()
  (interactive)
  (cond ((functionp 'consult-ripgrep)
         (let ((project (when-let (project (project-current))
                          (car (project-roots project))))
               (symbol (gaeric/symbol-at-reg-point)))
           (consult-ripgrep project symbol)))
        ((functionp 'counsel-rg)
         (counsel-rg (gaeric/symbol-at-reg-point)))
        (t
         (project-find-regexp))))

(defun gaeric/directory-find-regexp ()
  (interactive)
  (cond ((functionp 'consult-ripgrep)
         (consult-ripgrep default-directory (gaeric/symbol-at-reg-point)))
        (t
         (message "No function"))))

(when (macrop 'gaeric-comma-leader-def)
  (gaeric-comma-leader-def
    "gf" 'project-find-file
    "gg" 'project-find-regexp
    "gx" 'gaeric/directory-find-regexp
    "gs" 'gaeric/project-find-regexp))

(provide 'init-project)
