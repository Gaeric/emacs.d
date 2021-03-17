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
  (cond ((functionp 'counsel-rg)
         (counsel-rg (gaeric/symbol-at-reg-point)))
        ((functionp 'consult-ripgrep)
         (let ((project (when-let (project (project-current))
                          (car (project-roots project))))
               (symbol (gaeric/symbol-at-reg-point)))
           (consult-ripgrep project symbol)))
        (t
         (project-find-regexp))))

(when (macrop 'gaeric-comma-leader-def)
  (gaeric-comma-leader-def
    "gf" 'project-find-file
    "gs" 'gaeric/project-find-regexp))

(provide 'init-project)
