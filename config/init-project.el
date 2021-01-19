;; init-project.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(when (macrop 'gaeric-comma-leader-def)
  (gaeric-comma-leader-def
    "gf" 'project-find-file
    "gs" 'project-find-regexp))

(provide 'init-project)
