;; init-xref.el -*- coding: utf-8; lexical-binding: t; -*-
;; config xref for code navigation
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(local-require 'ivy-xref)

(setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
(setq xref-show-definitions-function #'ivy-xref-show-defs)

(gaeric-comma-leader-def
  "gd" 'xref-find-definitions
  "gr" 'xref-find-references)

(provide 'init-xref)
