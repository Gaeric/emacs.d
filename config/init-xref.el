;; init-xref.el -*- coding: utf-8; lexical-binding: t; -*-
;; config xref for code navigation
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(with-eval-after-load 'xref
  (local-require 'ivy-xref))

(setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
(setq xref-show-definitions-function #'ivy-xref-show-defs)

(provide 'init-xref)
