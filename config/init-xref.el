;; init-xref.el -*- coding: utf-8; lexical-binding: t; -*-
;; config xref for code navigation
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(require 'ivy-xref)

(with-eval-after-load 'xref
  (setq xref-search-program 'ripgrep)
  (require 'swiper)
  (setq xref-show-definitions-function 'ivy-xref-show-defs)
  (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))

(when (macrop 'gaeric-comma-leader-def)
  (gaeric-comma-leader-def
    "gd" 'xref-find-definitions
    "gr" 'xref-find-references
    "go" 'xref-find-definitions-other-window))

(provide 'init-xref)
