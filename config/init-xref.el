;; init-xref.el -*- coding: utf-8; lexical-binding: t; -*-
;; config xref for code navigation
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(with-eval-after-load 'xref
  (setq xref-search-program 'ripgrep)
  (setq xref-show-definitions-function 'xref-show-definitions-completing-read)
  (setq xref-show-xrefs-function 'xref-show-definitions-completing-read))

(when (macrop 'gaeric-comma-leader-def)
  (gaeric-comma-leader-def
    "gd" 'xref-find-definitions
    "gr" 'xref-find-references
    "go" 'xref-find-definitions-other-window))

(provide 'init-xref)
