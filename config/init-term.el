;; init-term.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(maybe-require-package 'vterm)

(when (macrop 'gaeric-space-leader-def)
  (gaeric-space-leader-def
    "ev" 'vterm-other-window
    )
  )

(provide 'init-term)
