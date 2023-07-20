;; init-cc.el -*- coding: utf-8; lexical-binding: t; -*-
;; configuration for c && cpp mode
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3
(defun gaeric-common-cc-mode-setup ()
  "setup shared by all languages (java/groovy/c++ ...)"
  (setq-default c-basic-offset 4)
  (setq-default c-ts-mode-indent-offset 4)
  ;; give me NO newline automatically after electric expressions are entered
  (setq c-auto-newline nil)

  ;make DEL take all previous whitespace with it
  (c-toggle-hungry-state 1)
  (eglot-ensure))

(add-hook 'c-mode-common-hook 'gaeric-common-cc-mode-setup)
(add-hook 'c-ts-mode-hook 'gaeric-common-cc-mode-setup)

(provide 'init-cc)
