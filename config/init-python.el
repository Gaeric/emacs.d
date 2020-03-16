;; init-python.el -*- coding: utf-8; lexical-binding: t; -*-
;; Config Python Language with pyvenv and lsp
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;; use pyvenv for virtual python env

(local-require 'pyvenv)


(defun gaeric-virtualenv-setup ()
  "用于激活虚拟环境，pyvnenv-workon由.dir-locals控制"
  (pyvenv-mode)
  (eglot-ensure))

(add-hook 'python-mode-hook 'gaeric-virtualenv-setup)


(provide 'init-python)
