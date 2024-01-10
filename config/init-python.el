;; init-python.el -*- coding: utf-8; lexical-binding: t; -*-
;; Config Python Language with pyvenv and lsp
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;; use pyvenv for virtual python env
(require-package 'pyvenv)

(defun gaeric-virtualenv-setup ()
  "用于激活虚拟环境，pyvnenv-workon由.dir-locals控制"
  (setq-default fill-column 80)
  (setq-default python-indent-offset 4)
  (auto-fill-mode)
  (pyvenv-mode))

(add-hook 'python-mode-hook 'gaeric-virtualenv-setup)

(provide 'init-python)
