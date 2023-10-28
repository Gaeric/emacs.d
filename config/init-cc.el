;; init-cc.el -*- coding: utf-8; lexical-binding: t; -*-
;; configuration for c && cpp mode
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(require 'citre)
(require 'citre-config)

(defun setup-citre-config ()
  ;; ubuntu workaround
  (setq citre-ctags-program "ctags-universal"
        citre-auto-enable-citre-mode-modes '(c-ts-mode)))

(defun gaeric-common-cc-mode-setup ()
  "setup shared by all languages (java/groovy/c++ ...)"
  (setq-default c-basic-offset 4)
  ;; give me NO newline automatically after electric expressions are entered
  (setq c-auto-newline nil)
  (yas-minor-mode)
  ;make DEL take all previous whitespace with it
  (c-toggle-hungry-state 1)
  (setup-citre-config)
  (setq lsp-bridge-find-def-fallback-function #'citre-jump)
  (setq lsp-bridge-find-ref-fallback-function #'citre-jump-to-reference))

(defun gaeric/c-ts-mode-setup()
  (setq-default c-ts-mode-indent-offset 4)
  (setup-citre-config))

(require-package 'clang-format)

(when (and (eq emacs-lsp-package 'native) (macrop 'gaeric-comma-leader-def))
  (gaeric-comma-leader-def
    :keymaps 'citre-mode-map
    "gr" 'citre-jump-to-reference))

(add-hook 'c-mode-common-hook 'gaeric-common-cc-mode-setup)
(add-hook 'c-ts-mode-hook 'gaeric/c-ts-mode-setup)

(provide 'init-cc)
