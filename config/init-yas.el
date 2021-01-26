;; init-yas.el --- YASnippet is a template system for Emacs -*- coding: utf-8; lexical-binding: t; -*-
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(require-package 'yasnippet)
(require-package 'yasnippet-snippets)

(dolist (hook
         (list
          'c-mode-common-hook
          'python-mode-hook
          'rust-mode-hook
          'text-mode-hook
          'lisp-interaction-mode-hook
          'emacs-lisp-mode-hook
          'web-mode))
  (add-hook hook #'yas-minor-mode))

(with-eval-after-load 'yasnippet
  (diminish 'yas-minor-mode)
  (yas-reload-all))

(provide 'init-yas)


