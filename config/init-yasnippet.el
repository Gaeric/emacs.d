;; -*- coding: utf-8 lexical-binding: t -*-

(require 'yasnippet)


(dolist (hook
         (list
          'python-mode-hook
          'rust-mode-hook
          'text-mode-hook
          'lisp-interaction-mode-hook
          'emacs-lisp-mode-hook
          'web-mode))
  (add-hook hook #'yas-minor-mode))


(with-eval-after-load 'yasnippet
  (yas-reload-all))


(provide 'init-yasnippet)


