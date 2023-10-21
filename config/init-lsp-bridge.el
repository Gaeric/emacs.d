;; init-lsp-bridge.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;; (add-to-list 'load-path "~/prog/lsp-bridge")

(require 'lsp-bridge)
(global-lsp-bridge-mode)

(define-key acm-mode-map (kbd "M-j") 'acm-select-next)
(define-key acm-mode-map (kbd "M-k") 'acm-select-prev)
(define-key acm-mode-map (kbd "M-n") 'acm-doc-scroll-up)
(define-key acm-mode-map (kbd "M-p") 'acm-doc-scroll-down)

(dolist (hook
         (list
          'c-ts-mode-hook
          'python-ts-mode-hook
          'rust-ts-mode-hook
          ))
  (add-hook hook #'lsp-bridge-mode))


(when (macrop 'gaeric-comma-leader-def)
  (gaeric-comma-leader-def
    :keymaps lsp-bridge-mode-map
    "en" 'lsp-bridge-diagnostic-jump-next
    "ep" 'lsp-bridge-diagnostic-jump-prev
    "gd" 'lsp-bridge-find-def
    "gr" 'lsp-bridge-find-references
    "go" 'lsp-bridge-find-def-other-window))


(provide 'init-lsp-bridge)
