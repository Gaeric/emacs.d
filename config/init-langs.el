;; init-langs.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3


(defvar emacs-lsp-package 'lsp-bridge
  "lsp-bridge/native/lsp-mode")

(when (eq emacs-lsp-package #'eglot)
  ;; eglot
  ;; https://github.com/joaotavora/eglot/issues/369

  ;; --- eglot config start
  ;; disable some feature such as highlight symbol
  ;; @see https://github.com/joaotavora/eglot/issues/334
  (require-package 'eglot)

  (setq read-process-output-max (* 1024 1024))

  (when (maybe-require-package 'eglot)
    (setq eglot-autoshutdown t)
    (setq eglot-ignored-server-capabilities
          '(:documentHighlightProvider
            :inlayHintProvider))
    (maybe-require-package 'consult-eglot)

    (dolist (hook
             (list
              'c-ts-mode-hook
              'python-ts-mode-hook
              'rust-ts-mode-hook))
      ;; (add-hook hook #'eglot-ensure)
      (add-hook hook 'yas-minor-mode)))
  ;; --- eglot config finish

  (when (macrop 'gaeric-comma-leader-def)
    (gaeric-comma-leader-def
      "en" 'prog-next-error
      "ep" 'prog-prev-error
      "ef" 'eglot-format
      "gd" 'xref-find-definitions
      ;; "gr" 'xref-find-references
      "go" 'xref-find-definitions-other-window)))


(when (eq emacs-lsp-package 'lsp-bridge)
  ;; (add-to-list 'load-path "~/prog/lsp-bridge")
  (require 'lsp-bridge)

  (add-hook 'after-init-hook (lambda () (global-lsp-bridge-mode)))

  (define-key acm-mode-map (kbd "M-j") 'acm-select-next)
  (define-key acm-mode-map (kbd "M-k") 'acm-select-prev)
  (define-key acm-mode-map (kbd "M-n") 'acm-doc-scroll-up)
  (define-key acm-mode-map (kbd "M-p") 'acm-doc-scroll-down)

  (when (macrop 'gaeric-comma-leader-def)
    (gaeric-comma-leader-def
      "en" 'lsp-bridge-diagnostic-jump-next
      "ep" 'lsp-bridge-diagnostic-jump-prev
      "ef" 'lsp-bridge-code-format
      "gd" 'lsp-bridge-find-def
      ;; "gr" 'lsp-bridge-find-references
      "go" 'lsp-bridge-find-def-other-window)))


;; breadcrumb config --- 
(require-package 'breadcrumb)
(add-hook 'after-init-hook
          (lambda ()
            (breadcrumb-mode)))

;; xref config --- 
(unless  (version< "28.0" emacs-version)
  (require-package 'xref))

(with-eval-after-load 'xref
  (setq xref-search-program 'ripgrep)
  ;; (setq xref-show-definitions-function 'xref-show-definitions-completing-read)
  ;; (setq xref-show-xrefs-function 'xref-show-definitions-completing-read)

  (setq xref-show-definitions-function 'consult-xref)
  (setq xref-show-xrefs-function 'consult-xref)
  )

(when (macrop 'gaeric-comma-leader-def)
  (gaeric-comma-leader-def
    "en" 'prog-next-error
    "ep" 'prog-prev-error
    "ef"  'eglot-format
    "gd" 'xref-find-definitions
    "gr" 'xref-find-references
    "go" 'xref-find-definitions-other-window))

;; --------- eglot config finish

(require 'breadcrumb)

(add-hook 'after-init-hook
          (lambda ()
            (breadcrumb-mode)))

(setq treesit-extra-load-path '("~/.emacs.d/site-lisp/tree-sitter-module/dist/"))

(when (maybe-require-package 'treesit-auto)
  (require 'treesit-auto)
  (global-treesit-auto-mode))

;; (maybe-require-package 'markdown-mode)

;; (require 'lsp-bridge)
;; (define-key acm-mode-map (kbd "M-j") 'acm-select-next)
;; (define-key acm-mode-map (kbd "M-k") 'acm-select-prev)
;; (define-key acm-mode-map (kbd "M-n") 'acm-doc-scroll-down)
;; (define-key acm-mode-map (kbd "M-p") 'acm-doc-scroll-up)

;; (dolist (hook
;;          (list
;;           'c-ts-mode-hook
;;           'python-ts-mode-hook
;;           'rust-ts-mode-hook
;;           'rustic-mode
;;           ))
;;   (add-hook hook #'lsp-bridge-mode))

;; (when (macrop 'gaeric-comma-leader-def)
;;   (gaeric-comma-leader-def
;;     :keymaps lsp-bridge-mode-map
;;     "en" 'lsp-bridge-diagnostic-jump-next
;;     "ep" 'lsp-bridge-diagnostic-jump-prev
;;     "gd" 'lsp-bridge-find-def
;;     "gr" 'lsp-bridge-find-references
;;     "go" 'lsp-bridge-find-def-other-window))

(provide 'init-langs)
