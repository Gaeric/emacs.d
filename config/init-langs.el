;; init-langs.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3


(defvar emacs-lsp-package 'native
  "lsp-bridge/native/lsp-mode/lspec")

(unless (display-graphic-p)
  (if (eq emacs-lsp-package 'lsp-bridge)
      (setq emacs-lsp-package 'native)))

(setq lsp-manage-mode
      (list
       'emacs-lisp-mode-hook
       'c-ts-mode-hook
       'c++-ts-mode-hook
       'python-ts-mode-hook
       'rust-ts-mode-hook
       'rustic-mode-hook
       'typescript-ts-mode-hook
       'tsx-ts-mode-hook
       'lua-ts-mode-hook
       ))

(when (eq emacs-lsp-package #'native)
  ;; eglot
  ;; https://github.com/joaotavora/eglot/issues/369

  ;; --- eglot config start
  ;; disable some feature such as highlight symbol
  ;; @see https://github.com/joaotavora/eglot/issues/334
  (require-package 'eglot)
  (require-package 'consult-eglot)

  (setq eglot-events-buffer-size 0)
  (setq read-process-output-max (* 1024 1024))
  (setq eglot-autoshutdown t)
  (setq eglot-ignored-server-capabilities
        '(:documentHighlightProvider :inlayHintProvider))
  ;; --- eglot config finish

  (dolist (hook lsp-manage-mode)
    (unless (eq hook 'emacs-lisp-mode-hook)
      (add-hook hook #'eglot-ensure))))
  

(when (eq emacs-lsp-package #'lspce)
  (load-rs-module "~/.emacs.d/site-lisp/lspce/target/release/liblspce_module.so")
  (require 'lspce)
  (setq lspce-server-programs `(("rust"  "rust-analyzer" "")
                                ("rustic"  "rust-analyzer" "")
                                ("python" "pylsp" "" )
                                ("python" "pyright-langserver" "--stdio")
                                ("C" "clangd" "")
                                ("java" ,lspce-java-path lspce-jdtls-cmd-args)
                                ("sh" "bash-language-server" "start")
                                ("go" "gopls" "")
                                ("typescript" "typescript-language-server" "--stdio")
                                ("js" "typescript-language-server" "--stdio")))
  (dolist (hook lsp-manage-mode)
    (unless (eq hook 'emacs-lisp-mode-hook)
      (add-hook hook #'lspce-mode))))

(when (eq emacs-lsp-package 'lsp-bridge)
  ;; (add-to-list 'load-path "~/prog/lsp-bridge")
  (require 'lsp-bridge)
  (setq-default lsp-bridge-enable-inlay-hint nil)
  (add-hook 'after-init-hook (lambda () (global-lsp-bridge-mode))))

(if (eq emacs-lsp-package 'lsp-bridge)
    (progn
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
          "gr" 'lsp-bridge-find-references
          "go" 'lsp-bridge-find-def-other-window)))

  (dolist (hook lsp-manage-mode)
    (add-hook hook #'yas-minor-mode)
    (add-hook hook #'corfu-mode))
  (when (macrop 'gaeric-comma-leader-def)
    (gaeric-comma-leader-def
      "en" 'prog-next-error
      "ep" 'prog-prev-error
      "ef" 'eglot-format
      "gd" 'xref-find-definitions
      "gr" 'xref-find-references
      "go" 'xref-find-definitions-other-window)))

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
  (setq xref-show-xrefs-function 'consult-xref))

(setq treesit-extra-load-path '("~/.emacs.d/site-lisp/tree-sitter-module/dist/"))
(when (maybe-require-package 'treesit-auto)
  (require 'treesit-auto)
  (global-treesit-auto-mode))

(provide 'init-langs)
