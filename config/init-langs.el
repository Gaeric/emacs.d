;; init-langs.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(setq eglot-manage-mode
      (list
       ;; 'emacs-lisp-mode-hook
       'c-ts-mode-hook
       'c++-ts-mode-hook
       'python-ts-mode-hook
       'js-ts-mode-hook
       'typescript-ts-mode-hook
       'tsx-ts-mode-hook
       ;; 'lua-ts-mode-hook
       'gdscript-mode-hook
       'gdscript-ts-mode-hook
       ;; 'wgsl-mode-hook
       'bash-ts-mode-hook
       'toml-ts-mode-hook
       'yaml-ts-mode-hook
       'rust-ts-mode-hook
       'rustic-mode-hook
       ))

(setq lsp-manage-mode
      (list
       'csharp-ts-mode-hook
       'csharp-mode-hook))

;; lsp-mode
(require-package 'lsp-mode)

(setq read-process-output-max (* 1024 1024)
      lsp-headerline-breadcrumb-enable nil
      lsp-completion-provider :capf
      lsp-keep-workspace-alive nil
      lsp-eldoc-render-all t
      lsp-log-io nil
      lsp-idle-delay 0.5
      lsp-enable-symbol-highlighting nil
      lsp-enable-on-type-formatting nil)

(dolist (hook lsp-manage-mode)
  (add-hook hook #'yas-minor-mode)
  (add-hook hook #'company-mode)
  (add-hook hook (lambda () (lsp-deferred))))

;; eglot
;; https://github.com/joaotavora/eglot/issues/369
;; --- eglot config start
;; disable some feature such as highlight symbol
;; @see https://github.com/joaotavora/eglot/issues/334
(require-package 'eglot)
(require-package 'consult-eglot)

(setq eglot-events-buffer-size 0
      read-process-output-max (* 1024 1024)
      eglot-autoshutdown t
      eglot-ignored-server-capabilities '(:documentHighlightProvider :inlayHintProvider))
;; --- eglot config finish

(dolist (hook eglot-manage-mode)
  (when (macrop 'gaeric-comma-leader-def)
    (gaeric-comma-leader-def
      "en" 'prog-next-error
      "ep" 'prog-prev-error
      "ef" 'eglot-format
      "er" 'eglot-code-actions
      "gd" 'xref-find-definitions
      "gr" 'xref-find-references
      "go" 'xref-find-definitions-other-window))

  (add-hook hook #'yas-minor-mode)
  (add-hook hook #'company-mode)
  (add-hook hook #'eglot-ensure))
  
;; breadcrumb config --- 
(require-package 'breadcrumb)
(add-hook 'after-init-hook (lambda () (breadcrumb-mode)))

;; xref config --- 
(unless  (version< "28.0" emacs-version)
  (require-package 'xref))

(with-eval-after-load 'xref
  (setq xref-history-storage 'xref-window-local-history)
  (setq xref-search-program 'ripgrep)
  ;; (setq xref-show-definitions-function 'xref-show-definitions-completing-read)
  ;; (setq xref-show-xrefs-function 'xref-show-definitions-completing-read)

  (setq xref-show-definitions-function 'consult-xref)
  (setq xref-show-xrefs-function 'consult-xref))

;; use treesit-auto-install-all to install grammers for treesit
(when (maybe-require-package 'treesit-auto)
  (require 'treesit-auto)
  (global-treesit-auto-mode)
  ;; disable toml-ts-mode, it's very slow
  (setq treesit-auto-langs
        (seq-remove (lambda (x) (memq x '(json toml cobol swift))) treesit-auto-langs)))

(provide 'init-langs)
