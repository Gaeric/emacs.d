;; init.el -*- coding: utf-8; lexical-binding: t; -*-
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;;----------------------------------------------------------------------------
;; 1. Adjust garbage collection thresholds during startup, and thereafter
;;    Max for setup, 64M for emacs28.
;; 2. set file-name-handler-alist to nil
;; @See https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;;----------------------------------------------------------------------------
(let* ((file-name-handler-alist nil)
       (normal-gc-cons-threshold (* 64 1024 1024)))
  (setq gc-cons-threshold most-positive-fixnum)

  ;;----------------------------------------------------------------------------
  ;; Basic Config
  ;;----------------------------------------------------------------------------
  ;; (require 'init-benchmarking)
  (require 'init-generic)
  (require 'init-elpa)
  (require 'init-utils)
  (require 'init-editing-utils)
  (require 'init-clipboard)
  (require 'init-evil)
  (require 'init-git)
  (require 'init-theme)
  (require 'init-org)
  (require 'init-keyfreq)
  (require 'init-lim)

  ;;----------------------------------------------------------------------------
  ;; Advanced Config
  ;;----------------------------------------------------------------------------
  (require 'init-dired)
  (require 'init-ibuffer)
  (require 'init-window)
  (require 'init-pair)
  (require 'init-minibuffer)
  (require 'init-project)
  (require 'init-complete)
  (require 'init-yas)
  (require 'init-rs-module)
  (require 'init-eldoc)
  
  ;;----------------------------------------------------------------------------
  ;; Prog Lang
  ;;----------------------------------------------------------------------------
  (require 'init-langs)
  (require 'init-lsp-bridge)
  (require 'init-cc)
  (require 'init-python)
  (require 'init-rust)
  (require 'init-web)
  (require 'init-godot)
  (require 'init-sql)
  
  ;;----------------------------------------------------------------------------
  ;; tools
  ;;----------------------------------------------------------------------------
  (require 'init-ai)

  ;;----------------------------------------------------------------------------
  ;;
  ;;----------------------------------------------------------------------------
  (when (file-exists-p custom-file)
    (load custom-file))

  ;; Allow access from emacsclient
  ;; (add-hook 'after-init-hook
  ;;           (lambda ()
  ;;             (require 'server)
  ;;             (unless (server-running-p)
  ;;               (server-start))))

  (add-hook 'after-init-hook
            (lambda ()
              (progn
                (show-paren-mode t)
                (global-hl-line-mode)
                (global-so-long-mode)
                (save-place-mode)
                (winner-mode)
                (toggle-frame-maximized))))

  (setq gc-cons-threshold normal-gc-cons-threshold))

(provide 'init)
