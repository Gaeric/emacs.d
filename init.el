;; init.el -*- coding: utf-8; lexical-binding: t; -*-
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

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
  (require 'init-generic)
  (require 'init-utils)
  (require 'init-evil)
  (require 'init-git)
  (require 'init-theme)
  (require 'init-org)
  (require 'init-lim)

  ;;----------------------------------------------------------------------------
  ;; Advanced Config
  ;;----------------------------------------------------------------------------
  (require 'init-winum)
  (require 'init-ivy)
  (require 'init-project)
  (require 'init-xref)
  (require 'init-company)
  (require 'init-yasnippet)
  (require 'init-lsp)
  (require 'init-cc)

  ;;----------------------------------------------------------------------------
  ;; Prog Lang
  ;;----------------------------------------------------------------------------
  (require 'init-python)
  (require 'init-rust)

  (add-hook 'after-init-hook
          (lambda ()
            (progn
              (electric-pair-mode t)
              (show-paren-mode t)
              (toggle-frame-fullscreen))))

  (setq gc-cons-threshold normal-gc-cons-threshold))


