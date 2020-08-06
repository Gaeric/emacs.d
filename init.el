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
  (require 'init-yas)
  (require 'init-lsp)

  ;;----------------------------------------------------------------------------
  ;; Prog Lang
  ;;----------------------------------------------------------------------------
  (require 'init-cc)
  (require 'init-python)
  (require 'init-rust)
  (require 'init-web)

  (add-hook 'after-init-hook
            (lambda ()
              (progn
                (electric-pair-mode t)
                (show-paren-mode t)
                ;; (toggle-frame-fullscreen)
                )))

  (setq gc-cons-threshold normal-gc-cons-threshold))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((pyvenv-workon . gsg))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:foreground "gold1" :height 1.2))))
 '(org-level-1 ((t (:foreground "DodgerBlue1" :weight normal :height 1.0 :blod nil))))
 '(org-level-2 ((t (:foreground "SteelBlue1" :weight normal :height 1.0 :bold nil))))
 '(org-level-3 ((t (:foreground "DeepSkyBlue1" :weight normal :height 1.0 :bold nil))))
 '(org-level-4 ((t (:foreground "SkyBlue1"))))
 '(org-level-5 ((t (:foreground "MediumOrchid1"))))
 '(org-level-6 ((t (:foreground "hot pink"))))
 '(org-level-7 ((t (:foreground "magenta"))))
 '(org-level-8 ((t (:foreground "deep pink")))))
