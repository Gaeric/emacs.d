;; init-ivy.el -*- coding: utf-8; lexical-binding: t; -*-
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;; (require 'flx)

(require 'ivy)
(require 'swiper)
(require 'counsel)

(ivy-mode 1)

;; for paste
(global-set-key (kbd "M-y") 'counsel-yank-pop)

(with-eval-after-load 'ivy
  (define-key ivy-mode-map (kbd "M-s /") 'swiper-thing-at-point)
  (setq-default
   ;; ivy-wrap t
   ivy-use-virtual-buffers t
   ivy-virtual-abbreviate 'fullpath
   ivy-count-format "(%d/%d) "
   ivy-magic-tilde nil
   ivy-dynamic-exhibit-delay-ms 150
   ivy-use-selectable-prompt t
   ;; ivy-initial-inputs-alist '((Man-completion-table . "^") (woman . "^"))
   smex-save-file (expand-file-name ".smex-items" user-emacs-directory)
   counsel-mode-override-describe-bindings t
   )

  (gaeric-space-leader-def
    ;; Ivy-based interface to standard commands
    ;; "bb"    'switch-to-buffer         ; replace by ivy-switch-buffer
    "<SPC>" 'counsel-M-x
    "ss"    'swiper
    "si"    'swiper-isearch
    "sp"    'swiper-thing-at-point
    "hf"    'counsel-describe-function
    "hv"    'counsel-describe-variable
    "hl"    'counsel-find-library
    "hi"    'counsel-info-lookup-symbol
    "hu"    'counsel-unicode-char
    "hj"    'counsel-set-variable
    "cv"    'ivy-push-view
    "cV"    'ivy-pop-view

    ;; Ivy-based interface to shell and system tools
    "cc"    'counsel-compile
    "cg"    'counsel-git
    "cj"    'counsel-git-grep
    "cL"    'counsel-git-log
    "cs"    'counsel-rg
    "cm"    'counsel-linux-app

    ;; Ivy-resume and other commands
    ;; ivy-resume resumes the last Ivy-based completion.
    "cr" 'ivy-resume
    "cb" 'counsel-bookmark
    "cd" 'counsel-descbinds
    "co" 'counsel-outline
    "ct" 'counsel-load-theme
    "cF" 'counsel-org-file
    )

  ;; M-j default ivy-yank-word
  ;; C-M-j ivy-immediate-done
  ;; M-o ivy-dispatching
  ;; C-M-m ivy-call
  ;; C-M-o ivy-dispatching-call
  ;; ivy-restrict-to-matches => Shift + SPC
  (define-key ivy-minibuffer-map (kbd "M-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "M-k") 'ivy-previous-line)

  (diminish 'ivy-mode))

(provide 'init-ivy)
