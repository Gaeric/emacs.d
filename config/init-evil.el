;; init-evil.el --- Evil for emacs -*- coding: utf-8; lexical-binding: t; -*-
;; Author:  Gaeric
;; URL: https://github.com/Gaeric/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require-package 'evil-collection)
;; (setq evil-want-integration t)  ; This is optional since it's already default
(setq evil-want-keybinding nil)
(setq evil-collection-exclude-modes '(company tide python))
(with-eval-after-load 'evil-collection
  (dolist (mode evil-collection-exclude-modes)
    (setq evil-collection-mode-list
          (delq mode evil-collection-mode-list))))
(evil-collection-init)

(when (maybe-require-package 'evil-org)
  (with-eval-after-load 'org-agenda
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)))

;;----------------------------------------------------------------------------
;; evil config
;; @see https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-evil.el
;;----------------------------------------------------------------------------

(when (maybe-require-package 'undo-fu)
  (define-minor-mode undo-fu-mode
    "Enables `undo-fu' for the curent session."
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map [remap undo] #'undo-fu-only-undo)
              (define-key map [remap redo] #'undo-fu-only-redo)
              map)
    :init-value nil
    :global t)


  (undo-fu-mode 1)
  (setq undo-limit 8000000)
  (setq undo-strong-limit 8000000)
  (setq undo-outer-limit 8000000))

(when (maybe-require-package 'evil)
  (setq evil-undo-system 'undo-fu)
  (evil-mode 1)
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map (kbd "C-r") 'undo-fu-only-redo)
  (setq evil-move-cursor-back t))


(require-package 'general)
(general-create-definer gaeric-space-leader-def
  :prefix "SPC"
  :states '(normal visual))

(general-create-definer gaeric-comma-leader-def
  :prefix ","
  :states '(normal visual))


;; As a general RULE, mode specific evil leader keys started
;; with uppercased character or 'g' or special character except "=" and "-"
(evil-declare-key 'normal org-mode-map
  "$" 'org-end-of-line ; smarter behaviour on headlines etc.
  "^" 'org-beginning-of-line ; ditto
  (kbd "TAB") 'org-cycle)

(when (maybe-require-package 'expand-region)
  (define-key evil-visual-state-map (kbd "v") 'er/expand-region))

;; {{ specify major mode uses Evil (vim) NORMAL state or EMACS original state.
;; You may delete this setup to use Evil NORMAL state always.
;; Such as
;; (dolist (p '((minibuffer-inactive-mode . emacs)
;;              (ivy-occur-grep-mode . normal)
;;              (js2-error-buffer-mode . emacs)
;;              (image-mode . emacs)))
;;   (evil-set-initial-state (car p) (cdr p)))
;; }}


;; Prefer Emacs way after pressing ":" in evil-mode
(define-key evil-ex-completion-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
(define-key evil-ex-completion-map (kbd "M-p") 'previous-complete-history-element)
(define-key evil-ex-completion-map (kbd "M-n") 'next-complete-history-element)


(when (maybe-require-package 'avy)
  (setq avy-all-windows nil)
  (setq avy-style 'pre)
  (global-set-key (kbd "M-g") 'avy-goto-char-timer))

;;----------------------------------------------------------------------------
;; evil-surround config
;;----------------------------------------------------------------------------
(require-package 'evil-surround)
(global-evil-surround-mode 1)

(defun evil-surround-emacs-lisp-mode-hook-setup ()
  (push '(?\( . ("( " . ")")) evil-surround-pairs-alist)
  (push '(?` . ("`" . "'")) evil-surround-pairs-alist))
(add-hook 'emacs-lisp-mode-hook 'evil-surround-emacs-lisp-mode-hook-setup)

(defun evil-surround-org-mode-hook-setup ()
  (push '(93 . ("[[" . "]]")) evil-surround-pairs-alist) ; ]
  (push '(?= . ("=" . "=")) evil-surround-pairs-alist)
  (push '(?~ . ("~" . "~")) evil-surround-pairs-alist))
(add-hook 'org-mode-hook 'evil-surround-org-mode-hook-setup)


;;----------------------------------------------------------------------------
;; evil-visualstar config
;;----------------------------------------------------------------------------
(require-package 'evil-visualstar)
(global-evil-visualstar-mode 1)


;; TODO: ffip
;; TODO: evil-matchit


;;----------------------------------------------------------------------------
;; evil-exchange config
;;----------------------------------------------------------------------------
(require-package 'evil-exchange)
(evil-exchange-install)

;;----------------------------------------------------------------------------
;; evil-nerd-commenter config
;; @see https://github.com/redguardtoo/evil-nerd-commenter
;;----------------------------------------------------------------------------
(require-package 'evil-nerd-commenter)

;;----------------------------------------------------------------------------
;; evil-matchit config
;;----------------------------------------------------------------------------
(when (maybe-require-package 'evil-matchit)
  (setq evilmi-shortcut "m")
  (add-hook 'prog-mode-hook 'evil-matchit-mode))


;; Spaces keybinds for vanilla Emacs
(when (macrop 'gaeric-space-leader-def)
  (gaeric-space-leader-def
    ;; "<SPC>" 'execute-extended-command
    "ff"    'find-file
    "fo"    'find-file-other-window
    "bo"    'switch-to-buffer-other-window
    "fs"    'save-buffer
    "bd"    'kill-this-buffer
    "bs"    'switch-to-scratch-buffer
    "bi"    'ibuffer
    "bm"    'switch-to-message-buffer
    "bb"    'switch-to-buffer
    "fed"   'open-init-file
    "ad"    'dired
    "tl"    'toggle-truncate-lines
    "tn"    'display-line-numbers-mode
    "wc"    'count-words
    "!"     'shell-command
    ))


;; Emacs key bindings
;; comment lines useful for normal mode
;; (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
(when (macrop 'gaeric-comma-leader-def)
  (gaeric-comma-leader-def
    "ci" 'evilnc-comment-or-uncomment-lines
    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    "cc" 'evilnc-copy-and-comment-lines
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "cr" 'comment-or-uncomment-region
    "cv" 'evilnc-toggle-invert-comment-line-by-line
    "."  'evilnc-copy-and-comment-operator
    "\\" 'evilnc-comment-operator ; if you prefer backslash key
    )

  (gaeric-comma-leader-def
    "cx"  'copy-to-x-clipboard
    "px"  'paste-from-x-clipboard
    "kc"  'kill-ring-to-clipboard
    "ee"  'eval-last-sexp
    ","   'evil-repeat-find-char-reverse
    "nn"  'narrow-to-region
    "nd"   'narrow-to-defun
    "np"   'narrow-to-page
    "ma"   'beginning-of-defun
    "me"   'end-of-defun
    "nw"    'widen
    ))

(provide 'init-evil)
