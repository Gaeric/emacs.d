;; init-evil.el --- Evil for emacs -*- coding: utf-8; lexical-binding: t; -*-
;; Author:  Gaeric
;; URL: https://github.com/Gaeric/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;----------------------------------------------------------------------------
;; evil config
;; @see https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-evil.el
;;----------------------------------------------------------------------------
(when (maybe-require-package 'evil)
  (maybe-require-package 'undo-fu)
  (setq evil-undo-system 'undo-fu)
  (evil-mode 1)
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

(evil-declare-key 'motion help-mode-map
  (kbd "TAB") 'forward-button)


;; {{ specify major mode uses Evil (vim) NORMAL state or EMACS original state.
;; You may delete this setup to use Evil NORMAL state always.
(dolist (p '((minibuffer-inactive-mode . emacs)
             (calendar-mode . emacs)
             (special-mode . emacs)
             (grep-mode . emacs)
             (Info-mode . emacs)
             (term-mode . emacs)
             (sdcv-mode . emacs)
             (anaconda-nav-mode . emacs)
             (log-edit-mode . emacs)
             (vc-log-edit-mode . emacs)
             (magit-log-edit-mode . emacs)
             (erc-mode . emacs)
             (neotree-mode . emacs)
             (w3m-mode . emacs)
             (gud-mode . emacs)
             (help-mode . motion)
             (eshell-mode . emacs)
             (shell-mode . emacs)
             (xref--xref-buffer-mode . emacs)
             ;;(message-mode . emacs)
             (epa-key-list-mode . emacs)
             (fundamental-mode . normal)
             (weibo-timeline-mode . emacs)
             (weibo-post-mode . emacs)
             (woman-mode . emacs)
             (sr-mode . emacs)
             (profiler-report-mode . emacs)
             (dired-mode . emacs)
             (compilation-mode . emacs)
             (speedbar-mode . emacs)
             (ivy-occur-mode . emacs)
             (ffip-file-mode . emacs)
             (ivy-occur-grep-mode . normal)
             (messages-buffer-mode . normal)
             (js2-error-buffer-mode . emacs)
             (image-mode . emacs)))
  (evil-set-initial-state (car p) (cdr p)))
;; }}


;; Prefer Emacs way after pressing ":" in evil-mode
(define-key evil-ex-completion-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
(define-key evil-ex-completion-map (kbd "M-p") 'previous-complete-history-element)
(define-key evil-ex-completion-map (kbd "M-n") 'next-complete-history-element)


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
  (global-evil-matchit-mode 1))



;; Emacs key bindings
;; comment lines useful for normal mode
;; (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
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


;; Spaces keybinds for vanilla Emacs
(gaeric-space-leader-def
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
  "w/"    'split-window-right
  "w-"    'split-window-below
  "ad"    'dired
  "tl"    'toggle-truncate-lines
  "tn"    'display-line-numbers-mode
  "wc"    'count-words
  "!"     'shell-command
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
  )

(provide 'init-evil)
