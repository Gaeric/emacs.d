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
(local-require 'evil)
(evil-mode 1)
(diminish 'undo-tree-mode)
(setq evil-move-cursor-back t)


(local-require 'general)
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
             (help-mode . emacs)
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
             (js2-error-buffer-mode . emacs)))
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
(local-require 'evil-surround)
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
(local-require 'evil-visualstar)
(global-evil-visualstar-mode 1)


;; TODO: ffip
;; TODO: evil-matchit


;;----------------------------------------------------------------------------
;; evil-exchange config
;;----------------------------------------------------------------------------
(local-require 'evil-exchange)
(evil-exchange-install)


;;----------------------------------------------------------------------------
;; evil-nerd-commenter config
;; @see https://github.com/redguardtoo/evil-nerd-commenter
;;----------------------------------------------------------------------------
(require 'evil-nerd-commenter)
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
  "bm"    'switch-to-message-buffer
  "bb"    'switch-to-buffer
  "fed"   'open-init-file
  "w/"    'split-window-right
  "w-"    'split-window-below
  "ad"    'dired
  "tl"    'toggle-truncate-lines
  "tn"    'linum-mode
  "wc"    'count-words
  "!"     'shell-command
  "nw"    'widen)

(gaeric-comma-leader-def
    "cx"  'copy-to-x-clipboard
    "px"  'paste-from-x-clipboard
    "kc"  'kill-ring-to-clipboard
    ","   'evil-repeat-find-char-reverse
    "cn"  'flymake-goto-next-error
    "cp"  'flymake-goto-prev-error)

(provide 'init-evil)
