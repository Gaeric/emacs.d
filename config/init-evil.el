;; init-evil.el --- Evil for emacs -*- coding: utf-8; lexical-binding: t; -*-
;; Author:  Gaeric
;; URL: https://github.com/Gaeric/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;----------------------------------------------------------------------------
;; evil-escape config
;; @see https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-evil.el
;;----------------------------------------------------------------------------
(local-require 'evil)
(evil-mode 1)
(setq evil-move-cursor-back t)
(evil-declare-key 'normal org-mode-map
  (kbd "TAB") 'org-cycle)


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
;; evil-virualstar config
;;----------------------------------------------------------------------------
(local-require 'evil-visualstar)
(global-evil-visualstar-mode 1)


;; TODO: ffip


;;----------------------------------------------------------------------------
;; evil-escape config
;; discard evil-escape
;;----------------------------------------------------------------------------


;;----------------------------------------------------------------------------
;; evil-exchange config
;;----------------------------------------------------------------------------
(local-require 'evil-exchange)
(evil-exchange-install)


;;----------------------------------------------------------------------------
;; evil-nerd-commenter config
;; @see https://github.com/redguardtoo/evil-nerd-commenter
;;----------------------------------------------------------------------------
(local-require 'evil-nerd-commenter)
;; Emacs key bindings
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-line)
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


(local-require 'general)
(general-create-definer gaeric-space-leader-def
  :prefix "SPC"
  :states '(normal visual))

(general-create-definer gaeric-comma-leader-def
  :prefix ","
  :states '(normal visual))

;; Spaces keybinds for vanilla Emacs
(gaeric-space-leader-def
  "ff"    'find-file
  "fo"    'find-file-other-window
  "bo"    'switch-to-buffer-other-window
  "fs"    'save-buffer
  "bd"    'kill-this-buffer
  "bs"    'switch-to-scratch-buffer
  "bm"    'switch-to-message-buffer
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
    ","   'evil-repeat-find-char-reverse)

(provide 'init-evil)
