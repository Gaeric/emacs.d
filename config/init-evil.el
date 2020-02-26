;; -*- coding: utf-8; lexical-binding: t; -*-


(local-require 'evil)
(evil-mode 1)
(evil-declare-key 'normal org-mode-map
  (kbd "TAB") 'org-cycle)

(local-require 'general)
(general-create-definer gaeric-space-leader-def
  :prefix "SPC"
  :states '(normal visual))

(general-create-definer gaeric-comma-leader-def
  :prefix ","
  :states '(nor visual))

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
