;; init-evil.el --- Evil for emacs -*- coding: utf-8; lexical-binding: t; -*-
;; Author:  Gaeric
;; URL: https://github.com/Gaeric/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; evil-want-keybinding must be set before evil/evil-surround load
(maybe-require-package 'undo-fu)
(maybe-require-package 'expand-region)

(setq evil-want-keybinding nil)
(setq undo-limit 8000000
      undo-strong-limit 8000000
      undo-outer-limit 8000000)

;; evil keybinds for org-mode/org-agenda
(when (maybe-require-package 'evil-org)
  (with-eval-after-load 'org-agenda
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)))

(when (maybe-require-package 'avy)
  (setq avy-all-windows nil)
  (setq avy-style 'pre)
  (global-set-key (kbd "M-g") 'avy-goto-char-timer))


;; evil/evil-plugin
(require-package 'evil-collection)
(require-package 'evil-surround)
(require-package 'evil-visualstar)
(require-package 'evil-exchange)
(require-package 'evil-nerd-commenter)


(when (maybe-require-package 'evil-escape)
  (setq-default evil-escape-delay 0.3)
  (setq evil-escape-excluded-major-modes '(dired-mode))
  (setq-default evil-escape-key-sequence ",.")
  ;; disable evil-escape when input method is on
  (evil-escape-mode 1))

(setq evil-collection-exclude-modes '(company tide python))
(with-eval-after-load 'evil-collection
  (dolist (mode evil-collection-exclude-modes)
    (setq evil-collection-mode-list
          (delq mode evil-collection-mode-list))))

(defun evil-surround-emacs-lisp-mode-hook-setup ()
  (push '(?\( . ("( " . ")")) evil-surround-pairs-alist)
  (push '(?` . ("`" . "'")) evil-surround-pairs-alist))
(add-hook 'emacs-lisp-mode-hook 'evil-surround-emacs-lisp-mode-hook-setup)

(defun evil-surround-org-mode-hook-setup ()
  (push '(93 . ("[[" . "]]")) evil-surround-pairs-alist) ; ]
  (push '(?= . ("=" . "=")) evil-surround-pairs-alist)
  (push '(?~ . ("~" . "~")) evil-surround-pairs-alist))
(add-hook 'org-mode-hook 'evil-surround-org-mode-hook-setup)

(when (maybe-require-package 'evil)
  ;; (setq evil-undo-system 'undo-tree)
  (setq evil-undo-system 'undo-redo)
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map (kbd "C-r") 'undo-fu-only-redo)
  ;; @see https://www.reddit.com/r/emacs/comments/12arjtn/my_basic_keybinding_setup_for_emacs_with_evilmode/
  (define-key evil-normal-state-map "U" 'undo-fu-only-redo)

  (setq evil-move-cursor-back t)
  (add-hook 'after-init-hook 'evil-mode))

(with-eval-after-load 'evil
  (evil-collection-init))


;; {{ change mode-line color by evil state
(with-eval-after-load 'evil
  (defconst my-default-color (cons "#424242"  "#eaeaea"))
  ;; (cons (face-background 'mode-line) (face-foreground 'mode-line))
  (defun my-show-evil-state ()
    "Change mode line color to notify user evil current state."
    (let* ((color (cond ((minibufferp) my-default-color)
                        ((evil-insert-state-p) '("#8d00a0" . "#ffffff"))
                        ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                        ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                        (t my-default-color))))
      (set-face-background 'mode-line (car color))
      (set-face-foreground 'mode-line (cdr color))))
  (add-hook 'post-command-hook #'my-show-evil-state))
;; }}


(with-eval-after-load 'evil
  ;;----------------------------------------------------------------------------
  ;; evil config
  ;; @see https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-evil.el
  ;;----------------------------------------------------------------------------

  ;; As a general RULE, mode specific evil leader keys started
  ;; with uppercased character or 'g' or special character except "=" and "-"
  (evil-declare-key 'normal org-mode-map
    "$" 'org-end-of-line ; smarter behaviour on headlines etc.
    "^" 'org-beginning-of-line ; ditto
    (kbd "TAB") 'org-cycle)

  (define-key evil-visual-state-map (kbd "v") 'er/expand-region)

  ;; {{ specify major mode uses Evil (vim) NORMAL state or EMACS original state.
  ;; You may delete this setup to use Evil NORMAL state always.
  ;; Such as
  ;; (dolist (p '((minibuffer-inactive-mode . emacs)
  ;;              (image-mode . emacs)))
  ;;   (evil-set-initial-state (car p) (cdr p)))
  ;; }}


  ;; Prefer Emacs way after pressing ":" in evil-mode
  (define-key evil-ex-completion-map (kbd "C-a") 'move-beginning-of-line)
  (define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
  (define-key evil-ex-completion-map (kbd "M-p") 'previous-complete-history-element)
  (define-key evil-ex-completion-map (kbd "M-n") 'next-complete-history-element)

  ;; evil-plugin
  (global-evil-surround-mode 1)
  (global-evil-visualstar-mode 1)
  (evil-exchange-install))

;;----------------------------------------------------------------------------
;; evil-matchit config
;;----------------------------------------------------------------------------
(when (maybe-require-package 'evil-matchit)
  (setq evilmi-shortcut "m")
  (add-hook 'prog-mode-hook 'evil-matchit-mode))

(when (maybe-require-package 'evil-goggles)
  (setq evil-goggles-duration 0.100) 
  (setq evil-goggles-enable-delete                nil
        evil-goggles-enable-yank                  t
        evil-goggles-enable-change                nil
        evil-goggles-enable-indent                nil
        evil-goggles-enable-join                  nil
        evil-goggles-enable-fill-and-move         nil
        evil-goggles-enable-shift                 nil
        evil-goggles-enable-surround              nil
        evil-goggles-enable-commentary            nil
        evil-goggles-enable-nerd-commenter        nil
        evil-goggles-enable-replace-with-register nil
        evil-goggles-enable-set-marker            nil
        evil-goggles-enable-record-macro          nil
        evil-goggles-enable-paste                 nil
        evil-goggles-enable-paste                 nil)
  (add-hook 'after-init-hook 'evil-goggles-mode))

;;----------------------------------------------------------------------------
;; General config -- As evil leader
;;----------------------------------------------------------------------------
(require-package 'general)
(require 'general)

(general-create-definer gaeric-space-leader-def
  :prefix "SPC"
  :states '(normal visual))

(general-create-definer gaeric-comma-leader-def
  :prefix ","
  :states '(normal visual))


;; Spaces keybinds for vanilla Emacs
(when (macrop 'gaeric-space-leader-def)
  (gaeric-space-leader-def
    "<SPC>" 'execute-extended-command
    "ff"    'find-file
    "fo"    'find-file-other-window
    "bo"    'switch-to-buffer-other-window
    "fs"    'save-buffer
    "bd"    'kill-current-buffer
    "bs"    'switch-to-scratch-buffer
    "bi"    'ibuffer
    "bm"    'switch-to-message-buffer
    "bb"    'switch-to-buffer
    "bp"    'project-switch-to-buffer
    "fed"   'open-init-file
    "ad"    'dired
    "es"    'eshell
    "tl"    'toggle-truncate-lines
    "tn"    'display-line-numbers-mode
    "!"     'shell-command
    "hf"    'describe-function
    "hv"    'describe-variable
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
    "nd"  'narrow-to-defun
    "np"  'narrow-to-page
    "nw"  'widen
    "ma"  'beginning-of-defun
    "me"  'end-of-defun
    "mm"  'mark-defun
    "xg"  'revert-buffer-quick
    ))

(provide 'init-evil)
