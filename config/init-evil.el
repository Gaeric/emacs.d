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

(when (maybe-require-package 'expand-region)
  (define-key evil-visual-state-map (kbd "v") 'er/expand-region))

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

;; `org-capture' is global
(gaeric-space-leader-def
  "oc" 'org-capture
  "oo" 'org-agenda
  "oa" 'org-agenda-list
  )

(gaeric-comma-leader-def
  :keymaps 'org-mode-map
  "op" 'org-pomodoro
  "cp" 'org-previous-visible-heading
  "cn" 'org-next-visible-heading
  "cf" 'org-forward-heading-same-level
  "cb" 'org-backward-heading-same-level
  "ns" 'org-narrow-to-subtree
  "nb" 'org-narrow-to-block
  "ne" 'org-narrow-to-element
  "'"  'org-edit-special)

(gaeric-comma-leader-def
  :keymaps 'org-src-mode-map
  "c'" 'org-edit-src-exit
  "ck" 'org-edit-src-abort)

(gaeric-comma-leader-def
  "wc" 'lim-count-words)

(gaeric-comma-leader-def
  ;; "ff" 'find-file-in-project
  ;; "fp" 'find-file-in-project-at-point
  ;; "fs" 'find-file-in-project-by-selected ;类比于swiper-thing-at-point
  "gf" 'project-find-file
  "gs" 'project-find-regexp)

(gaeric-space-leader-def
  ;; Ivy-based interface to standard commands
  "<SPC>" 'counsel-M-x
  "ss"    'swiper
  "si"    'swiper-isearch
  "sp"    'swiper-thing-at-point

  "hf"    'counsel-describe-function
  "hv"    'counsel-describe-variable
  "hd"    'counsel-descbinds             ;显示按键绑定
  "hl"    'counsel-find-library
  "hi"    'counsel-info-lookup-symbol
  "hu"    'counsel-unicode-char
  "hj"    'counsel-set-variable
  "cv"    'ivy-push-view
  "cV"    'ivy-pop-view

  ;; Ivy-based interface to shell and system tools
  "cc"    'counsel-compile ;counsel增强的编译选项
  "cg"    'counsel-git ;在受git控制的项目下使用git查找文件
  "cj"    'counsel-git-grep ;在受git控制的项目下使用git grep查找内容
  "cL"    'counsel-git-log ;搜索git log
  "cs"    'counsel-rg ;使用rg搜索当前目录
  "cm"    'counsel-linux-app ;启动linux下的应用

  ;; Ivy-resume and other commands
  ;; ivy-resume resumes the last Ivy-based completion.
  "cr" 'ivy-resume                    ;重复上一次的命令
  "cb" 'counsel-bookmark              ;查找bookmark中的内容
  "co" 'counsel-outline               ;用于在outline模式的大纲间快速移动
  "ct" 'counsel-load-theme            ;加载theme
  "cF" 'counsel-org-file              ;浏览org-mode中的所有附件
  )

(gaeric-comma-leader-def
  "gd" 'xref-find-definitions
  "gr" 'xref-find-references
  "go" 'xref-find-definitions-other-window)

(gaeric-comma-leader-def
  "en"  'prog-next-error
  "ep"  'prog-prev-error)

(provide 'init-evil)
