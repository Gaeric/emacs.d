;; init-editing-utils.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(require-package 'sudo-edit)

(when (maybe-require-package 'anzu)
  (add-hook 'after-init-hook 'global-anzu-mode)
  (setq anzu-mode-lighter "")
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (when (maybe-require-package 'evil-anzu)
    (with-eval-after-load 'evil
      (require 'evil-anzu))))


(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(add-hook 'after-init-hook 'electric-indent-mode)

;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(add-hook 'after-init-hook 'transient-mark-mode)

(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

(defun prog-next-error ()
  (interactive)
  (if (bound-and-true-p flymake-mode)
      (call-interactively 'flymake-goto-next-error)
    (if (bound-and-true-p flycheck-mode)
        (call-interactively 'flycheck-next-error))))

(defun prog-prev-error ()
  (interactive)
  (if (bound-and-true-p flymake-mode)
      (call-interactively 'flymake-goto-prev-error)
    (if (bound-and-true-p flycheck-mode)
        (call-interactively 'flycheck-previous-error))))

(defun decode-file-from-gbk ()
  (interactive)
  (decode-coding-region (point-min) (point-max) 'gbk))

(require-package 'diminish)

(with-eval-after-load 'auto-revert-mode
    (diminish 'auto-revert-mode))

(provide 'init-editing-utils)
