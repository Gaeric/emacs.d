;; init-editing-utils.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;; (setq beacon-before-blink-hook nil)
;; beacon-color 0.5

(when (maybe-require-package 'beacon)
  (defun gaeric/change-beacon-color ()
    "To remind the current mode"
    (let ((color
           (cond
            ((evil-emacs-state-p) "#8d00a0")
            ((evil-normal-state-p) "#006fa0")
            (t "#444488"))))
      (setq beacon-color color)))

  (setq-default beacon-lighter "")
  (setq-default beacon-size 20)
  (add-hook 'beacon-before-blink-hook 'gaeric/change-beacon-color)
  (add-hook 'after-init-hook 'beacon-mode))

(require-package 'sudo-edit)

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
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))

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

(provide 'init-editing-utils)
