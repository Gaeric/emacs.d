;; init-general.el --- config for vanilla emacs -*- coding: utf-8; lexical-binding: t; -*-
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3


;; 关闭菜单栏、工具栏及滚动条
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; 使屏幕可以平滑滚动
(setq scroll-margin 3
      scroll-conservatively 100)

;; inhibit start screen
(setq inhibit-startup-screen t)

(prefer-coding-system 'utf-8-unix)
(setq use-file-dialog nil
      use-dialog-box nil              ;never pop dialog
      ring-bell-function 'ignore      ;关闭出错时的提示声
      default-major-mode 'text-mode   ;设置默认地主模式为TEXT模式
      ad-redefinition-action 'warn    ;redefine warning 时进行警告
      resize-mini-windows nil         ;保持minibuffer window固定高度
      recentf-max-saved-items 2048    ;设置recentf的默认大小
      make-backup-files nil           ;默认不进行备份
      ad-redefinition-action 'accept  ;清除ad-handle-definition的告警
      )

(setq-default 
  comment-style 'indent   ;设定自动缩进的注释风格
  indent-tabs-mode nil    ;不使用Tab键插入\t
  cursor-type 'bar        ;设置默认的光标样式
  )

(global-auto-revert-mode t)            ;文件修改后自动载入
(column-number-mode 1)                 ;在modeline上展示行列位置（行默认开启）

(fset 'yes-or-no-p 'y-or-n-p)          ;以 y/n代表 yes/no
(set-face-attribute 'default nil :height 105) ;设置字体大小

;; Set up `load-path'
(eval-when-compile (require 'cl))
(if (fboundp 'normal-top-level-add-to-load-path)
    (let* ((site-lisp-dir "~/.emacs.d/site-lisp/")
	  (default-directory site-lisp-dir))
      (progn
	(setq load-path
	      (append
	       (cl-loop for dir in (directory-files site-lisp-dir)
		     unless (string-match "^\\." dir)
		     collecting (expand-file-name dir))
	       load-path)))))

;; for long line performance
;; @see https://emacs-china.org/t/topic/25811/5
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)


;; narrow config
(put 'narrow-to-region 'disabled nil)
(add-hook 'text-mode-hook 'save-place-local-mode)

(if (eq system-type 'window-nt)
    (add-to-list 'process-coding-system-alist '("[rR][gG]" . (utf-8-dos . windows-1251-dos))))

;; use customize-variable to config recentf-exclude for ignore some file
(setq recentf-exclude '("~/Downloads/pixiv/*"))

(provide 'init-generic)

;;; init-generic.el ends here
