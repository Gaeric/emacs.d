;; init-general.el --- config for vanilla emacs -*- coding: utf-8; lexical-binding: t; -*-
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
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

(setq use-file-dialog nil)
(setq use-dialog-box nil)              ;never pop dialog
(setq ring-bell-function 'ignore)      ;关闭出错时的提示声
(setq default-major-mode 'text-mode)   ;设置默认地主模式为TEXT模式
(setq x-select-enable-clipboard t)     ;支持emacs和外部程序的粘贴
(setq ad-redefinition-action 'warn)    ;redefine warning 时进行警告
(setq resize-mini-windows nil)         ;保持minibuffer window固定高度
(setq recentf-max-saved-items 2048)    ;设置recentf的默认大小
(setq-default comment-style 'indent)   ;设定自动缩进的注释风格
(setq-default indent-tabs-mode nil)    ;不使用Tab键插入\t
(setq-default cursor-type 'bar)        ;设置默认的光标样式
(setq make-backup-files nil)           ;默认不进行备份

(fset 'yes-or-no-p 'y-or-n-p)          ;以 y/n代表 yes/no
(set-face-attribute 'default nil :height 110) ;设置字体大小

(provide 'init-generic)

;;; init-generic.el ends here
