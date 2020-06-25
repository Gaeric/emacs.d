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

(setq use-file-dialog nil)
(setq use-dialog-box nil)              ;never pop dialog
(setq ring-bell-function 'ignore)      ;关闭出错时的提示声
(setq default-major-mode 'text-mode)   ;设置默认地主模式为TEXT模式
(setq select-enable-clipboard t)       ;支持emacs和外部程序的粘贴
(setq ad-redefinition-action 'warn)    ;redefine warning 时进行警告
(setq resize-mini-windows nil)         ;保持minibuffer window固定高度
(setq recentf-max-saved-items 2048)    ;设置recentf的默认大小
(setq-default comment-style 'indent)   ;设定自动缩进的注释风格
(setq-default indent-tabs-mode nil)    ;不使用Tab键插入\t
(setq-default cursor-type 'bar)        ;设置默认的光标样式
(setq make-backup-files nil)           ;默认不进行备份
(global-auto-revert-mode t)            ;文件修改后自动载入
(column-number-mode 1)                 ;在modeline上展示行列位置（行默认开启）

(fset 'yes-or-no-p 'y-or-n-p)          ;以 y/n代表 yes/no
(set-face-attribute 'default nil :height 110) ;设置字体大小

;; Set up `load-path'
(eval-when-compile (require 'cl))
(if (fboundp 'normal-top-level-add-to-load-path)
    (let* ((site-lisp-dir "~/.emacs.d/site-lisp/")
	  (default-directory site-lisp-dir))
      (progn
	(setq load-path
	      (append
	       (loop for dir in (directory-files site-lisp-dir)
		     unless (string-match "^\\." dir)
		     collecting (expand-file-name dir))
	       load-path)))))

;; Setting English Font
(set-face-attribute
 'default nil :family "Monospace")
;; Setting Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (font-spec :family "WenQuanYi Micro Hei Mono" :size 14)))

;; narrow config
(put 'narrow-to-region 'disabled nil)
;;----------------------------------------------------------------------------
;; make dired use the same buffer for viewing directory
;; http://ergoemacs.org/emacs/emacs_dired_tips.html
;;----------------------------------------------------------------------------
(with-eval-after-load 'dired
  ;; was dired-advertised-find-file
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  ;; was dired-up-directory
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))))

(provide 'init-generic)

;;; init-generic.el ends here
