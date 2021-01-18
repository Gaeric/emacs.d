;; init-ivy.el -*- coding: utf-8; lexical-binding: t; -*-
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(require-package 'ivy)
(require-package 'swiper)
(require-package 'counsel)

;; smex默认按历史排列使用的命令
(require-package 'amx)
(require-package 'flx)
;; turn on ivy
(add-hook 'after-init-hook 'ivy-mode)

;; for paste
(global-set-key (kbd "M-y") 'counsel-yank-pop)


(with-eval-after-load 'ivy
  (setq-default
   ;; ivy-wrap t
   ivy-use-virtual-buffers t
   ivy-virtual-abbreviate 'fullpath
   ivy-count-format "(%d/%d) "
   ivy-magic-tilde nil
   ;; ivy-dynamic-exhibit-delay-ms 150
   ivy-use-selectable-prompt t
   ivy-initial-inputs-alist '((Man-completion-table . "^") (woman . "^"))
   smex-save-file (expand-file-name ".smex-items" user-emacs-directory)
   amx-save-file (expand-file-name ".amx-items" user-emacs-directory)
   counsel-mode-override-describe-bindings t
   ivy-re-builders-alist
   '((t . ivy--regex-plus)
     (ivy-switch-buffer . ivy--regex-fuzzy)
     (counsel-M-x . ivy--regex-fuzzy)))

  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-s") 'swiper)

  (when (macrop 'gaeric-space-leader-def)
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
      ))

  ;; M-j default ivy-yank-word
  ;; C-M-j ivy-immediate-done
  ;; M-o ivy-dispatching
  ;; C-M-m ivy-call
  ;; C-M-o ivy-dispatching-call
  ;; ivy-restrict-to-matches => Shift + SPC
  (define-key ivy-minibuffer-map (kbd "M-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "M-k") 'ivy-previous-line)

  (diminish 'ivy-mode))

(provide 'init-ivy)
