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
