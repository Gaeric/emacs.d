;; init-winum.el --- quickly switch to window -*- coding: utf-8; lexical-binding: t; -*-
;; Author:  Gaeric
;; URL: https://github.com/Gaeric/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require-package 'winum)

(eval-after-load 'winum
  (progn
    ;; 修改winum标记的显示方式
    (setq winum-format "%s")
    ;; 不显示winum的标记
    (setq winum-mode-line-position 0)
    (winum-mode 1)
    (set-face-attribute 'winum-face nil :weight 'bold)
    (setq winum-keymap
          (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
            (define-key map (kbd "M-1") 'winum-select-window-1)
            (define-key map (kbd "M-2") 'winum-select-window-2)
            (define-key map (kbd "M-3") 'winum-select-window-3)
            (define-key map (kbd "M-4") 'winum-select-window-4)
            (define-key map (kbd "M-5") 'winum-select-window-5)
            (define-key map (kbd "M-6") 'winum-select-window-6)
            (define-key map (kbd "M-7") 'winum-select-window-7)
            (define-key map (kbd "M-8") 'winum-select-window-8)
            (define-key map (kbd "M-9") 'winum-select-window-9)))
    (gaeric-space-leader-def
      "0" 'winum-select-window-0-or-10
      "1" 'winum-select-window-1
      "2" 'winum-select-window-2
      "3" 'winum-select-window-3
      "4" 'winum-select-window-4
      "5" 'winum-select-window-5
      "6" 'winum-select-window-6
      "7" 'winum-select-window-7
      "8" 'winum-select-window-8
      "9" 'winum-select-window-9)))

(provide 'init-winum)
