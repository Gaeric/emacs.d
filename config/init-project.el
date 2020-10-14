;; init-project.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3


;; Find file/directory and review Diff/Patch/Commit quickly everywhere.
;; @see https://github.com/technomancy/find-file-in-project
(require-package 'find-file-in-project)

;; ffip diff for evil-mode

(defun ffip-diff-mode-hook-setup ()
    (evil-local-set-key 'normal "p" 'diff-hunk-prev)
    (evil-local-set-key 'normal "n" 'diff-hunk-next)
    (evil-local-set-key 'normal "P" 'diff-file-prev)
    (evil-local-set-key 'normal "N" 'diff-file-next)
    (evil-local-set-key 'normal (kbd "RET") 'ffip-diff-find-file)
    (evil-local-set-key 'normal "o" 'ffip-diff-find-file))
(add-hook 'ffip-diff-mode-hook 'ffip-diff-mode-hook-setup)

;; use fd (rust) replace GNU Find
(setq ffip-use-rust-fd t)

;; Only for project
(gaeric-comma-leader-def
  ;; "ff" 'find-file-in-project
  ;; "fp" 'find-file-in-project-at-point
  ;; "fs" 'find-file-in-project-by-selected ;类比于swiper-thing-at-point
  "gf" 'project-find-file
  "gs" 'project-find-regexp)
;; C-h i g (ivy) Enter for more key-binding tips.


(provide 'init-project)
