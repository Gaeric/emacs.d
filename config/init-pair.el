;; init-pair.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3


(defun gaeric-pair-jump-left ()
  "To left of previous match parentheses."
  (interactive)
  (backward-char 1)
  (while (not (looking-at "\\(['\"<({]\\|[[]\\)")) (backward-char 1)))


;; kind of (up-list)
(defun gaeric-pair-jump-right ()
  "To right of next match parentheses."
  (interactive)
  ;; Jump out of string if cursor in string area.
  (while (not (looking-at "\\(['\">)}]\\|]\\)")) (forward-char 1))
  (forward-char 1))

(define-key global-map (kbd "M-n") 'gaeric-pair-jump-right)
(define-key global-map (kbd "M-p") 'gaeric-pair-jump-left)

(provide 'init-pair)
