;; -*- coding: utf-8; lexical-binding: t; -*-
;; init.el ---  -*- coding: utf-8; lexical-binding: t; -*-
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;;----------------------------------------------------------------------------
;; 1. Adjust garbage collection thresholds during startup, and thereafter
;; 2. set 
;; Max for setup, 64M for emacs28.
;; @See https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;;----------------------------------------------------------------------------
(let* ((file-name-handler-alist nil)
       (normal-gc-cons-threshold (* 64 1024 1024)))
  (setq gc-cons-threshold most-positive-fixnum)
  (add-hook 'after-init-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))
