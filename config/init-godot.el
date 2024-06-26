;; init-godot.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(when (maybe-require-package 'gdscript-mode)
  ;; If true, use tabs for indents. Default: t
  ;; (setq gdscript-use-tab-indents t) 

  ;; Controls the width of tab-based indents
  ;; (setq gdscript-indent-offset 4) 

  ;; Use this executable instead of 'godot' to open the Godot editor.
  ;; (setq gdscript-godot-executable "/path/to/godot") 

  ;; Save all buffers and format them with gdformat anytime Godot executable is run.
  (setq gdscript-gdformat-save-and-format t))

(provide 'init-godot)
