(when (maybe-require-package 'gdscript-mode)
  (setq gdscript-use-tab-indents t) ;; If true, use tabs for indents. Default: t
  (setq gdscript-indent-offset 4) ;; Controls the width of tab-based indents
  ;; (setq gdscript-godot-executable "/path/to/godot") ;; Use this executable instead of 'godot' to open the Godot editor.
  (setq gdscript-gdformat-save-and-format t) ;; Save all buffers and format them with gdformat anytime Godot executable is run.
  )

(provide 'init-godot)
