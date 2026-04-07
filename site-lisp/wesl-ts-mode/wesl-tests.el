;;; wesl-tests.el --- Tree-sitter support for the WebGPU Shading Language -*- lexical-binding: t; -*-

(let ((buf (get-buffer-create "test-buffer")))
  (with-current-buffer buf
    (erase-buffer)
    (insert-file-contents "utils.wgsl"))
  (switch-to-buffer buf)
  (wesl-ts-mode))
