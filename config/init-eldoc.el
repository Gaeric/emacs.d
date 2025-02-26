;; init-eldoc.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(when (maybe-require-package 'eldoc-box)
  (when (not (and (version< emacs-version "31.0") (display-graphic-p)))
    (setq eldoc-box-max-pixel-height 200)
    (add-hook 'prog-mode-hook #'eldoc-mode)
    (add-hook 'eldoc-mode-hook #'eldoc-box-hover-mode)
    (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
    (setq eldoc-box-max-pixel-height 300)))

(defun gaeric/show-eldoc-buffer ()
  (interactive)
  (save-excursion
    (switch-to-buffer-other-window (eldoc-doc-buffer))
    (let ((eldoc-doc-buffer-width 50)
          (width (window-text-width)))
      (if (> width eldoc-doc-buffer-width)
          (shrink-window-horizontally (- width eldoc-doc-buffer-width))
        (enlarge-window-horizontally (- eldoc-doc-buffer-width width))))))

(when (macrop 'gaeric-space-leader-def)
  (gaeric-space-leader-def
    "ei" 'gaeric/show-eldoc-buffer))


(provide 'init-eldoc)
