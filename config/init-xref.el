;; init-xref.el -*- coding: utf-8; lexical-binding: t; -*-
;; config xref for code navigation
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3


;; @see https://github.com/oantolin/embark/issues/145
(defun embark/xref-show-definitions-completing-read (fetcher alist)
  (let ((old-dir default-directory)
        (set-default-directory (lambda (dir) (setq default-directory dir))))
    (advice-add 'file-name-directory :filter-return set-default-directory)
    (unwind-protect
        (xref-show-definitions-completing-read fetcher alist)
      (setq default-directory old-dir)
      (advice-remove 'file-name-directory set-default-directory))))

(with-eval-after-load 'xref
  (setq xref-search-program 'ripgrep)
  (setq xref-show-xrefs-function 'embark/xref-show-definitions-completing-read)
  (setq xref-show-definitions-function 'embark/xref-show-definitions-completing-read))

(when (macrop 'gaeric-comma-leader-def)
  (gaeric-comma-leader-def
    "gd" 'xref-find-definitions
    "gr" 'xref-find-references
    "go" 'xref-find-definitions-other-window))

(provide 'init-xref)
