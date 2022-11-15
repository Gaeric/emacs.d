;; init-rs-module.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(defvar gaeric/rs-module-default-path default-directory)
(defvar gaeric/rs-module-file-dir "dylib")

(defun dos-reload-workaround (path)
  "Hot Reload Dynamic Module On Windows"
  (let* ((file-abs (expand-file-name path))
         (file-date (format-time-string "%Y%m%d%H%M%S"))
         (file-new (concat (file-name-base file-abs) "_" file-date "." (file-name-extension file-abs))))
    (if (file-exists-p (concat gaeric-static-file-dir file-new))
        (message "Skipping unmodified file %s" file-abs)
      (copy-file file-abs file-new)
      (let ((default-directory gaeric/rs-module-default-path))
        (rs-module/load (file-relative-name file-new))))))

(defun load-rs-module (path)
  (rs-module/load (file-relative-name (expand-file-name path) gaeric/rs-module-default-path)))


(provide 'init-rs-module)
