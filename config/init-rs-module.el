;; init-rs-module.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;; For example
;; (rs-module/load ".emacs.d/site-lisp/lim/target/release/liblim.so")
;; (lim-count-words (buffer-substring-no-properties (point-min) (point-max)))

(defvar gaeric/rs-module-default-path default-directory)

(defvar gaeric/rs-module-file-dir "dylib")

(defun dos-reload-workaround (path)
  "Hot Reload Dynamic Module On Windows"
  (let* ((file-abs (expand-file-name path))
         (file-date (format-time-string "%Y%m%d%H%M%S"))
         (file-new (concat (file-name-base file-abs) "_" file-date "." (file-name-extension file-abs))))
    (if (file-exists-p (concat gaeric/rs-module-file-dir file-new))
        (message "Skipping unmodified file %s" file-abs)
      (copy-file file-abs file-new)
      (let ((default-directory gaeric/rs-module-default-path))
        (rs-module/load (file-relative-name file-new))))))

(defun load-rs-module (path)
  (if (eq system-type 'window-nt)
      (dos-reload-workaround path)
    (rs-module/load
     (file-relative-name
      (expand-file-name path)
      gaeric/rs-module-default-path))))

(defvar libemacs-rs-module "~/.emacs.d/site-lisp/emacs-module-rs/target/release/libemacs_rs_module.so")
(defvar libemacs-rs-module-support t)

(if (file-exists-p libemacs-rs-module)
    (load-file libemacs-rs-module)
  (setq libemacs-rs-module-support nil))


(provide 'init-rs-module)
