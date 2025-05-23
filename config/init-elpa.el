;;; init-elpa.el --- Settings and helpers for package.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)

(setq package-check-signature nil)

(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))


;;; Standard package repositories
;; (setq package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;                          ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
;;                          ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

;; Work-around for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(when (and (version< emacs-version "26.3")
           (boundp 'libgnutls-version)
           (>= libgnutls-version 30604))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))


;;; On-demand installation of packages

(require 'cl-lib)

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (or (package-installed-p package min-version)
      (let* ((known (cdr (assoc package package-archive-contents)))
             (versions (mapcar #'package-desc-version known)))
        (if (cl-find-if (lambda (v) (version-list-<= min-version v)) versions)
            (package-install package)
          (if no-refresh
              (error "No version of %s >= %S is available" package min-version)
            (package-refresh-contents)
            (require-package package min-version t))))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))


;;; Fire up package.el

;; (unless (file-exists-p package-quickstart-file)
;; (package-quickstart-refresh))

(package-initialize)


;; package.el updates the saved version of package-selected-packages correctly only
;; after custom-file has been loaded, which is a bug. We work around this by adding
;; the required packages to package-selected-packages after startup is complete.

(defvar sanityinc/required-packages nil)

(defun sanityinc/note-selected-package (oldfun package &rest args)
  "If OLDFUN reports PACKAGE was successfully installed, note it in `sanityinc/required-packages'."
  (let ((available (apply oldfun package args)))
    (prog1 available
      (when (and available (boundp 'package-selected-packages))
        (add-to-list 'sanityinc/required-packages package)))))

(advice-add 'require-package :around 'sanityinc/note-selected-package)

(when (fboundp 'package--save-selected-packages)
  (require-package 'seq)
  (add-hook 'after-init-hook
            (lambda () (package--save-selected-packages
                        (seq-uniq (append sanityinc/required-packages package-selected-packages))))))


(let ((package-check-signature nil))
  (require-package 'gnu-elpa-keyring-update))


(defun sanityinc/set-tabulated-list-column-width (col-name width)
  "Set any column with name COL-NAME to the given WIDTH."
  (when (> width (length col-name))
    (cl-loop for column across tabulated-list-format
             when (string= col-name (car column))
             do (setf (elt column 1) width))))

(defun sanityinc/maybe-widen-package-menu-columns ()
  "Widen some columns of the package menu table to avoid truncation."
  (when (boundp 'tabulated-list-format)
    (sanityinc/set-tabulated-list-column-width "Version" 13)
    (let ((longest-archive-name (apply 'max (mapcar 'length (mapcar 'car package-archives)))))
      (sanityinc/set-tabulated-list-column-width "Archive" longest-archive-name))))

(add-hook 'package-menu-mode-hook 'sanityinc/maybe-widen-package-menu-columns)


(provide 'init-elpa)
;;; init-elpa.el ends here
