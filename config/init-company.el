;; init-company.el  -*- coding: utf-8; lexical-binding: t; -*-
;; Modular in-buffer completion framework for Emacs
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(require-package 'company)
;; need pos-tip
;; for document info
(require-package 'company-quickhelp)

;; <TAB> just use for indent
;; (setq tab-always-indent 'complete)

(add-hook 'emacs-startup-hook
          (lambda ()
            (progn
              (global-company-mode)
              (company-quickhelp-mode))))

(with-eval-after-load 'company
  ;; company-eclim for Eclipse
  ;; semantic use for CEDET semantic
  (dolist (backend '(company-eclim company-semantic))
    (delq backend company-backends))
  (diminish 'company-mode)
  (define-key company-mode-map (kbd "M-/") 'company-complete)
  (define-key company-active-map (kbd "M-/") 'company-other-backend)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "M-j") 'company-select-next)
  (define-key company-active-map (kbd "M-k") 'company-select-previous)
  (define-key company-search-map (kbd "M-n") nil)
  (define-key company-search-map (kbd "M-p") nil)
  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        company-idle-delay .1))

;; TODO page-break-lines-mode


(provide 'init-company)
