;; init-complete.el  -*- coding: utf-8; lexical-binding: t; -*-
;; Modular in-buffer completion framework for Emacs
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;; (with-eval-after-load 'company
;;   ;; company-eclim for Eclipse
;;   ;; semantic use for CEDET semantic
;;   (dolist (backend '(company-eclim company-semantic))
;;     (delq backend company-backends))
;;   (diminish 'company-mode)
;;   (define-key company-mode-map (kbd "M-/") 'company-complete)
;;   (define-key company-active-map (kbd "M-/") 'company-other-backend)
;;   (define-key company-active-map (kbd "M-n") nil)
;;   (define-key company-active-map (kbd "M-p") nil)
;;   (define-key company-active-map (kbd "M-j") 'company-select-next)
;;   (define-key company-active-map (kbd "M-k") 'company-select-previous)
;;   (define-key company-search-map (kbd "M-n") nil)
;;   (define-key company-search-map (kbd "M-p") nil)
;;   (setq company-tooltip-align-annotations t
;;    company-minimum-prefix-length 2
;;    company-idle-delay .1))

;; (require-package 'company-quickhelp)

;; (add-hook 'company-mode-hook
;;           (company-quickhelp-local-mode))

;; ;; <TAB> just use for indent
;; ;; (setq tab-always-indent 'complete)

;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (progn
;;               (global-company-mode)
;;               (company-quickhelp-mode))))

(when (maybe-require-package 'corfu)
  (setq-default corfu-auto t)
  (with-eval-after-load 'eshell
    (add-hook 'eshell-mode-hook (lambda () (setq-local corfu-auto nil))))
  (setq-default corfu-quit-no-match 'separator)

  (add-hook 'after-init-hook 'global-corfu-mode)
  (when (display-graphic-p)
    (with-eval-after-load 'corfu
      (corfu-popupinfo-mode)))

  (unless (display-graphic-p)
    (require-package 'corfu-terminal)
    (corfu-terminal-mode +1)))


(when (maybe-require-package 'cape)
  (add-to-list 'completion-at-point-functions #'cape-file))

(provide 'init-complete)
