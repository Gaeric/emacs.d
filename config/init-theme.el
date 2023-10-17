;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;; Author:  Gaeric
;; URL: https://github.com/Gaeric/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(require-package 'color-theme-sanityinc-solarized)
(require-package 'color-theme-sanityinc-tomorrow)

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))
;; (setq-default custom-enabled-themes '(sanityinc-tomorrow-day))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (reapply-themes))


;; -------------------------------------------------------------------
;; circadian load the theme record time
;; -------------------------------------------------------------------
;; (require-package 'circadian)
;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.

;; Only Setting Fonts for GUI
(when (display-graphic-p)
  ;; Setting English Font
  (if (eq system-type 'windows-nt)
      (progn
        (set-face-attribute
         'default nil :family "Consolas" :height 110)
        ;; Setting Chinese Font
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "微软雅黑" :size 14))))
    (set-face-attribute
     'default nil :family "Monospace" :height 105)
    ;; Setting Chinese Font
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        (font-spec :family "WenQuanYi Micro Hei Mono" :size 13)))))

;; (require-package 'hide-mode-line)
;; (add-hook 'after-init-hook
;;           (global-hide-mode-line-mode))

(provide 'init-theme)
