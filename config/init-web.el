;; init-web.el -*- coding: utf-8; lexical-binding: t; -*-
;; Web Template Editing Mode
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;; @see http://web-mode.org
;; License: GPLv3

(local-require 'web-mode)
(local-require 'emmet-mode)

(defun spacemacs/emmet-expand ()
  (interactive)
  (unless (if (bound-and-true-p yas-minor-mode)
              (call-interactively 'emmet-expand-yas)
            (call-interactively 'emmet-expand-line))
    (indent-for-tab-command)))

(when (maybe-require-package 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (setq web-mode-engines-alist
        '(("django"    . "\\.html\\'")))

  ;; Make Electric-Pair mode and web-mode both work well
  ;; See: https://github.com/fxbois/web-mode/issues/275
  (add-hook
   'web-mode-hook
   '(lambda ()
      (setq-local electric-pair-inhibit-predicate
                  (lambda (c)
                    (if (char-equal c ?{) t (electric-pair-default-inhibit c))))))


  (when (maybe-require-package 'emmet-mode)
    (dolist (hook
             '(css-mode-hook
               html-mode-hook
               web-mode-hook))
      (add-hook hook 'emmet-mode))

    (with-eval-after-load 'emmet-mode
      (evil-define-key 'insert emmet-mode-keymap (kbd "TAB") 'spacemacs/emmet-expand)
      (evil-define-key 'insert emmet-mode-keymap (kbd "<tab>") 'spacemacs/emmet-expand))))

(provide 'init-web)
