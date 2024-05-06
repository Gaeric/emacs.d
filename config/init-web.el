;; init-web.el -*- coding: utf-8; lexical-binding: t; -*-
;; Web Template Editing Mode
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;; @see http://web-mode.org
;; License: GPLv3

(require-package 'web-mode)
(require-package 'emmet-mode)

(defun spacemacs/emmet-expand ()
  (interactive)
  (unless (if (bound-and-true-p yas-minor-mode)
              (call-interactively 'emmet-expand-yas)
            (call-interactively 'emmet-expand-line))
    (indent-for-tab-command)))

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(setq web-mode-engines-alist
      '(("django"    . "\\.html\\'")
        ("vue"    . "\\.vue\\'")))

;; Make Electric-Pair mode and web-mode both work well
;; See: https://github.com/fxbois/web-mode/issues/275
(add-hook
 'web-mode-hook
 #'(lambda ()
     (setq-local electric-pair-pairs
                 (append '((?' . ?')) electric-pair-pairs))
     (setq-local electric-pair-text-pairs
                 (append '((?' . ?')) electric-pair-text-pairs))))

(with-eval-after-load 'web-mode
  ;; make org-mode export fail, I use evil and evil-matchit
  ;; to select text, so expand-region.el is not used
  (remove-hook 'web-mode-hook 'er/add-web-mode-expansions)
  (setq web-mode-enable-auto-closing t) ; enable auto close tag in text-mode
  (setq-default web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-imenu-regexp-list
        '(("<\\(h[1-9]\\)\\([^>]*\\)>\\([^<]*\\)" 1 3 ">" nil)
          ("^[ \t]*<\\([@a-z]+\\)[^>]*>? *$" 1 " id=\"\\([a-zA-Z0-9_]+\\)\"" "#" ">")
          ("^[ \t]*<\\(@[a-z.]+\\)[^>]*>? *$" 1 " contentId=\"\\([a-zA-Z0-9_]+\\)\"" "=" ">")
          ;; angular imenu
          (" \\(ng-[a-z]*\\)=\"\\([^\"]+\\)" 1 2 "="))))

(dolist (hook
         '(css-mode-hook
           html-mode-hook
           web-mode-hook))
  (add-hook hook 'emmet-mode))

(with-eval-after-load 'emmet-mode
  (evil-define-key 'insert emmet-mode-keymap (kbd "TAB") 'spacemacs/emmet-expand)
  (evil-define-key 'insert emmet-mode-keymap (kbd "<tab>") 'spacemacs/emmet-expand))


;;; JS/TS config
(when (maybe-require-package 'prettier-js)
  (with-eval-after-load 'prettier-js
    (diminish 'prettier-js-mode)))

(defvar webide-package 'lsp "tide/lsp")

(defun setup-webide-mode ()
  (interactive)
  (if (eq webide-package 'lsp)
      (progn
        (setq typescript-ts-mode-indent-offset 4)
        nil)
    (require-package 'tide)
    (tide-setup)
    (flycheck-mode +1)
    (setq js-indent-level 2)
    (setq typescript-indent-level 2)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))

  (prettier-js-mode)
  (eldoc-mode))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode))

;;; lsp is slowly
;; @https://github.com/ananthakumaran/tide/
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (or 
                   (string-equal "tsx" (file-name-extension buffer-file-name))
                   (string-equal "jsx" (file-name-extension buffer-file-name)))
              (setup-webide-mode))))

(add-hook 'typescript-ts-mode-hook 'setup-webide-mode)
(add-hook 'js-ts-mode-hook 'setup-webide-mode)

(provide 'init-web)
;;; init-web.el ends here
