;; init-org.el --- org init -*- coding: utf-8; lexical-binding: t; -*-
;; Author:  <lantian@ZERO>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; (when (maybe-require-package 'org-pomodoro)
;;   (setq org-pomodoro-format "%s"))

(with-eval-after-load 'org
  (message "load org after org file")
  ;; ----------------------------------------------------------------------
  ;; 配置org-emphasis，优化org-mode中文体验
  ;; ----------------------------------------------------------------------
  (setq org-emphasis-regexp-components
        ;; add multibyte char at pre and post For chinese
        '("-[:space:][:multibyte:]('\"{" "-[:space:][:multibyte:].,:!?;'\")}\\[" "[:space:]" "." 1))
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  ;; (org-element-update-syntax)
  ;; Non-nil means interpret "_" and "^" for display.
  ;; 适用于导出
  (setq org-export-with-sub-superscripts '{})
  ;; 适用于org-mode中渲染
  ;; (setq org-use-sub-superscripts '{})
  ;; ----------------------------------------------------------------------
  ;; 配置org-mode 标题样式
  ;; ----------------------------------------------------------------------
  ;; here goes your Org config :)
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; '(org-block ((t (:background "#282C34" :foreground "aquamarine1" :slant italic))))
   ;; '(org-block-begin-line ((t (:background "#282C34" :foreground "#5B6268"))))
   '(org-document-title ((t (:foreground "gold1" :height 1.2))))
   '(org-level-1 ((t (:foreground "DodgerBlue1"  :weight normal  :height 1.0  :blod nil))))
   '(org-level-2 ((t (:foreground "SteelBlue1"   :weight normal   :height 1.0   :bold nil))))
   '(org-level-3 ((t (:foreground "DeepSkyBlue1" :weight normal :height 1.0 :bold nil))))
   '(org-level-4 ((t (:foreground "SkyBlue1"))))
   '(org-level-5 ((t (:foreground "MediumOrchid1"))))
   '(org-level-6 ((t (:foreground "hot pink"))))
   '(org-level-7 ((t (:foreground "magenta"))))
   '(org-level-8 ((t (:foreground "deep pink")))))

  ;; 导出的源代码内容使用单独的css文件控制样式
  (setq org-html-htmlize-output-type 'css)
  ;; 不生成默认的css及javascript内容
  (setq org-html-head-include-default-style nil)
  (setq org-html-head-include-scripts nil)

  (setq org-publish-project-alist
        '(
          ;; ... add all the components here (see below)...
          ("pre_pub"
           :base-directory "~/org/blog" ;; org文件的目录
           :base-extension "png\\|org"
           :publishing-directory "~/py_projects/org_blog/static/static_html" ;导出目录
           :publishing-function org-html-publish-to-html
           ;; :auto-sitemap t
           )))

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                (sequence "WAITING(w@/!)" "SOMEDAY(S@/!)" "|" "CANCELLED(c@/!)" ))))

  ;; Change task state to STARTED when clocking in
  (setq org-clock-in-switch-to-state "STARTED")
  
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline "~/org/inbox.org" "Workspace")
           "* TODO [#B] %?\n  %i\n")
          ("n" "notes" entry
           (file+headline "~/org/inbox.org" "notes")
           "* %?\n  %i\n %U")
          ("l" "links" entry
           (file+headline "~/org/inbox.org" "Links")
           "* TODO [#C] %?\n  %i\n %a \n %U")
          ("a" "Amusement" entry
           (file+headline "~/org/gtd.org" "Amusement")
           "* TODO [#C] %?\n  %i\n %U")
          ("e" "Excerpt" entry
           (file+headline "~/org/inbox.org" "Excerpt")
           "* %?"))))


(defvar gaeric-static-file-dir "")
(defun gaeric-static-blog-change-link (path desc backend)
  "change link for static file"
  (if (eq backend 'html)
      (unless desc
        (message "baspath is %s" (expand-file-name path))
        (if (file-exists-p path)
            (let* ((file-abs (expand-file-name path))
                   (file-md5 (substring-no-properties (md5 file-abs) 0 8))
                   (file-new (concat (file-name-base file-abs) "_" file-md5 "." (file-name-extension file-abs))))
              (if (file-exists-p (concat gaeric-static-file-dir file-new))
                  (message "Skipping unmodified file %s" file-abs)
                (copy-file file-abs file-new))
              (format "<img src=\"%s\" alt=\"%s\"/>" file-new file-new))))))


(with-eval-after-load 'org-agenda
  (setq org-agenda-custom-commands
        '(("w" . "任务安排")
          ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
          ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
          ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
          ;;("p" . "项目安排")
          ("W" "Weekly Review")))

  (setq org-agenda-files
        (quote ("~/org/inbox.org" "~/org/gtd.org"))))

;; `org-capture' is global
(gaeric-space-leader-def
  "aoc" 'org-capture
  "aoo" 'org-agenda)

(gaeric-comma-leader-def
    :keymap 'org-mode-map
    "op" 'org-pomodoro
    "cp" 'org-previous-visible-heading
    "cn" 'org-next-visible-heading
    "cf" 'org-forward-heading-same-level
    "cb" 'org-backward-heading-same-level
    "ns" 'org-narrow-to-subtree
    "nb" 'org-narrow-to-block
    "ne" 'org-narrow-to-element)

(provide 'init-org)