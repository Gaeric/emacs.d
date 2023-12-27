;; init-org.el --- org init -*- coding: utf-8; lexical-binding: t; -*-
;; Author:  Gaeric
;; URL: https://github.com/Gaeric/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(when (display-graphic-p)
  (require-package 'org-download)
  ;; Drag-and-drop to `dired`
  ;; Use ImageMagaick convert on windows
  (add-hook 'org-mode-hook 'org-download-enable)
  (add-hook 'dired-mode-hook 'org-download-enable)

  (setq org-download-image-dir "d:/work_cloud/wiki/images")
  (defun org-download-clipboard-ms (&optional basename)
    "Capture the image from the clipboard and insert the resulting file."
    (interactive)
    (let ((org-download-screenshot-method
           (cl-case system-type
             (gnu/linux
              (if (string= "wayland" (getenv "XDG_SESSION_TYPE"))
                  (if (executable-find "wl-paste")
                      "wl-paste -t image/png > %s"
                    (user-error
                     "Please install the \"wl-paste\" program included in wl-clipboard"))
                (if (executable-find "xclip")
                    "xclip -selection clipboard -t image/png -o > %s"
                  (user-error
                   "Please install the \"xclip\" program"))))
             ((windows-nt cygwin) "magick clipboard: %s")
             ((darwin berkeley-unix)
              (if (executable-find "pngpaste")
                  "pngpaste %s"
                (user-error
                 "Please install the \"pngpaste\" program from Homebrew."))))))
      (org-download-screenshot basename)))

  (advice-add #'org-download-clipboard :override
              #'org-download-clipboard-ms))

(maybe-require-package 'htmlize)

(when (maybe-require-package 'toc-org)
  (add-hook 'org-mode-hook 'toc-org-mode))

(when (maybe-require-package  'org-bullets)
  ;; (setq org-bullets-bullet-list '("♠" "♥" "♣" "♦" "♪" "♫"))
  (setq org-bullets-bullet-list '("𝄞" "✶" "✡" "♫" "*"))

  (add-hook 'org-mode-hook (lambda ()
                             (save-place-local-mode)
                             (org-bullets-mode 1))))

(when (maybe-require-package 'org-pomodoro)
  (setq org-pomodoro-keep-killed-pomodoro-time t)
  (setq org-pomodoro-long-break-frequency 3)
  (setq org-pomodoro-format "Pomodoro %s")
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)
    (define-key org-agenda-mode-map (kbd "Z") 'org-agenda)))

(when (maybe-require-package 'org-roam)
  (setq org-roam-directory "~/org/roam/")
  (setq org-roam-v2-ack t))

(defun gaeric/publish-conf (&optional plist)
;; only use for org-download image
  (org-link-set-parameters "file" :export #'gaeric/org-export-link-static))

(defun gaeric/export-conf (&optional plist)
  ;; Non-nil means interpret "_" and "^" for display.
  ;; for export
  (setq org-export-with-sub-superscripts '{})
  ;; for render
  ;; (setq org-use-sub-superscripts '{})

  ;; 导出的源代码内容使用单独的css文件控制样式
  (setq org-html-htmlize-output-type 'css)
  ;; 不生成默认的css及javascript内容
  (setq org-html-head-include-default-style nil)
  (setq org-html-head-include-scripts nil)
  (setq org-publish-timestamp-directory
        (convert-standard-filename "~/.emacs.d/.org-timestamps/"))
  (org-link-set-parameters "file" :export #'gaeric/org-export-link-static))

(defvar gaeric/work-base "D:/work_cloud/")
(defvar gaeric/work-daily-dir (concat gaeric/work-base "daily_work/"))
(defvar gaeric/work-wiki-file (concat gaeric/work-base "wiki/wiki.org"))
(defvar gaeric/org-home "~/org/")
(defvar gaeric/org-gtd-file (concat gaeric/org-home "gtd.org"))
(defvar gaeric/org-inbox-file (concat gaeric/org-home "inbox.org"))

(defun gaeric/org-capture-work ()
  (org-back-to-heading t)
  (if (string-equal (plist-get org-capture-current-plist :description) "work")
      (let ((case-fold-search nil))
        (when (looking-at org-complex-heading-regexp)
          (let* ((headline (match-string-no-properties 4))
                 (filename (concat
                            (format-time-string "%Y%m%d") "_" headline))
                 (dir (concat
                       gaeric/work-daily-dir
                       filename)))
            (unless (file-exists-p dir)
              (dired-create-directory dir))
            (end-of-line)
            (newline-and-indent)
            (insert (format "[[file+sys:%s][%s]]"
                            (file-relative-name dir (file-name-directory gaeric/work-wiki-file))
                            headline)))))))

(add-hook 'org-capture-before-finalize-hook 'gaeric/org-capture-work)

(defun gaeric/org-pomodoro-todo-today ()
  "加入时间戳"
  (interactive)
  (org-back-to-heading t)
  (move-end-of-line nil)
  (newline-and-indent)
  ;; (org-remove-timestamp-with-keyword "XXX")
  (org-insert-time-stamp (org-current-time)))


(defun gaeric/org-state-change-timestamp-hook ()
  "依据pomodoro流程添加和删除时间戳"
  (cond ((equal org-state "READY")
         (gaeric/org-pomodoro-todo-today))))


(defun gaeric/get-org-link-ap ()
  "Get Org link at Point."
  (interactive)
  (when (org-in-regexp org-link-plain-re 1)
    (kill-ring-save (match-beginning 0) (match-end 0))))

(add-hook 'org-after-todo-state-change-hook 'gaeric/org-state-change-timestamp-hook)

(with-eval-after-load 'org
  ;; org-emphasis for chinese
  (setq org-emphasis-regexp-components
        ;; add multibyte char at pre and post For chinese
        '("-[:space:][:multibyte:]('\"{" "-[:space:][:multibyte:].,:!?;'\")}\\[" "[:space:]" "." 1))
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  ;; (org-element-update-syntax)
  (gaeric/export-conf)

  (setq org-fontify-done-headline t)
  (setq org-ascii-text-width 65536)
  (setq org-adapt-indentation t)

  (setq org-publish-project-alist
        '(
          ;; ... add all the components here (see below)...
          ("pre_pub"
           :base-directory "~/org/blog" ;; org文件的目录
           :base-extension "png\\|org"
           :publishing-directory "~/project_py/org_blog/static/static_html" ;导出目录
           :publishing-function org-html-publish-to-html
           :preparation-function gaeric/publish-conf
           :completion-function gaeric/export-conf
           ;; :auto-sitemap t
           )))

  ;; org-src-edit时打开新窗口，完成后恢复布局
  (setq org-src-window-setup 'other-window)
  ;; 将org-clock日志写在LOGBOOK中
  (setq org-log-into-drawer t)

  ;; TODO: 待处理的事务
  ;; STARTED: 事务进行中
  ;; PEND: 事务挂起
  ;; CANCELLED: 已弃置处理的事务
  ;; DONE: 已处理完成的事务
  (setq org-todo-keywords
        '((sequence
           "TODO(t)" "READY(r)" "STARTED(s!)" "PEND(w@)" "|" "DONE(d@)" "CANCELLED(c@/@)")))

  (setq org-clock-in-switch-to-state "STARTED")
  (setq org-clock-out-switch-to-state "PEND")
  
  (setq org-capture-templates
        '(
          ("w" "work" entry
           (file+headline gaeric/work-wiki-file "DailyRecord")
           "* TODO [#B] %?\n  %i\n")
          ("R" "Report" plain
           (file+olp+datetree gaeric/work-wiki-file "Report")
           "%?\n %i\n  %U"
           :time-prompt t :tree-type week)
          ("t" "Todo" entry
           (file+headline gaeric/org-gtd-file "Workspace")
           "* TODO [#B] %?\n  %i\n")
          ("n" "notes" entry
           (file+headline gaeric/org-inbox-file "notes")
           "* %?\n  %i\n %U")
          ("r" "reviews" entry
           (file+olp+datetree gaeric/org-inbox-file "reviews")
           "* review\n   %?%i\n  %U"
           :time-prompt t :tree-type week)
          ("b" "Breakpoint" entry (clock)
           "* TODO %? :BreakPoint:\n  %U\n  %i")
          ("e" "Excerpt" entry
           (file+headline gaeric/org-inbox-file "Excerpt")
           "* %?"))))
;; -------------------------------------------------------------------
;; for blog static file
;; -------------------------------------------------------------------
(defvar gaeric-static-file-dir "")
(defun gaeric/org-export-link-static (path desc backend &optional info)
  "change link for static file"
  (if (eq backend 'html)
      (unless desc
        (message "abspath is %s" (expand-file-name path))
        (if (file-exists-p path)
            (let* ((file-abs (expand-file-name path))
                   (file-md5 (substring-no-properties (md5 file-abs) 0 8))
                   (file-new (concat (file-name-base file-abs) "_" file-md5 "." (file-name-extension file-abs))))
              (if (file-exists-p (concat gaeric-static-file-dir file-new))
                  (message "Skipping unmodified file %s" file-abs)
                (copy-file file-abs file-new))
              (format "<img src=\"%s\" alt=\"%s\"/>" file-new file-new))))))

;; /usr/share/emacs/28.0.50/lisp/org/ox-html.el
;; 3117:      ((org-export-custom-protocol-maybe link desc 'html))

(defun gaeric/html-base64-image (image)
  "Encode the image to base64, and concat to a Data URLs"
  (let* ((suffix (ffap-file-suffix image))
         (type (cond
                ((string= suffix ".jpg") "jpeg")
                ((string= suffix ".jpeg") "jpeg")
                ((string= suffix ".png") "png")
                ((string= suffix ".gif") "gif")
                (t nil))))
    (if (and type (file-exists-p image))
        (let ((base64 (with-temp-buffer
                        (insert-file-contents image)
                        (base64-encode-region (point-min) (point-max))
                        (buffer-substring-no-properties (point-min) (point-max)))))
          (format "data:image/%s;base64,%s" type base64)))))

(defun gaeric/org-export-img-base64 (path desc backend &optional info)
  "Export img to base64 then insert the file"
  (let ((source (gaeric/html-base64-image path)))
    (if (and (plist-get info :html-inline-images) source)
        (org-html--format-image source nil info))))

(with-eval-after-load 'org-agenda
  (setq org-agenda-span 'day)
  (setq org-agenda-custom-commands
        '(("w" . "任务安排")
          ("wa" "重要紧急" tags-todo "+PRIORITY=\"A\"")
          ("wb" "重要不紧急" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
          ("wc" "不重要紧急" tags-todo "+PRIORITY=\"C\"")
          ("W" "Weekly Review")))

  (setq org-agenda-files
        (quote ("~/org/inbox.org" "~/org/gtd.org"))))

;; `org-capture' is global
(when (macrop 'gaeric-space-leader-def)
  (gaeric-space-leader-def
    "oc" 'org-capture
    "oo" 'org-agenda
    "oa" 'org-agenda-list
    "of" 'org-roam-node-find
    ))

(when (macrop 'gaeric-comma-leader-def)
  (gaeric-comma-leader-def
    :keymaps 'org-mode-map
    "op" 'org-pomodoro
    "oa" 'org-open-at-point
    "od" 'org-download-clipboard
    "cp" 'org-previous-visible-heading
    "cn" 'org-next-visible-heading
    "ns" 'org-narrow-to-subtree
    "nb" 'org-narrow-to-block
    "ne" 'org-narrow-to-element
    "c," 'org-insert-structure-template
    "'"  'org-edit-special)

  (gaeric-comma-leader-def
    :keymaps 'org-src-mode-map
    "c'" 'org-edit-src-exit
    "ck" 'org-edit-src-abort))

(provide 'init-org)
