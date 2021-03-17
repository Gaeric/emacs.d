;; init-xref.el -*- coding: utf-8; lexical-binding: t; -*-
;; config xref for code navigation
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

(defun xref-show-definitions-ivy-read (fetcher alist)
  "Let the user choose the target definition with ivy-read."
  (let* ((xrefs (funcall fetcher))
         (xref-alist (xref--analyze xrefs))
         (orig-buf (current-buffer))
         (orig-pos (point))
         done
         xref-alist-with-line-info
         xref
         (group-prefix-length
          ;; FIXME: Groups are not always file names, but they often
          ;; are.  At least this shouldn't make the other kinds of
          ;; groups look worse.
          (let ((common-prefix (try-completion "" xref-alist)))
            (if (project-current)
                (length (file-truename (car (project-roots (project-current)))))
              (if (> (length common-prefix) 0)
                  (length (file-name-directory common-prefix))
                0)))))

    (cl-loop for ((group . xrefs) . more1) on xref-alist
             do
             (cl-loop for (xref . more2) on xrefs do
                      (with-slots (summary location) xref
                        (let* ((line (xref-location-line location))
                               (line-fmt
                                (if line
                                    (format #("%d:" 0 2 (face xref-line-number))
                                            line)
                                  ""))
                               (group-fmt
                                (propertize
                                 (substring group group-prefix-length)
                                 'face 'xref-file-header))
                               (candidate
                                (format "%s:%s%s" group-fmt line-fmt summary)))
                          (push (cons candidate xref) xref-alist-with-line-info)))))

    (if (not (cdr xrefs))
        (xref-pop-to-location xref (assoc-default 'display-action alist))
      (let* ((collection (reverse xref-alist-with-line-info))
             (ctable
              (lambda (string pred action)
                (cond
                 ((eq action 'metadata)
                  '(metadata . ((category . xref-location))))
                 (t
                  (complete-with-action action collection string pred)))))
             (def (caar collection)))
        (ivy-read "xref: "
                  ctable
                  :require-match t
                  :def def
                  :caller 'xref-ivy-read
                  :action (lambda (candidate)
                            (setq done (eq 'ivy-done this-command))
                            (xref-pop-to-location
                             (cdr (assoc candidate collection))
                             (assoc-default 'display-action alist)))
                  :unwind (lambda ()
                            (unless done
                              (switch-to-buffer orig-buf)
                              (goto-char orig-pos))))))))

;; TODO: xref-ivy-read occur

(with-eval-after-load 'xref
  (setq xref-search-program 'ripgrep)
  (setq xref-show-definitions-function 'xref-show-definitions-ivy-read)
  (setq xref-show-xrefs-function 'xref-show-definitions-ivy-read))

(when (macrop 'gaeric-comma-leader-def)
  (gaeric-comma-leader-def
    "gd" 'xref-find-definitions
    "gr" 'xref-find-references
    "go" 'xref-find-definitions-other-window))

(provide 'init-xref)
