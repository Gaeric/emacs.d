;; init-ai.el -*- coding: utf-8; lexical-binding: t; -*-
;; 
;; Author:  <Gaeric>
;; URL: https://github.com/Gaeric
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;; (require 'mind-wave)

(maybe-require-package 'gptel)

(setq gpt-api-key-file "~/.emacs.d/ai/gpt-key")
(setq xai-api-key-file "~/.emacs.d/ai/xai-key")

;; local config
;; (setq gptel-proxy "http://127.0.0.1:20171")
;; (setq gptel-model 'gpt-4o-mini)

(add-hook 'gptel-post-response-functions 'gptel-end-of-response)
(setq gptel-default-mode 'org-mode)

(defun gaeric/ai-read-api (key-file)
  (let (api-key)
    (if (file-exists-p key-file)
        (with-temp-buffer
          "*ai-api-key*"
          (insert-file-contents key-file)
          (setq api-key (buffer-string))))
    api-key))

(setq gptel-api-key (gaeric/ai-read-api gpt-api-key-file))
(setq xai-api-key (gaeric/ai-read-api xai-api-key-file))

;; (setq gptel-model 'grok-3-mini-latest
;;       gptel-backend
;;       (gptel-make-xai "xAI"
;;         :key xai-api-key
;;         :stream t))


(defun gaeric/gptel-send ()
  (interactive)
  (move-end-of-line nil)
  (insert (if use-hard-newlines hard-newline "\n"))
  (gptel-send))

(when (macrop 'gaeric-space-leader-def)
  (gaeric-space-leader-def
    "ag" 'gptel
    )
  )

(when (macrop 'gaeric-comma-leader-def)
  (gaeric-comma-leader-def
    "ac" 'gaeric/gptel-send
    )
  )

;; (defun gaeric/gptel-curl--get-args (info token)
;;   "Produce list of arguments for calling Curl.

;; REQUEST-DATA is the data to send, TOKEN is a unique identifier."
;;   (let* ((data (plist-get info :data))
;;          ;; We have to let-bind the following two variables since their dynamic
;;          ;; values are used for key lookup and url resoloution
;;          (gptel-backend (plist-get info :backend))
;;          (gptel-stream (plist-get info :stream))
;;          (url (let ((backend-url (gptel-backend-url gptel-backend)))
;;                 (if (functionp backend-url)
;;                     (funcall backend-url) backend-url)))
;;          (data-json (encode-coding-string (gptel--json-encode data) 'utf-8))
;;          (headers
;;           (append '(("Content-Type" . "application/json"))
;;                   (when-let* ((header (gptel-backend-header gptel-backend)))
;;                     (if (functionp header)
;;                         (funcall header) header)))))
;;     (when gptel-log-level
;;       (when (eq gptel-log-level 'debug)
;;         (gptel--log (gptel--json-encode
;;                      (mapcar (lambda (pair) (cons (intern (car pair)) (cdr pair)))
;;                              headers))
;;                     "request headers"))
;;       (gptel--log data-json "request body"))
;;     (append
;;      gptel-curl--common-args
;;      (gptel-backend-curl-args gptel-backend)
;;      (list (format "-w(%s . %%{size_header})" token))
;;      (if (length< data-json gptel-curl-file-size-threshold)
;;          (list (format "-d%s" data-json))
;;        (letrec
;;            ((temp-filename (make-temp-file "gptel-curl-data" nil ".json" data-json))
;;             (cleanup-fn (lambda (&rest _)
;;                           (when (file-exists-p temp-filename)
;;                             (delete-file temp-filename)
;;                             (remove-hook 'gptel-post-response-functions cleanup-fn)))))
;;          (add-hook 'gptel-post-response-functions cleanup-fn)
;;          (list "--data-binary"
;;                (format "@%s" temp-filename))))
;;      (when (not (string-empty-p gptel-proxy))
;;        (list "--proxy" gptel-proxy))
;;      (cl-loop for (key . val) in headers
;;               collect (format "-H%s: %s" key val))
;;      (list url))))

;; (advice-add #'gptel-curl--get-args :override #'gaeric/gptel-curl--get-args)

(provide 'init-ai)
