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

(defun gaeric/gptel-send ()
  (interactive)
  (move-end-of-line nil)
  (insert (if use-hard-newlines hard-newline "\n"))
  (gptel-send))

(when (macrop 'gaeric-comma-leader-def)
  (gaeric-comma-leader-def
    "ag" 'gptel
    "ac" 'gaeric/gptel-send))

(setq ds-api-key-file "~/.emacs.d/ai/ds-key")
(setq ds-api-key (gaeric/ai-read-api ds-api-key-file))

(gptel-make-openai "deepseek"
  :host "api.deepseek.com"
  :endpoint "/v1/chat/completions"
  :stream t
  :key ds-api-key
  :models '((deepseek-chat
             :description "deepseek v3"
             :capabilities (tool-use)
             :context-window 128)
            (deepseek-reasoner
             :description "deepseek r1"
             :capabilities (tool-use reasoning)
             :context-window 128)))

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

;; (when (require-package 'ai-code)
;;   (with-eval-after-load 'ai-code
;;     (ai-code-set-backend 'opencode))
;;   (setq ai-code-auto-test-type 'ask-me)


;;   (when (macrop 'gaeric-comma-leader-def)
;;     (gaeric-comma-leader-def
;;       ;; use consult-ripgrep grep at current-dir
;;       "am" 'ai-code-menu))
;;   )


;; eca config eg.
;; {
;;     "providers": {
;;         "local": {
;;             "api": "openai-chat",
;;             "url": "http://192.168.65.232:4000/v1",
;;             "key": "xx",
;;             "models": {}
;;         },
;;         "deepseek": {
;;           "api": "openai-chat",
;;           "url": "https://api.deepseek.com",
;;           "models": {
;;             "deepseek-chat": {}
;;           },
;;           "key": "xx"
;;         }
;;     },
;;     "$schema": "https://eca.dev/config.json"
;; }

;; ECA provider failures may stem from build env issues;
;; self-compiled eca binary has no such problem.
;; Deps: JDK 21+, Clojure CLI, Babashka >= 0.8.156
;; Build: bb debug-cli | bb prod-cli | bb native-cli
(when (require-package 'eca)
  ;; (if (file-exists-p "~/.emacs.d/eca/eca")
  ;;     (setq eca-custom-command (list "~/.emacs.d/eca/eca" "server" "--log-level debug")))
  (if (file-exists-p "~/data/packages/eca/eca")
      (setq eca-custom-command (list "~/data/packages/eca/eca" "server" "--log-level debug")))
  (when (macrop 'gaeric-comma-leader-def)
    (gaeric-comma-leader-def
      "am" 'eca)))

(provide 'init-ai)
