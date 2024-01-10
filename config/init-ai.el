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

(setq gpt-api-key-file "~/.emacs.d/gpt/api-key")
(setq gptel-proxy "http://127.0.0.1:20171")
(setq gptel-model "gpt-4")

(if (file-exists-p gpt-api-key-file)
    (with-temp-buffer "*gpt-api-key*"
                      (insert-file-contents gpt-api-key-file)
                      (setq gptel-api-key (buffer-string))))

(provide 'init-ai)
