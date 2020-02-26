;; -*- coding: utf-8; lexical-binding: t; -*-


(local-require 'evil)
(evil-mode 1)
(evil-declare-key 'normal org-mode-map
  (kbd "TAB") 'org-cycle)

(provide 'init-evil)
