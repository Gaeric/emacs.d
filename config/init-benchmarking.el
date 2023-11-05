;;; init-benchmarking.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun sanityinc/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))


(defvar sanityinc/require-times nil
  "A list of (FEATURE LOAD-START-TIME LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defun sanityinc/require-times-wrapper (orig feature &rest args)
  "Note in `sanityinc/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        (apply orig feature args)
      (when (and (not already-loaded) (memq feature features))
        (let ((time (sanityinc/time-subtract-millis (current-time) require-start-time)))
          (add-to-list 'sanityinc/require-times
                       (list feature require-start-time time)
                       t))))))

(advice-add 'require :around 'sanityinc/require-times-wrapper)


(define-derived-mode sanityinc/require-times-mode tabulated-list-mode "Require-Times"
  "Show times taken to `require' packages."
  (setq tabulated-list-format
        [("Start time (ms)" 20 sanityinc/require-times-sort-by-start-time-pred)
         ("Feature" 30 t)
         ("Time (ms)" 12 sanityinc/require-times-sort-by-load-time-pred)])
  (setq tabulated-list-sort-key (cons "Start time (ms)" nil))
  ;; (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'sanityinc/require-times-tabulated-list-entries)
  (tabulated-list-init-header)
  (when (fboundp 'tablist-minor-mode)
    (tablist-minor-mode)))

(defun sanityinc/require-times-sort-by-start-time-pred (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))

(defun sanityinc/require-times-sort-by-load-time-pred (entry1 entry2)
  (> (string-to-number (elt (nth 1 entry1) 2))
     (string-to-number (elt (nth 1 entry2) 2))))

(defun sanityinc/require-times-tabulated-list-entries ()
  (cl-loop for (feature start-time millis) in sanityinc/require-times
           with order = 0
           do (incf order)
           collect (list order
                         (vector
                          (format "%.3f" (sanityinc/time-subtract-millis start-time before-init-time))
                          (symbol-name feature)
                          (format "%.3f" millis)))))

(defun sanityinc/require-times ()
  "Show a tabular view of how long various libraries took to load."
  (interactive)
  (with-current-buffer (get-buffer-create "*Require Times*")
    (sanityinc/require-times-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))




(defun sanityinc/show-init-time ()
  (message "init completed in %.2fms"
           (sanityinc/time-subtract-millis after-init-time before-init-time)))

(add-hook 'after-init-hook 'sanityinc/show-init-time)


(defun gaeric/log-time (msg)
  (message "%s: %s" msg (format-time-string "%s.%3N")))


(defun gaeric/profiler-process (func)
  (insert (format ";; %s start\n" (gaeric/log-time (identity func))))
  (with-temp-buffer (funcall func))
  (insert (format ";; %s end\n" (gaeric/log-time (identity func)))))


(require 'gnus-util)

(setq gaeric/profiler-buffer "*gaeric-profiler*")
(setq gaeric/msg-tick-start 0)

(defun gaeric/profiler-init ()
  (unless (gnus-buffer-live-p gaeric/profiler-buffer)
    (generate-new-buffer gaeric/profiler-buffer)))

(defun gaeric/profiler-record (msg)
  (with-current-buffer gaeric/profiler-buffer
    (goto-char (point-max))
    (insert msg)))

(defun gaeric/msg-start (&rest args)
  (gaeric/profiler-init)
  (setq gaeric/msg-tick-start (current-time)))

(defun gaeric/msg-end (&rest args)
  (let ((escape-time (* 1000 (float-time (time-subtract (current-time) gaeric/msg-tick-start)))))
    (gaeric/profiler-record
     (format " take: %f ms %s\n" escape-time
             (if (> escape-time 1) "++" "")))))

(dolist (symbol '(jsonrpc--log-event
                  eglot-completion-at-point
                  ))
  (advice-add symbol :before `(lambda (&rest args)
                                (gaeric/msg-start)
                                (gaeric/profiler-record (format "%s" ',symbol))))
  (advice-add symbol :after 'gaeric/msg-end))

(provide 'init-benchmarking)
;;; init-benchmarking.el ends here
