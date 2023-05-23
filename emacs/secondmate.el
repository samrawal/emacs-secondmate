(require 'json)
(require 'url)
(require 'url-parse)

(defgroup secondmate nil
  "Perform GPT-powered code completion"
  :group 'editing)

(defcustom secondmate-url "http://localhost:9900/"
  "URL the Python server is running at."
  :type 'string
  :group 'secondmate)

(defun secondmate--url (context)
  (let ((url (url-generic-parse-url secondmate-url))
        (params (url-build-query-string `(("text" ,context)))))
    (setf (url-filename url) (format "%s/?%s" (url-filename url) params))
    url))

(defun secondmate ()
  (interactive)
  ;; TODO: what is best way to do it -- backward-paragraph or
  ;; previous-line?
  (let* ((context-beg (or (use-region-beginning)
                          (save-excursion
                            (forward-line (- (min 20 (- (line-number-at-pos) 1))))
                            (beginning-of-line)
                            (point))))
         (context-end (or (use-region-end) (point)))
         (context (buffer-substring-no-properties context-beg context-end))
         (url (secondmate--url context))
         (url-buf (url-retrieve-synchronously url))
         (old-buf (current-buffer)))
    (unwind-protect
      (with-current-buffer url-buf
        (goto-char url-http-end-of-headers)
        (let ((generation (cdr (assoc 'generation (json-read)))))
          (with-current-buffer old-buf
            (insert generation))))
      (kill-buffer url-buf)))
  (message "Inserted completion."))

(defun secondmate-redo ()
  (interactive)
  (undo)
  (secondmate))

(global-set-key (kbd "C-c M-/") 'secondmate)
(global-set-key (kbd "C-c M-\\") 'secondmate-redo)

(provide 'secondmate)
