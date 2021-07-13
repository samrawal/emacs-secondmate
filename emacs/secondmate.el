(require 'url)
(require 'json)

(defun secondmate ()
  (interactive)
  ;; TODO: what is best way to do it -- backward-paragraph or
  ;; previous-line?
  (let* ((context-beg (save-excursion
                        (previous-line (min 3 (- (line-number-at-pos) 1)))
                        (beginning-of-line)
                        (point)))
         (context-end (point))
         (context (buffer-substring-no-properties context-beg context-end))
         (params (url-build-query-string `(("text" ,context))))
         (url (format "http://localhost:9900/?%s" params))
         (url-buf (url-retrieve-synchronously url))
         (old-buf (current-buffer)))
    (unwind-protect
      (with-current-buffer url-buf
        (goto-char url-http-end-of-headers)
        (let ((generation (cdr (assoc 'generation (json-read)))))
          (with-current-buffer old-buf
            (insert generation))))
      (kill-buffer url-buf))))

(defun secondmate-redo ()
  (interactive)
  (undo)
  (secondmate))

(global-set-key (kbd "C-c c") 'secondmate)
(global-set-key (kbd "C-c v") 'secondmate-redo)
