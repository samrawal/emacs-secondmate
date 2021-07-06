; The only reason this is being passed through Python because
; I haven't been able to get HTTP APIs working in elisp. 

(set 'pypath "/usr/local/bin/python3")
(set 'scriptpath "/PATH/TO/secondmate.py")

(defun secondmate ()
  (interactive)
  ; grab context (TODO: what is best way to do it -- backward-paragraph or previous-line?)
  (set 'endcontext (point))
  (previous-line (min 3 (- (line-number-at-pos) 1)))
  (beginning-of-line)
  (set 'startcontext (point))
  (copy-region-as-kill startcontext endcontext)

  (set 'codesample (concat "\'" (car kill-ring) " \'"))
  (set 'fullcommand (concat pypath " " scriptpath " " codesample))
  (goto-char endcontext)

  (insert (shell-command-to-string fullcommand))
)


(global-set-key (kbd "C-c c") 'secondmate)
