
(defun lk/fix-with-cljfmt-and-reload ()
  "Run rubocop -a and reloads the buffer."
  (interactive)
  (save-buffer)
  (start-process "cljfmt-fix"
                 (get-buffer-create "*clj-fmt-fix*")
                 "lein"
                 "cljfmt"
                 "fix"
                 buffer-file-name)
  (revert-buffer t t))
