;;; lk/helm-terminals.el -- fast navigation for terminal buffers

(defun only-terminal-buffers ()
  (let ((buffers (remove-if-not (lambda (buffer)
                                  (string-prefix-p "*term:" (buffer-name buffer)))
                                (buffer-list))))
    (mapcar (lambda (buffer) (buffer-name buffer))
            buffers)))



(defun helm-select-terminal-buffer ()
  (interactive)
  (helm :sources
        (helm-build-sync-source "terminal-buffers"
          :candidates (only-terminal-buffers)
          :action #'switch-to-buffer
          :fuzzy-match t)
        :buffer "**helm-terminal-buffers**"))

(provide 'lk/helm-terminals)
