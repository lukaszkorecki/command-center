;;; lk/helm-terminals.el -- fast navigation for terminal buffers

(defun lk/only-terminal-buffers ()
  (let ((buffers (remove-if-not (lambda (buffer)
                                  (with-current-buffer buffer (derived-mode-p 'term-mode)))
                                (buffer-list))))
    (mapcar (lambda (buffer) (buffer-name buffer))
            buffers)))

(defun helm-select-terminal-buffer ()
  (interactive)
  (helm :sources
        (helm-build-sync-source "terminal-buffers"
          :candidates (lk/only-terminal-buffers)
          :action #'switch-to-buffer
          :fuzzy-match t)
        :buffer "**helm-terminal-buffers**"))

(provide 'lk/helm-terminals)
