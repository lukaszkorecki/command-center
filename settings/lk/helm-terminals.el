;;; lk/helm-terminals.el -- fast navigation for terminal buffers

(defun only-terminal-buffers ()
  (let ((buffers (remove-if-not (lambda (buffer)
                                  (string-prefix-p "*term:" (buffer-name buffer)))
                                (buffer-list))))
    (message buffers)
    (mapc buffer-name buffers)))

(setq helm-terminal-buffers-source
      '( (name . "Terminals" )
         (candidates . only-terminal-buffers)
         (action . (lambda (candidate)
                     (switch-to-buffer candidate)))))

(defun helm-terminals ()
  (interactive)
  (helm :sources   '(helm-terminal-buffers-source)))


(provide 'lk/helm-terminals)
