;;; lk/helm-erc-channel.el -- fast navigation for erc channels

(defun only-erc-channel-buffers ()
  (mapcar (lambda (buffer) (buffer-name buffer))
          (erc-channel-list nil)))

(defun helm-select-erc-channel-buffer ()
  (interactive)
  (helm :sources
        (helm-build-sync-source "erc-buffers"
          :candidates (only-erc-channel-buffers)
          :action #'switch-to-buffer
          :fuzzy-match t)
        :buffer "**helm-erc-channel-buffers**"))

(provide 'lk/helm-erc-channels)
