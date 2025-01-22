(defun lk/read-file (file &optional oneline)
  (with-temp-buffer
  	(insert-file-contents file)
	  (goto-char (point-min))
	  (buffer-substring
     (point)
		 (if oneline
         ;; read only 1st line
         (line-end-position)
       ;; read whole file
       (point-max)))))


(defun lk/invoke-cli (bufname cmd)
  (let* ((return (make-hash-table)))
    (when-let ((b (get-buffer bufname)))
      (kill-buffer b))
    (with-current-buffer (get-buffer-create bufname)
      (erase-buffer)
      (let ((exit-code (call-process-shell-command cmd nil t)))
        (puthash :status exit-code return)
        (puthash :output (buffer-string) return))
      (kill-buffer))
    return))


(defun hget (alist k)
  "Get element from a hash table but in a sane way"
  (gethash k alist))

(defun hput (hm k v)
  "Modifies hast table in place, but also returns it so its easier to thread"
  (puthash  k v hm)
  hm)


(defun lk/fix-utf ()
  (interactive)
  ;; unicode rules everything around me
  (setq locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (define-coding-system-alias 'UTF-8 'utf-8))


(provide 'lk/utils)
