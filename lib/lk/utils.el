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
        (puthash :output (buffer-string) return)))
    return))


(defun hget (alist k)
  "Get element from a hash table but in a sane way"
  (gethash k alist))

(defun hput (hm k v)
  "Modifies hast table in place, but also returns it so its easier to thread"
  (puthash  k v hm)
  hm)


(provide 'lk/utils)
