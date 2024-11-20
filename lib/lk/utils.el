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
