;;;; git.el --- git functions and such
(defun git-surf-file ()
  (interactive)
  (start-process "git-surf"
                 (get-buffer-create "*git-surf*")
                 "bash"
                 (expand-file-name "~/.DotFiles/bins/git-surf")
                 buffer-file-name))
