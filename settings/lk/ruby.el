;;; lk/ruby --- ruby related customizations
;;;; Commentary:
;;; Code:
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))

(add-hook 'ruby-mode-hook #'linum-mode)

(defun lk/ruby-convert-hash ()
  "Convert old school :hash => 'val' to hash: 'val'."
  (interactive)
  (delete-char 1)
  (search-forward " ")
  (delete-backward-char 1)
  (zap-to-char 1 ?>)
  (insert ":"))


(defun lk/fix-with-rubocop-and-reload ()
  "Run rubocop -a and reloads the buffer."
  (interactive)
  (save-buffer)
  (start-process "rubo-cop-silent"
                 (get-buffer-create "*Rubo-cop-auto*")
                 "rubocop"
                 "-a"
                 buffer-file-name)
  (revert-buffer t t))

(add-hook 'ruby-mode-hook (lambda () (abbrev-mode)))


(defun lk/run-tests ()
  "Run minitest test for current buffer using `tu` wrapper."
  (interactive)
  (shell-command
   (format
    (file-truename "~/.DotFiles/bins/tu %s %s")
    (shell-quote-argument buffer-file-name)
    (projectile-project-root))))



(provide 'lk/ruby)
;;; ruby.el ends here
