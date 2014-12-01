;;; ruby.el --- ruby related customizations
;;;; Commentary:
;;; Mostly hooks for evil mappings
;;; Code:
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))

(defun ruby-convert-hash ()
  "Convert old school :hash => 'val' to hash: 'val'."
  (interactive)
  (delete-char 1)
  (search-forward " ")
  (delete-backward-char 1)
  (zap-to-char 1 ?>)
  (insert ":"))


(defun evil-fix-with-rubocop-and-reload ()
  "Run rubocop -a and reloads the buffer."
  (interactive)
  (start-process "rubo-cop-silent"
                 (get-buffer-create "*Rubo-cop-auto*")
                 "rubocop"
                 "-a"
                 buffer-file-name)
  (find-file-noselect buffer-file-name))

(add-hook 'ruby-mode-hook (lambda () (abbrev-mode)))

(provide 'ruby)
;;; ruby.el ends here
