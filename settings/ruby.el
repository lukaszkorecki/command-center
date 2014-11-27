;;; ruby.el --- ruby related customizations
;;;; Commentary:
;;; Mostly hooks for evil mappings
;;; Code:
(add-to-list 'auto-mode-alist '("\\.(rb|rake)$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))

(defun ruby-convert-hash ()
  (interactive)
  (delete-char 1)
  (search-forward " ")
  (delete-backward-char 1)
  (zap-to-char 1 ?>)
  (insert ":"))


(defun evil-fix-with-rubocop-and-reload ()
  "Run rubocop -a and reloads the buffer."
  (interactive)
  (rubocop-autocorrect-current-file)
  (revert-buffer nil 't))

(add-hook 'ruby-mode-hook (lambda () (abbrev-mode)))

(provide 'ruby)
;;; ruby.el ends here
