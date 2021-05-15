;;; lk/ruby --- ruby related customizations
;;;; Commentary:
;;; Code:

(defun lk/ruby-format-current-buffer ()
  (interactive)
  (lk/invoke-compile-tool-in-project "Gemfile" "rubocop -A %s"))


(add-hook
 'ruby-mode-hook
 (lambda ()
  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))

  (setq-default ruby-indent-level 2)
   (local-set-key (kbd "C-x c f") 'lk/ruby-format-current-buffer)))

(provide 'lk/ruby)
;;; ruby.el ends here
