;;; lk/ruby --- ruby related customizations
;;;; Commentary:
;;; Code:
(use-package ruby-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))
  (setq-default ruby-indent-level 2))

(provide 'lk/ruby)
;;; ruby.el ends here
