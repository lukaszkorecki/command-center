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

(setq-default ruby-indent-level 2)

(provide 'lk/ruby)
;;; ruby.el ends here
