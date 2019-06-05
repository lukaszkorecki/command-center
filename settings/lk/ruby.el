;;; lk/ruby --- ruby related customizations
;;;; Commentary:
;;; Code:

(defun lk/ruby-format-current-buffer ()
  "Format current buffer with CljFmt - assume it's installed already
     (it is as it was added to ~/.lein/profiles.clj)"
  (interactive)
  (let ((file-name (buffer-file-name (current-buffer))))
    (compilation-start
     (format "rubocop -a %s" file-name)
     'compilation-mode)))

(use-package ruby-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))
  (setq-default ruby-indent-level 2)
  :bind
  (:map ruby-mode-map
        (("C-x c f" .  lk/ruby-format-current-buffer))))

(provide 'lk/ruby)
;;; ruby.el ends here
