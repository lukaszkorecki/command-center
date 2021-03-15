;;; lk/ruby --- ruby related customizations
;;;; Commentary:
;;; Code:

(defun lk/ruby-format-current-buffer ()
  "Format current buffer with CljFmt - assume it's installed already
     (it is as it was added to ~/.lein/profiles.clj)"
  (interactive)
  (lk/invoke-compile-tool-in-project "Gemfile" "rubocop -A %s"))


(defun lk/rb-rebuild-tags ()
  (interactive)
  (lk/invoke-compile-tool-in-project "Gemfile"
  "git ls-files | egrep  '*.rb' | xargs etags --language=ruby TAGS"))


(use-package ruby-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
  (setq-default ruby-indent-level 2)
  :bind
  (:map ruby-mode-map
        (("C-x c f" .  lk/ruby-format-current-buffer)
         ("C-x c t" . lk/rb-rebuild-tags )
         )))

(provide 'lk/ruby)
;;; ruby.el ends here
