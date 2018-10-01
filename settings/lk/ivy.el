(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (bind-key "C-c C-r" 'ivy-resume))

(use-package counsel
  :ensure t
  :bind
  ("M-x" . counsel-M-x)
  ("C-c n i" . counsel-imenu)
  ("C-c n b" . counsel-ibuffer)
  ("C-c n g" . counsel-git-grep))

(use-package projectile
  :after ivy
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (setq projectile-remember-window-configs t)
  (setq projectile-git-command "git ls-files -z -c --recurse-submodules")
  (projectile-mode +1))

(provide 'lk/ivy)
