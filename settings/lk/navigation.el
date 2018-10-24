(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (setq ivy-height 25)
  (ivy-mode 1)
  :bind
  ("C-c n s" . swiper)
  ("C-c C-r" . ivy-resume))

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

(use-package dumb-jump
  :ensure t
  :bind
  (("C-c n j" . dumb-jump-go))
  :config
  (setq dumb-jump-selector 'ivy))

(provide 'lk/navigation)
