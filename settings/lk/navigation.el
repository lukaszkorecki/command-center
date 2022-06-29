;;; navigation.el --- ...
;;; Commentary:

;;; Code:



(use-package ivy
  :diminish ivy-mode
  :config (setq ivy-height 25)
  (ivy-mode 1)
  :bind ("C-c s" . swiper)
  ("C-c C-r" . ivy-resume))

(use-package counsel
  :bind ("M-x" . counsel-M-x)
  ("C-c n i" . counsel-imenu)
  ("C-c n b" . counsel-ibuffer)
  ("C-c n g" . counsel-git-grep)
  ("C-c n y" . counsel-yank-pop))

(use-package projectile
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :init (projectile-mode +1)
  :config (setq projectile-completion-system 'ivy)
  (setq projectile-git-command "git ls-files -z -c --recurse-submodules")
  (add-to-list 'projectile-globally-ignored-directories "vendor")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "target")
  (setq projectile-project-root-functions
        '(
          projectile-root-local
          projectile-root-top-down
          projectile-root-bottom-up
          projectile-root-top-down-recurring
          ))
  )


(defun lk/find-todos-etc ()
  (interactive)
  (vc-git-grep
   "(TODO|FIXME|NOTE|XXX|HACK):" " "
   (vc-git-root default-directory)))

(global-set-key (kbd "C-c n t") 'lk/find-todos-etc)


(use-package ace-window
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-ignore-current t)
  (add-hook 'term-mode-hook
            (lambda ()
              (define-key term-raw-map (kbd "M-o") 'ace-window)))
  :bind (( "M-o" . ace-window)))

;; Window and buffer management
(global-set-key (kbd "C-x |") 'split-window-horizontally)
(global-set-key (kbd "C-x -") 'split-window-vertically)

(use-package transpose-frame
  :bind (( "C-c t" . transpose-frame)))


(use-package emamux)

(defun lk/open-locally ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))


(provide 'lk/navigation)
;;; navigation.el ends here
