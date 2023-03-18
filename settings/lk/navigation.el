;;; navigation.el --- ...
;;; Commentary:

;;; Code:

(use-package ivy
  :diminish ivy-mode
  :config (setq ivy-height 25)
  (ivy-mode 1)
  :bind (("C-c s" . swiper)
         ("C-c C-r" . ivy-resume)))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-c n i" . counsel-imenu)
         ("C-c n b" . counsel-ibuffer)
         ("C-c n y" . counsel-yank-pop)))

(use-package projectile
  :init
  (projectile-mode +1)
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map)
        (("C-c C-g" . 'projectile-grep)))
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-git-command "git ls-files -z -c --recurse-submodules")
  (add-to-list 'projectile-globally-ignored-directories "vendor")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "target")
  (setq projectile-project-root-functions
        '(projectile-root-local
          projectile-root-top-down
          projectile-root-bottom-up
          projectile-root-top-down-recurring)))


(use-package ace-window
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-ignore-current t)
  (setq aw-dispatch-always t)
  (setq aw-minibuffer-flag t)
  (set-face-foreground 'aw-background-face "gray70")
  (add-hook 'term-mode-hook
            (lambda ()
              (define-key term-raw-map (kbd "M-o") 'ace-window)))
  :bind (( "M-o" . ace-window)))

;; Window and buffer management
(global-set-key (kbd "C-x |") 'split-window-horizontally)
(global-set-key (kbd "C-x -") 'split-window-vertically)

(use-package transpose-frame :bind (( "C-c t" . transpose-frame)))


(use-package avy
  :bind (("C-c v c" . avy-goto-char-2)
         ("C-c v l" . avy-goto-line)
         ("C-c v w" . avy-goto-word-1)))


(use-package emamux)

(defun lk/open-locally ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))


(provide 'lk/navigation)
;;; navigation.el ends here
