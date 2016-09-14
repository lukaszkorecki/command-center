;;;; lk/helm.el -- all things helm & projectile

(require 'lk/helm-terminals)


(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(setq helm-M-x-fuzzy-match t)
;; dock helm window in the bottom
(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))


(setq projectile-use-git-grep t)
(setq projectile-completion-system 'grizzl)

;; project/dir navigation
(global-set-key (kbd "C-x n t") 'helm-select-terminal-buffer)
(global-set-key (kbd "C-x n p") 'helm-projectile-find-file)
(global-set-key (kbd "C-x n b") 'helm-buffers-list)
(global-set-key (kbd "C-x n g") 'helm-git-grep)

;; in-buffer navigation
(global-set-key (kbd "C-x i") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c s") 'helm-swoop)

;; override M-x to use helm-M-x
(global-set-key (kbd "M-x") 'helm-M-x)

(provide 'lk/helm)
