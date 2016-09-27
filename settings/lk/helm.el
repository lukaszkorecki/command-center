;;;; lk/helm.el -- all things helm & projectile

(require 'lk/helm-terminals)
(require 'lk/helm-erc-channels)

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
(global-unset-key (kbd "C-x C-n"))
(global-unset-key (kbd "C-c p t"))
(global-unset-key (kbd "C-c p p"))
(global-unset-key (kbd "C-c p b"))
(global-unset-key (kbd "C-c p g"))
(global-unset-key (kbd "C-c p i"))
(global-unset-key (kbd "C-c p s"))

(global-set-key (kbd "C-c p t") 'helm-select-terminal-buffer)
(global-set-key (kbd "C-c p c") 'helm-select-erc-channel-buffer)
(global-set-key (kbd "C-c p p") 'helm-projectile-find-file)
(global-set-key (kbd "C-c p b") 'helm-buffers-list)
(global-set-key (kbd "C-c p g") 'helm-git-grep)

;; in-buffer navigation
(global-set-key (kbd "C-c p i") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-c p s") 'helm-occur)

;; override M-x to use helm-M-x
(global-set-key (kbd "M-x") 'helm-M-x)

(provide 'lk/helm)
