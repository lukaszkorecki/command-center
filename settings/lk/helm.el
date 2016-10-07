;;;; lk/helm.el -- all things helm & projectile

(require 'lk/helm-terminals)
(require 'lk/helm-erc-channels)

(require 'helm-projectile)

(setq projectile-completion-system 'helm)
;; (projectile-global-mode)
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

(global-set-key (kbd "C-c n t") 'helm-select-terminal-buffer)
(global-set-key (kbd "C-c n c") 'helm-select-erc-channel-buffer)
(global-set-key (kbd "C-c n p") 'helm-projectile-find-file)
(global-set-key (kbd "C-c n n") 'helm-projectile-find-file)
(global-set-key (kbd "C-c n b") 'helm-buffers-list)
(global-set-key (kbd "C-c n g") 'helm-git-grep)

;; in-buffer navigation
(global-set-key (kbd "C-c n i") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-c n s") 'helm-occur)

;; override M-x to use helm-M-x
(global-set-key (kbd "M-x") 'helm-M-x)

(provide 'lk/helm)
