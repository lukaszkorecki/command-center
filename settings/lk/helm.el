;;;; lk/helm.el -- all things helm & projectile
(use-package projectile)
(use-package grizzl)
(use-package helm
  :init (setq helm-M-x-fuzzy-match t)
  :bind (( "C-c n b" . helm-buffers-list)


         ;; in-buffer navigation
         ( "C-c n i" . helm-semantic-or-imenu)
         ( "C-c n s" . helm-occur)

         ;; override M-x to use helm-M-x
         ( "M-x" . helm-M-x)))

(use-package helm-git-grep
  :after (helm)
  :bind ( ( "C-c n g" . helm-git-grep)))

(use-package helm-projectile
  :after (helm projectile grizzl)
  :init
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (setq projectile-git-command "git ls-files -z -c --recurse-submodules")
  (setq projectile-use-git-grep t)
  (setq projectile-completion-system 'grizzl)
  :bind (( "C-c n p" . helm-projectile-find-file)))

;; dock helm window in the bottom
(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))



(provide 'lk/helm)
