;; set font size
(when (display-graphic-p)
  (set-face-attribute 'default nil :height 120))

(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)

(use-package color-theme
  :ensure t)
(use-package solarized-theme
  :ensure t
  :after (color-theme)
  :init
  (load-theme 'solarized-dark 't))

(use-package ansi-color
  :ensure t)

;; yes/no -> y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; turn off splash screen
(setq inhibit-startup-message t)

;; turn off the bell
(setq ring-bell-function 'ignore)

;; show buffer file name in title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; turn off initial scratch buffer message
(setq initial-scratch-message "")

;; always match parens
(show-paren-mode t)
(setq show-paren-delay 0)

;; show imenu-list
(use-package imenu-list
  :ensure t
  :bind (( "C-c n l" . imenu-list-smart-toggle)))

(use-package ace-window
  :ensure t
  :init
  (setq aw-keys  '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-ignore-current t)
  :bind (( "M-o" . ace-window)))

(provide 'lk/ui)
