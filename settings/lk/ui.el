;; set font size


(defun on-after-init ()
  (when (display-graphic-p)
    (set-face-attribute 'default nil
                        :family "Monaco"
                        :height 110))
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)

(use-package ansi-color
  :ensure t)

;; yes/no -> y/n
(defalias 'yes-or-no-p 'y-or-n-p)

(setq confirm-kill-emacs 'y-or-n-p)

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

(use-package rainbow-delimiters
  :ensure t)


(setq echo-keystrokes 0.1
      use-dialog-box nil visible-bell nil)

(use-package highlight-indent-guides
  :ensure t
  :init
  (setq highlight-indent-guides-method 'character)
  (add-hook 'yaml-mode 'highlight-indent-guides-mode))

;; Fix ansi-term rendering
(add-hook 'term-mode-hook 'my-term-mode-hook)
(defun my-term-mode-hook ()
  (setq bidi-paragraph-direction 'left-to-right))


(when (not  (string-equal system-type "darwin"))
  (use-package color-theme-approximate
    :config
    (color-theme-approximate-on)))

(when (string-equal system-type "darwin")
  (use-package solarized-theme
    :ensure t
    :init (load-theme 'solarized-light t)))

(use-package bufler
  :ensure t
  :init
  (require 'bufler)
  :bind
  (("C-x C-B" . bufler-list)))


(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode nil)
  :bind (("C-c u" . undo-tree-visualize)))

(use-package treemacs
  :ensure t
  :init
  (setq treemacs-space-between-root-nodes nil))

(use-package treemacs-projectile
  :ensure t)

(provide 'lk/ui)
