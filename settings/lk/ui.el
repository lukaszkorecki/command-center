;;; ui.el --- ...
;;; Commentary:

;;; Code:


;; set font size

(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

(defun on-after-init ()
  (when (display-graphic-p)
    (set-face-attribute 'default nil :family "PT Mono" :height 125))
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)


;; yes/no -> y/n
(defalias 'yes-or-no-p 'y-or-n-p)

(setq confirm-kill-emacs 'y-or-n-p)

;; turn off splash screen
(setq inhibit-startup-message t)

;; turn off the bell
(setq ring-bell-function 'ignore)

;; show buffer file name in title bar
(setq frame-title-format
      '((:eval
         (if (buffer-file-name)
             (abbreviate-file-name (buffer-file-name))
           "%b"))))

;; turn off initial scratch buffer message
(setq initial-scratch-message "")

;; always match parens
(show-paren-mode t)
(setq show-paren-delay 0)
(global-set-key (kbd "C-c f") 'toggle-frame-maximized)

(use-package rainbow-delimiters)


(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell nil)

;; Fix ansi-term rendering
(add-hook 'term-mode-hook 'my-term-mode-hook)
(defun my-term-mode-hook ()
  (setq bidi-paragraph-direction 'left-to-right))


(when (not (string-equal system-type "darwin"))
  (use-package color-theme-approximate
    :config (color-theme-approximate-on)))

(use-package bufler
  :init (require 'bufler)
  :bind (("C-x C-b" . bufler-list)))


(use-package undo-tree
  :init (global-undo-tree-mode nil)
  :bind (("C-c u" . undo-tree-visualize)))

(use-package treemacs
  :init (setq treemacs-space-between-root-nodes nil))

(use-package treemacs-projectile)

(use-package unicode-fonts
  :config (unicode-fonts-setup))

;; just exit if terminated or C-x C-c is invoked
(setq confirm-kill-processes nil)

(provide 'lk/ui)
;;; ui.el ends here
