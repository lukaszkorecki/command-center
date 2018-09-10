;; set font size
(when (display-graphic-p)
  (set-face-attribute 'default nil :height 120))

(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)

(use-package color-theme)
(use-package solarized-theme
  :after (color-theme)
  :init
  (load-theme 'solarized-light 't))

(use-package ansi-color)
;; yes/no -> y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; turn off splash screen
(setq inhibit-startup-message t)

;; turn off initial scratch buffer message
(setq initial-scratch-message "")

;; always match parens
(show-paren-mode t)
(setq show-paren-delay 0)



(provide 'lk/ui)
