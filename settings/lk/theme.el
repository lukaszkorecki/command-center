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

(provide 'lk/theme)
