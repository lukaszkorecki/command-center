;; set font size
(when (display-graphic-p)
  (set-face-attribute 'default nil :height 120))

(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)

(load-theme 'solarized-dark 't)

(provide 'lk/theme)
