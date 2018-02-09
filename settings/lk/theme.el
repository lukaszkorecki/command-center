(load-theme 'solarized-light 't)
;; set font size
(when (display-graphic-p)
  (set-face-attribute 'default nil :height 120))

(provide 'lk/theme)
