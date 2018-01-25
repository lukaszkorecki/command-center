(load-theme 'solarized-light 't)
;; set font size
(when (display-graphic-p)
  (set-face-attribute 'default nil :height 140))

(provide 'lk/theme)
