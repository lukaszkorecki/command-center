;;; Customized dracula theme

(setq linum-format " \u2502 ")
(setq linum-format "%4d \u2502 ")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)


(provide 'lk/theme)
