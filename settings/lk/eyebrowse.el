
(require 'eyebrowse)
(eyebrowse-mode t)
(setq custom-file "~/.emacs.d/custom.el")


(global-set-key (kbd "C-x >") 'eyebrowse-next-window-config)
(global-set-key (kbd "C-x <") 'eyebrowse-prev-window-config)

(provide 'lk/eyebrowse)
