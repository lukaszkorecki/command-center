;;; evil.el --- Evil mode customizations
(evil-leader/set-key
 "|" 'split-window-horizontally
 "-" 'split-window-vertically
 "g" 'find-grep
 "d" 'dired
 "P" 'projectile-find-file-other-window
 "p" 'projectile-find-file)
