;;; evil.el --- Evil mode customizations
(evil-leader/set-key
 "|" 'split-window-horizontally
 "-" 'split-window-vertically
 "g" 'projectile-grep
 "t" 'projectile-find-tag
 "T" 'projectile-regenrate-tags
 "d" 'dired
 "P" 'projectile-find-file-other-window
 "p" 'projectile-find-file)
