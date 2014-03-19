;;; evil.el --- Evil mode customizations
(evil-leader/set-key
 "|" 'split-window-horizontally
 "-" 'split-window-vertically
 "ch" 'ruby-convert-hash
 "g" 'projectile-grep
 "t" 'projectile-find-tag
 "T" 'projectile-regenerate-tags
 "P" 'projectile-find-file-other-window
 "p" 'projectile-find-file)


;;; Activate magit like fugitive
(evil-ex-define-cmd "Git-status" 'magit-status)

;;; TODO add more vim mappings to magit!

;;; tags!
(evil-ex-define-cmd "tag" 'tags-search)
