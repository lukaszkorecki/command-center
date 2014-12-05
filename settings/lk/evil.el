;;; evil.el --- Evil mode customizations
;;;; Commentary:
; Sets up most of the things I've used in Vim
; which were mapped to <leader><k>
; Additionally adds Fugitive like functions
;;; Code:
(evil-leader/set-key
 "|" 'split-window-horizontally
 "-" 'split-window-vertically
 "ch" 'ruby-convert-hash
 "g" 'lk/git-grep
 "d" 'dired
 "P" 'projectile-find-file-other-window
 "p" 'projectile-find-file
 "x" 'execute-extended-command
 "c" 'comment-or-uncomment-region-or-line
 "f" 'lk/fix-with-rubocop-and-reload
 "u" 'lk/run-tests
 "b" 'ruby-toggle-block)


(evil-ex-define-cmd "Git-status" 'magit-status)
(evil-ex-define-cmd "Git-remove" 'lk/git-remove-current-file)
(evil-ex-define-cmd "Git-checkout" 'lk/git-checkout-current-file)
(evil-ex-define-cmd "Git-grep" 'lk/git-grep)

;;; tags!
(evil-ex-define-cmd "tag" 'tags-search)

(provide 'evil.el)
;;; evil.el ends here
