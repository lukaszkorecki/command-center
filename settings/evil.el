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
 "g" 'projectile-grep
 "d" 'dired
 "t" 'projectile-find-tag
 "T" 'projectile-regenerate-tags
 "P" 'projectile-find-file-other-window
 "p" 'projectile-find-file
 "x" 'execute-extended-command
 "f" 'evil-fix-with-rubocop-and-reload
 "b" 'ruby-toggle-block)


;;; Make magit and emacs behavie like Fugitive
(defun evil-git-checkout-current-file ()
  "Run git checkout on currently opened file."
  (interactive)
  (start-process "git-checkout"
                 (get-buffer-create "*git-checkout*")
                 "git"
                 "checkout"
                 buffer-file-name)
  (find-file-noselect buffer-file-name))

(defun evil-git-remove-current-file ()
  "Run git rm on current file and kill current buffer."
  (interactive)
  (start-process "git-remove"
                 (get-buffer-create "*git-remove*")
                 "git"
                 "rm"
                 "-f"
                 buffer-file-name)
  (kill-buffer))

(evil-ex-define-cmd "Git-status" 'magit-status)
(evil-ex-define-cmd "Git-remove" 'evil-git-remove-current-file)
(evil-ex-define-cmd "Git-checkout" 'evil-git-checkout-current-file)

;;; tags!
(evil-ex-define-cmd "tag" 'tags-search)

;; functions provided by move-text package
(define-key global-map (kbd "\C-k") 'move-text-up)
(define-key global-map (kbd "\C-j") 'move-text-down)


(provide 'evil.el)
;;; evil.el ends here
