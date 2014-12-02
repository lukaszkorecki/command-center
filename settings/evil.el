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
 "g" 'evil-git-grep
 "d" 'dired
 "P" 'projectile-find-file-other-window
 "p" 'projectile-find-file
 "x" 'execute-extended-command
 "c" 'comment-or-uncomment-region-or-line
 "f" 'evil-fix-with-rubocop-and-reload
 "u" 'run-tests
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

(defun evil-git-grep (search)
  "Run git-grep in current git dir with SEARCH."
  (interactive (list (completing-read "Search for: " nil nil nil (current-word))))
  (grep-find (concat "git --no-pager grep -P -n " search " `git rev-parse --show-toplevel`")))


(evil-ex-define-cmd "Git-status" 'magit-status)
(evil-ex-define-cmd "Git-remove" 'evil-git-remove-current-file)
(evil-ex-define-cmd "Git-checkout" 'evil-git-checkout-current-file)
(evil-ex-define-cmd "Git-grep" 'evil-git-grep)

;;; tags!
(evil-ex-define-cmd "tag" 'tags-search)

;; functions provided by move-text package
(define-key global-map (kbd "\C-k") 'move-text-up)
(define-key global-map (kbd "\C-j") 'move-text-down)


(provide 'evil.el)
;;; evil.el ends here
