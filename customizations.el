;;;; customizations.el -- custom functions which make usingemacs easier
;;; Commentary:
;;; also include maps :-)
;;; Code:

;;; Make magit and emacs behavie like Fugitive
(defun lk/git-checkout-current-file ()
  "Run git checkout on currently opened file."
  (interactive)
  (start-process "git-checkout"
                 (get-buffer-create "*git-checkout*")
                 "git"
                 "checkout"
                 buffer-file-name)
  (find-file-noselect buffer-file-name))

(defun lk/git-remove-current-file ()
  "Run git rm on current file and kill current buffer."
  (interactive)
  (start-process "git-remove"
                 (get-buffer-create "*git-remove*")
                 "git"
                 "rm"
                 "-f"
                 buffer-file-name)
  (kill-buffer))

(defun lk/git-grep (search)
  "Run git-grep in current git dir with SEARCH."
  (interactive (list (completing-read "Search for: " nil nil nil (current-word))))
  (grep-find (concat "git --no-pager grep -P -n " search " `git rev-parse --show-toplevel`")))



(global-set-key (kbd "C-x p") 'projectile-find-file)
(global-set-key (kbd "C-x p") 'projectile-find-file-other-window)
(global-set-key (kbd "C-x g") 'lk/git-grep)


(provide 'customizations)
;;; customizations.el ends here
