;;;; lk/customizations.el -- custom functions which make usingemacs easier
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


(defun lk/select-line ()
  "Select current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))




(global-set-key (kbd "C-x p") 'helm-projectile-find-file)
(global-set-key (kbd "C-x M-p") 'projectile-find-file-other-window)
(global-set-key (kbd "C-x g") 'lk/git-grep)
(global-set-key (kbd "C-x =") 'indent-according-to-mode)

(global-set-key (kbd "C-x t") 'turnip-send-region)

(global-set-key (kbd "C-x l") 'lk/select-line)
(global-set-key (kbd "C-x j") 'join-line)


;; override M-x to use helm-M-x

(global-set-key (kbd "M-x") 'helm-M-x)
(provide 'lk/customizations)
;;; customizations.el ends here
