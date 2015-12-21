;;;; lk/customizations.el -- custom functions which make usingemacs easier
;;; Commentary:
;;; also include maps :-)
;;; Code:

;;; Make magit and emacs behavie like Fugitive
(require 'git)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(setq helm-M-x-fuzzy-match t)

(setq projectile-use-git-grep t)
(setq projectile-completion-system 'grizzl)

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

(defun lk/git-surf-current-line ()
  (interactive)
  (defvar-local curr-line (line-number-at-pos))
  (message "Current line %s" curr-line))

(defun lk/select-line ()
  "Select current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(defun lk/join-lines ()
  (interactive)
  (next-line)
  (end-of-line)
  (join-line))

(global-set-key (kbd "C-x p") 'helm-projectile-find-file)
(global-set-key (kbd "C-x M-p") 'helm-projectile-find-other-file)
(global-set-key (kbd "C-x b") 'helm-mini)

(global-set-key (kbd "C-x g") 'lk/git-grep)
(global-set-key (kbd "C-x =") 'indent-according-to-mode)

(global-set-key (kbd "C-x t") 'turnip-send-region)

(global-set-key (kbd "C-x l") 'lk/select-line)
(global-set-key (kbd "C-x j") 'lk/join-lines)

(global-set-key (kbd "C-x |") 'split-window-horizontally)
(global-set-key (kbd "C-x -") 'split-window-vertically)

;; better window movements
(global-set-key (kbd "ESC <left>") 'windmove-left)
(global-set-key (kbd "ESC <right>") 'windmove-right)
(global-set-key (kbd "ESC <up>") 'windmove-up)
(global-set-key (kbd "ESC <down>") 'windmove-down)

;; override M-x to use helm-M-x
(global-set-key (kbd "M-x") 'helm-M-x)

;; ace jump
(require 'ace-jump-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-word-mode)

(provide 'lk/customizations)
;;; customizations.el ends here
