;;;; lk/customipaczations.el -- custom functions which make using emacs easier
;;; Commentary:
;;; If stuff grows too big, move it out to a separate file
;;; Code:

(require 'git)

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

(defun lk/count-buffers ()
  (length (buffer-list)))

(defun lk/open-pr ()
  (interactive)
  (shell-command "git surf -p"))

(global-set-key (kbd "C-x =") 'indent-according-to-mode)

(global-set-key (kbd "C-x l") 'lk/select-line)
(global-set-key (kbd "C-x j") 'lk/join-lines)
(global-set-key (kbd "C-x g p") 'lk/open-pr)

(global-set-key (kbd "C-x |") 'split-window-horizontally)
(global-set-key (kbd "C-x -") 'split-window-vertically)

;; better window movements
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)

(global-set-key (kbd "C-c t") 'transpose-frame)

;; bind awkard M-[ & M-] to something better
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; magit stuff
(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-x C-g") 'vc-git-grep)

;; window-number

;; enable window-number mode
(require 'window-number)
(window-number-mode 1)

(global-set-key (kbd "C-c C-n") 'window-number-switch)

;; override C-x C-o with a variant which:
;; deletes all blank lines and inserts a new one
(defun lk/reduce-blank-lines ()
  (interactive)
  (delete-blank-lines)
  (end-of-line)
  (insert-char "\n" 1))


(require 'sane-term)
(global-set-key (kbd "C-x n t") 'sane-term)

(global-set-key (kbd "C-x r") 'vr/replace)
(provide 'lk/customizations)
;;; customizations.el ends here
