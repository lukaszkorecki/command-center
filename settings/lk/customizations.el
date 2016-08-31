;;;; lk/customipaczations.el -- custom functions which make using emacs easier
;;; Commentary:
;;; If stuff grows too big, move it out to a separate file
;;; Code:

;;; Make magit and emacs behavie like Fugitive
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

(global-set-key (kbd "C-x =") 'indent-according-to-mode)

(global-set-key (kbd "C-x l") 'lk/select-line)
(global-set-key (kbd "C-x j") 'lk/join-lines)


(global-set-key (kbd "C-x |") 'split-window-horizontally)
(global-set-key (kbd "C-x -") 'split-window-vertically)

;; better window movements
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)

;; bind awkard M-[ & M-] to something better
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; magit stuff
(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-x C-g") 'vc-git-grep)

;; avy jump
(require 'avy)
(global-set-key (kbd "C-x y") 'avy-goto-word-0)

;; override C-x C-o with a variant which:
;; deletes all blank lines and inserts a new one
(defun lk/reduce-blank-lines ()
  (interactive)
  (delete-blank-lines)
  (end-of-line)
  (insert-char "\n" 1))

;; shortcut fn for starting ansi-term with a custom name
(defun lk/new-term (term-name)
  (interactive "sTerm name: ")
  (let ((buf-name (format "term: %s" term-name)))
    (ansi-term "/bin/bash" buf-name)))

(provide 'lk/customizations)
;;; customizations.el ends here
