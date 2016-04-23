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


;; magit stuff
(global-set-key (kbd "C-c m s") 'magit-status)

;; avy jump
(require 'avy)
(global-set-key (kbd "C-c s") 'avy-goto-word-0)

(provide 'lk/customizations)
;;; customizations.el ends here
