;;;; lk/customipaczations.el -- custom functions which make using emacs easier
;;; Commentary:
;;; If stuff grows too big, move it out to a separate file
;;; Code:


;; Editing helpers
(defun lk/show-kill-ring ()
  "Insert all `kill-ring' content in a new buffer named *copy history*.
Based on  `http://ergoemacs.org/emacs/emacs_show_kill_ring.html'"
  (interactive)
  (let* ((buf-name "*copy history*")
         (_ignore (kill-buffer (get-buffer buf-name)))
         (copy-buf (generate-new-buffer buf-name)))
    (progn
      (switch-to-buffer copy-buf)
      (funcall 'fundamental-mode)
      (setq buffer-offer-save t)
      (dolist (x kill-ring)
        (insert x "\n\n>>>>---------------------------------\n\n"))
      (goto-char (point-min)))))

(defun lk/select-line ()
  "Select current line"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun lk/join-lines ()
  (interactive)
  (next-line)
  (end-of-line)
  (join-line))


;; Disable certain commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'magit-clean 'disabled nil)

;; disable c-z which maps to minimize
(global-unset-key (kbd "C-z"))
;; set in projectile later, originally used in dired/tramp
(global-unset-key (kbd "C-c n p"))

(global-set-key (kbd "C-x =") 'indent-according-to-mode)

(global-set-key (kbd "C-x l") 'lk/select-line)
(global-set-key (kbd "C-x j") 'lk/join-lines)

(global-set-key (kbd "C-c n k") 'lk/show-kill-ring)

;; Git and git-surf helpers
(defun lk/open-pr ()
  (interactive)
  (shell-command "~/.emacs.d/etc/bin/git-surf -p"))

(defun lk/open-current-file-in-gh ()
  (interactive)
  (let* ((line-no (line-number-at-pos))
         (command (format "~/.emacs.d/etc/bin/git-surf -r%s,%s %s"
                          line-no line-no
                          (file-name-nondirectory (buffer-file-name)))))
    (message command)
    (shell-command command)))

(global-set-key (kbd "C-x g f") 'lk/open-current-file-in-gh)

(use-package git
  :ensure t
	:bind (("C-x C-g" . vc-git-grep)
         ("C-x g p" . lk/open-pr)))

(use-package dumb-jump
  :ensure t
  :bind
  (("C-c n j" . dumb-jump-go))
  :config
  (setq dumb-jump-selector 'ivy))

;; magit stuff

(use-package magit
  :ensure t
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
	:bind
  (( "C-c m s" . magit-status)))

;; Editing and general syntax highlighting
(use-package move-text
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

;; Window and buffer management

(global-set-key (kbd "C-x |") 'split-window-horizontally)
(global-set-key (kbd "C-x -") 'split-window-vertically)

(use-package transpose-frame
  :ensure t
  :bind (( "C-c t" . transpose-frame)))

;; bind awkard M-[ & M-] to something better
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)


;; override C-x C-o with a variant which:
;; deletes all blank lines and inserts a new one
(defun lk/reduce-blank-lines ()
  (interactive)
  (delete-blank-lines)
  (end-of-line)
  (insert-char "\n" 1))

(defun lk/count-buffers ()
  (length (buffer-list)))

(use-package sane-term
    :ensure t
	  :bind (( "C-x n T" . sane-term)
           ("C-x n t" . sane-term-create)))

(use-package visual-regexp
  :ensure t
  :ensure visual-regexp
  :bind (( "C-x r" . vr/replace)))

;; OSX stuff, make sure alt is meta in GUI emacs
(defun mac-switch-meta nil
  "switch meta between Option and Command"
  (interactive)
  (if (eq mac-option-modifier nil)
      (progn
        (setq mac-option-modifier 'meta)
        (setq mac-command-modifier 'hyper))
    (progn
      (setq mac-option-modifier nil)
      (setq mac-command-modifier 'meta))))
(mac-switch-meta)
(mac-switch-meta)

;; more better bookmarks
(global-set-key (kbd "C-c b s") 'bookmark-set)
(global-set-key (kbd "C-c b j") 'bookmark-jump)
(global-set-key (kbd "C-c b l") 'list-bookmarks)

(provide 'lk/customizations)
;;; customizations.el ends here
