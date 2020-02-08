;; strip whitespace
(use-package whitespace
  :ensure t
  :init
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  :config
  (setq whitespace-style '(face empty tabs lines-tail trailing))
  (setq whitespace-line-column 80))

;; add final newline automaticaly
(setq require-final-newline t)

;; indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default standard-indent 2)

(define-key global-map (kbd "RET") 'newline-and-indent)

(define-key global-map (kbd "C-c R") 'revert-buffer)

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


;; Editing and general syntax highlighting
(use-package move-text
  :ensure t)


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

(use-package visual-regexp
  :ensure t
  :ensure visual-regexp
  :bind (( "C-x r" . vr/replace)))

;; no backup files
(setq-default make-backup-files nil)
;; no lockfiles
(setq create-lockfiles nil)


(provide 'lk/editing)
