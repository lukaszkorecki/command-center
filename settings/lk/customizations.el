;;;; lk/customipaczations.el -- custom functions which make using emacs easier
;;; Commentary:
;;; If stuff grows too big, move it out to a separate file
;;; Code:


;; Editing helpers

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

;; disable c-z which maps to minimize
(global-unset-key (kbd "C-z"))
;; set in helm-projectile later, originally used in dired/tramp
(global-unset-key (kbd "C-c n p"))

(global-set-key (kbd "C-x =") 'indent-according-to-mode)

(global-set-key (kbd "C-x l") 'lk/select-line)
(global-set-key (kbd "C-x j") 'lk/join-lines)

;; Git and git-surf helpers


(defun lk/open-pr ()
  (interactive)
  (shell-command "git surf -p"))

(defun lk/open-current-file-in-gh ()
  (interactive)
  (let* ((line-no (line-number-at-pos))
         (command (format "git surf -r%s,%s %s"
                          line-no line-no
                          (file-name-nondirectory (buffer-file-name)))))
    (message command)
    (shell-command command)))

(global-set-key (kbd "C-x g f") 'lk/open-current-file-in-gh)

(use-package git
	     :bind (("C-x C-g" . vc-git-grep)
              ( "C-x g p" . lk/open-pr)))

;; magit stuff

(use-package magit
	     :bind (( "C-c m s" . magit-status)))


;; Editing and general syntax highlighting
(use-package move-text)
(use-package rainbow-delimiters)

;; Window and buffer management

(global-set-key (kbd "C-x |") 'split-window-horizontally)
(global-set-key (kbd "C-x -") 'split-window-vertically)

(use-package transpose-frame
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
	     :bind (( "C-x n T" . sane-term)
                    ("C-x n t" . sane-term-create)))

(use-package vr
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


(provide 'lk/customizations)
;;; customizations.el ends here
