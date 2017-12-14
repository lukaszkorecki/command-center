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

(require 'avy)
(global-set-key (kbd "C-.") 'avy-goto-char)

(global-set-key (kbd "C-x =") 'indent-according-to-mode)

(global-set-key (kbd "C-x l") 'lk/select-line)
(global-set-key (kbd "C-x j") 'lk/join-lines)

(require 'dumb-jump)
(global-set-key (kbd "C-c C-j") 'dumb-jump-go)
(global-set-key (kbd "C-c C-p") 'dumb-jump-go-prompt)

;; Git and git-surf helpers

(require 'git)
(defun lk/open-pr ()
  (interactive)
  (shell-command "git surf -p"))

(defun lk/open-current-file-in-gh ()
  (interactive)
  (shell-command (format "git surf %s" (file-name-nondirectory (buffer-file-name)))))

(defun lk/open-current-region-in-gh (start end)
  (interactive "r")
  (shell-command
   (format "git surf -r%s,%s %s"
           start
           end
           (file-name-nondirectory (buffer-file-name)))))


(defun lk/test-lines (start end)
  (interactive "r")
  (message "start %d end %d" start end))

(global-set-key (kbd "C-x g p") 'lk/open-pr)
(global-set-key (kbd "C-x g f") 'lk/open-current-file-in-gh)
(global-set-key (kbd "C-x g r") 'lk/open-current-region-in-gh)

;; magit stuff
(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-x C-g") 'vc-git-grep)


;; Window and buffer management

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

(defun lk/count-buffers ()
  (length (buffer-list)))

(require 'sane-term)
(global-set-key (kbd "C-x n t") 'sane-term)

(global-set-key (kbd "C-x r") 'vr/replace)

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
