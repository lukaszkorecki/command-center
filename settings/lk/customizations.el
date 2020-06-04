;;;; lk/customipaczations.el -- custom functions which make using emacs easier
;;; Commentary:
;;; If stuff grows too big, move it out to a separate file
;;; Code:


;; Editing helpers

(defun lk/count-buffers ()
  (length (buffer-list)))

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

(when (string-equal system-type "darwin")
  (mac-switch-meta)
  (mac-switch-meta))

(use-package sane-term
    :ensure t
	  :bind (( "C-x n t" . sane-term)
           ("C-x n T" . sane-term-create)))

(global-set-key (kbd "C-x n e") 'eshell)

(use-package which-key
  :ensure t
  :init
  (setq which-key-show-early-on-C-h t)
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode))

(use-package todoist
  :ensure t
  :init
  (setq todoist-show-all 't))

(provide 'lk/customizations)
;;; customizations.el ends here
