;;; -*- lexical-binding: t; -*-
;;; system-tweaks.el --- Platform-specific customizations
;;; Commentary:
;;; Contains platform-specific tweaks (primarily macOS) and general Emacs
;;; enhancements like which-key. Custom helper functions that make using Emacs easier.

;;; Code:

;; Editing helpers

(defun lk/count-buffers () (length (buffer-list)))

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
  (mac-switch-meta)

  (global-set-key (kbd "H-x") 'execute-extended-command))

;; hide dotfiles (except . and ..) in dired, toggle with C-x M-o
;; extensions are cleared so dired-omit-mode hides nothing else
(use-package dired-x
  :ensure nil
  :defer nil
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-files "\\`\\.\\(?:[^.]\\|\\..\\)"
        dired-omit-extensions nil
        dired-omit-verbose nil))

;; built into Emacs 30+, no :ensure
(use-package which-key
  :config (setq which-key-show-early-on-C-h t)
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (setq which-key-idle-delay 1000)
  (setq which-key-idle-secondary-delay 0.05)
  (which-key-mode))

(provide 'lk/system-tweaks)
;;; system-tweaks.el ends here
