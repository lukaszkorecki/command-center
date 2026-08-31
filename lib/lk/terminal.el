;;; -*- lexical-binding: t; -*-
;;; terminal.el --- Terminal and utility tools
;;; Commentary:
;;; Configures ghostel for terminal emulation, mermaid for diagrams,
;;; keychain for SSH key management, and time-zones utility.

;;; Code:

(defun lk/ghostel-new ()
  "Create a fresh Ghostel terminal buffer."
  (interactive)
  ;; '(4) is the raw prefix arg Emacs hands to `interactive "P"' when the user
  ;; presses C-u once — ghostel treats any non-numeric prefix as "make a new
  ;; buffer", so this is the programmatic form of `C-u M-x ghostel'.
  (ghostel '(4)))

(defun lk/kill-all-ghostels ()
  (interactive)
  (lk/kill-buffers-by-major-mode 'ghostel-mode))

(use-package ghostel
  :vc (:url "https://github.com/dakra/ghostel"
            :lisp-dir "lisp"
            :rev :newest)
  :config
  (setq ghostel-buffer-name "*term*")
  :bind (("C-x t n" . lk/ghostel-new)
         ("C-c M-o" . ghostel-clear-scrollback)
         ("C-x t p" . ghostel-project)
         ("C-x t o" . ghostel-other)
         :map ghostel-semi-char-mode-map
         ("C-c ESC o" . ghostel-clear-scrollback)
         ("C-q" . ghostel-send-next-key)
         ("M-." . ghostel--send-event)
         ("M-," . ghostel--send-event)
         ("M-/" . ghostel--send-event)
         ;; we don't want this tho
         ("M-o" . ace-window)))

(provide 'lk/terminal)
;;; terminal.el ends here
