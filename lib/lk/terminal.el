;;; -*- lexical-binding: t; -*-
;;; terminal.el --- Terminal and utility tools
;;; Commentary:
;;; Configures vterm for terminal emulation, mermaid for diagrams,
;;; keychain for SSH key management, and time-zones utility.

;;; Code:


;; Trialing ghostel as a vterm replacement — vterm config kept commented
;; out below until the trial wraps up.

;; (defun lk/vterm-project-association ()
;;   "Associate VTerm buffer with the current project."
;;   (when-let* ((project (project-current)))
;;     (setq-local project-current project)))

;; (use-package vterm
;;   :ensure t
;;   :config
;;   (setq vterm-shell "/bin/zsh")
;;   (setq vterm-kill-buffer-on-exit t)
;;   :hook ( vterm-mode-hook  . lk/vterm-project-association)
;;
;;   :bind (("C-c M-o" . vterm-clear-scrollback)
;;          ("C-c ESC o" . vterm-clear-scrollback)
;;          ("C-q" . vterm-send-next-key)))

;; (use-package multi-vterm
;;   :ensure t
;;   :bind (( "C-x t n" . multi-vterm )
;;          ( "C-x t p" . multi-vterm-project )))

;; (defun lk/kill-all-vterms ()
;;   (interactive)
;;   (lk/kill-buffers-by-major-mode 'vterm-mode))

;; (use-package vterm-anti-flicker-filter
;;   :ensure t
;;   :vc (:url "https://github.com/martinbaillie/vterm-anti-flicker-filter"))

(defun lk/ghostel-new ()
  "Create a fresh Ghostel terminal buffer (the `multi-vterm' equivalent)."
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
  :bind (("C-c M-o"   . ghostel-clear-scrollback)
         ("C-c ESC o" . ghostel-clear-scrollback)
         ("C-q"       . ghostel-send-next-key)
         ("C-x t n"   . lk/ghostel-new)
         ("C-x t p"   . ghostel-project)
         ("C-x t o"   . ghostel-other)
         ;; Forward Meta-punctuation to the terminal — ghostel only
         ;; binds M-<letter> by default, so these fall through to
         ;; global bindings (e.g. M-. → xref-find-definitions).
         :map ghostel-mode-map
         ("M-."       . ghostel--send-event)
         ("M-,"       . ghostel--send-event)
         ("M-/"       . ghostel--send-event)))



(provide 'lk/terminal)
;;; terminal.el ends here
