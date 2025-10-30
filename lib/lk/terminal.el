;;; terminal.el --- Terminal and utility tools
;;; Commentary:
;;; Configures vterm for terminal emulation, mermaid for diagrams,
;;; keychain for SSH key management, and time-zones utility.

;;; Code:

(use-package mermaid-mode
  :ensure t
  :config ;
  (setq mermaid-mmdc-location "docker")
  (setq mermaid-flags "run -u 1000 -v /tmp:/tmp ghcr.io/mermaid-js/mermaid-cli/mermaid-cli:9.1.6"))

(defun lk/vterm-project-association ()
  "Associate VTerm buffer with the current project."
  (when-let ((project (project-current)))
    (setq-local project-current project)))

(use-package vterm
  :ensure t
  :config (setq vterm-shell "/bin/zsh")
  (setq vterm-kill-buffer-on-exit t)
  :hook ( vterm-mode-hook  . lk/vterm-project-association)

  :bind (("C-c M-o" . vterm-clear-scrollback)
         ("C-c ESC o" . vterm-clear-scrollback)
         ("C-q" . vterm-send-next-key)))

(use-package multi-vterm
  :ensure t
  :bind (( "C-x t n" . multi-vterm )
         ( "C-x t p" . multi-vterm-project )))

(defun lk/kill-all-vterms ()
  (interactive)
  (lk/kill-buffers-by-major-mode 'vterm-mode))

(use-package keychain-environment
  :ensure t
  :init (keychain-refresh-environment))

(use-package time-zones
  :vc (:url "https://github.com/xenodium/time-zones"))

(provide 'lk/terminal)
;;; terminal.el ends here
