
;;; -*- lexical-binding: t; -*-

(setenv "INSIDE_EMACS" "TRUE")

(use-package better-defaults
  :ensure t)

;; reduce GC thrash
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 20000000
      read-process-output-max
      (* 1024 1024))

(when (>= emacs-major-version 25)
  (eval-after-load 'bytecomp
    (lambda ()
      (add-to-list 'byte-compile-not-obsolete-funcs 'preceding-sexp))))


(use-package mise
  :ensure t
  :hook (prog-mode-hook . mise-mode))

;;; startup.el --- Initial environment setup, GC settings, mise
;;; Commentary:
;;; Sets up the initial Emacs environment with better defaults,
;;; configures garbage collection for better performance, and loads mise.

(use-package emacs
  :ensure t
  :custom ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons
     (format "[CRM%s] %s"
             (replace-regexp-in-string
              "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
              crm-separator)
             (car args))
     (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))


;; Persist history over Emacs restarts
(use-package savehist :ensure t :init (savehist-mode))

;; customizations file
(when (file-exists-p "~/.emacs.d/custom.el")
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file))

(setq abbrev-file-name "~/.emacs.d/abbrev.el")
;; make sure transient is available everyhwere
(require 'transient)

(provide 'lk/startup)
;;; startup.el ends here
