;;; /Users/lukasz/.emacs.d/init.el --- /Users/lukasz/.emacs.d/init.el
;;; Commentary:

;;; Code:


;;; Commentary:
;;  loads all customizations and packages

;;; Code:
(load-file "~/.emacs.d/deps.el")

;; customizations file
(when (file-exists-p "~/.emacs.d/custom.el")
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file))

(setq abbrev-file-name "~/.emacs.d/abbrev.el")
(require 'transient)
(require 'lk/boot)

(require 'lk/ui)
(require 'lk/setup-path)

(require 'lk/secrets)

;; saner regex
(require 're-builder)
(setq reb-re-syntax 'string)

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))


(require 'lk/navigation)
(require 'lk/autocomplete)
(require 'copilot)
(require 'lk/git)
(require 'lk/lsp)
(require 'lk/tools)
(require 'lk/editing)
(require 'lk/lang)
(require 'lk/customizations)
(require 'lk/modeline)
(require 'lk/proj-mgr)

(lk/fix-utf)
(provide 'init)
