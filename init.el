;;;; init --- loads all customizations and packages
;;;; Commentary - Prolly needs splitting more
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/settings")

;; Make all custom executables work in terminal and GUI emacs
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/.DotFiles/bin")

(setenv "PATH" (concat (getenv "PATH")
                       ":/usr/local/bin:~/.DotFiles/bin"))

;; reduce GC thrash
(setq gc-cons-threshold 20000000)
(set-language-environment "UTF-8")
(when (>= emacs-major-version 25)
  (eval-after-load 'bytecomp
    (lambda ()
      (add-to-list 'byte-compile-not-obsolete-funcs
                   'preceding-sexp))))

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'better-defaults)

;; Fix missing env vars in emacs server+ssh
(require 'exec-path-from-shell)
(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

(require 'lk/customizations)
(require 'lk/helm)

;; make it pretty!
(require 'lk/theme)

(require 'ansi-color)
;; yes/no -> y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; turn off splash screen
(setq inhibit-startup-message t)

;; turn off initial scratch buffer message
(setq initial-scratch-message "")

;; strip whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
;; add final newline automaticaly
(setq require-final-newline t)

;; indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default standard-indent 2)

(setq x-alt-keysym 'meta) ; make alt work as meta in x11
(define-key global-map (kbd "RET") 'newline-and-indent)

;; no backup files
(setq-default make-backup-files nil)

(setq echo-keystrokes 0.1
      use-dialog-box nil visible-bell nil)

;; always match parens
(show-paren-mode t)
(setq show-paren-delay 0)

(require 'transpose-frame)

;; load custom abbreviations
(require 'lk/abbrevs)

;; language customizations
(require 'lk/lang)

;; customizations file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; vc mode line needs refreshing every now and then
(setq auto-revert-check-vc-info t)

(require 's)

(defun vc-status-mode-line ()
  "Builds a source control string or nil."
  (when vc-mode
    `(" ["
      ,(s-trim (substring-no-properties vc-mode))
      "] ")))

;; customize the mode-line
(setq-default
 mode-line-format
 (list
  " ⎈ | %l | %c | "
  ;; window number etc
  '(:eval (propertize
           (format " [B: %s] [W: %s] " (lk/count-buffers) (window-number))
           'face 'font-lock-comment-face))
  ;; buffername
  '(:eval (propertize "%b " 'face 'font-lock-keyword-face))
  ;; major mode
  '(:eval (propertize "%m " 'face 'font-lock-comment-face))
  ;; list minor modes
  '(:eavl (propertize 'minor-mode-alist 'face 'font-lock-variable-name-face))
  ;; encoding and line ending
  '(:eval (propertize "%z " 'face 'font-lock-string-face))
  ;; modified * / RO % / no changes -
  '(:eval (propertize " %*" 'face 'font-lock-warning-face))
  '(:eval (vc-status-mode-line))
  '(global-mode-string global-mode-string)))


;; Disable certain commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'magit-clean 'disabled nil)
;; end
(provide 'init)
;;; init.el ends here
