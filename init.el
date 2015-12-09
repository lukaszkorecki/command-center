;;;; init --- loads all customizations and packages
;;;; Commentary - yeah!

(add-to-list 'load-path "~/.emacs.d/settings")

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'better-defaults)
(require 'lk/customizations)

;; use ssh for tramp
(setq tramp-default-method "ssh")

(require 'ansi-color)
;; yes/no -> y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; turn off splash screen
(setq inhibit-startup-message t)

;; turn off initial scratch buffer message
(setq initial-scratch-message "")

;; strip whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default standard-indent 2)
(setq-default fill-column 78)

(define-key global-map (kbd "RET") 'newline-and-indent)

;; no backup files
(setq-default make-backup-files nil)

(when window-system
  (set-fontset-font t 'unicode "Apple Symbols" nil 'prepend)
  (scroll-bar-mode -1)) ; no scrollbars

(setq echo-keystrokes 0.1
      use-dialog-box nil visible-bell nil)

;; always match parens
(show-paren-mode t)

;; load custom abbreviations
(require 'lk/abbrevs)

;; language customizations
(require 'lk/lang)


;; customizations file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; set colors and fonts
(require 'lk/font-face)
(lk/normal-font)

(require 'color-theme)
(color-theme-sanityinc-tomorrow-eighties)

;; customize the mode-line
(setq-default mode-line-format
 (list
  '(:eval (propertize " %l | %c | " 'face 'font-lock-comment-face))
; buffername
  '(:eval (propertize "%b " 'face 'font-lock-keyword-face))
; major mode
  '(:eval (propertize "%m " 'face 'font-lock-comment-face))
; list minor modes
  '(:eavl (propertize 'minor-mode-alist 'face 'font-lock-variable-name-face))
; encoding and line ending
  '(:eval (propertize "%z " 'face 'font-lock-string-face))
  ; modified * / RO % / no changes -
  '(:eval (propertize " %*" 'face 'font-lock-warning-face))
))

(provide 'init)
;;; init.el ends here
