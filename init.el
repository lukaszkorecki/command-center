;;;; init --- loads all customizations and packages
;;; Commentary:
;;; Poorly reinventing Cask ;-)

;;; Code:
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/settings")

(require 'cask "~/.cask/cask.el")
(cask-initialize)


(require 'lk/font-face)
(require 'lk/customizations)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(setq projectile-use-git-grep t)
(setq projectile-completion-system 'grizzl)
; use ssh for tramp

(setq tramp-default-method "ssh")
; line numbers
(global-linum-mode 1)
; colum number s
(column-number-mode 1)
; whitespaces
(whitespace-mode 1)
; ansi
(require 'ansi-color)
; yes/no -> y/n
(defalias 'yes-or-no-p 'y-or-n-p)

; set colors
(require 'color-theme)


; strip whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default standard-indent 2)
(setq-default fill-column 78)

(define-key global-map (kbd "RET") 'newline-and-indent)

; highlight current line
(global-hl-line-mode 1)

; no backup files
(setq-default make-backup-files nil)

(when window-system
  (set-fontset-font t 'unicode "Apple Symbols" nil 'prepend)
  (scroll-bar-mode -1)) ; no scrollbars

(menu-bar-mode -1) ; no menu
(tool-bar-mode -1) ; no toolbar

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell nil)

; always match parens
(show-paren-mode t)

; make clipboard work
(setq x-select-enable-clipboard 0)

; load custom abbreviations
(require 'lk/abbrevs)

; language customizations
(require 'lk/lang)

; load flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

; customizations file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(lk/normal-font)

; customize the mode-line
(setq-default
 mode-line-format
 (list
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
