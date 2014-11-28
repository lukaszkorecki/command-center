;;; init.el --- loads all customizations and packages
;;; Commentary:
;;; Poorly reinventing Cask ;-)

;;; Code:
(load-file "~/.emacs.d/package/init.el")
(load-file "~/.emacs.d/package/evil-init.el")
(load-file "~/.emacs.d/settings/font-face.el")
(projectile-global-mode)

(setq-default projectile-use-git-grep 1)

;fonts

; line numbers
(global-linum-mode 1)
; colum number s
(column-number-mode 1)
(whitespace-mode 1)

; yes/no -> y/n
(defalias 'yes-or-no-p 'y-or-n-p)

; set colors
(require 'color-theme)
(load-theme 'sanityinc-tomorrow-night)

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
(setq make-backup-files nil)

(if window-system
    (menu-bar-mode -1) ; no menu
  (tool-bar-mode -1) ; no toolbar
  (scroll-bar-mode -1)) ; no scrollbars

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell nil)

; always match parens
(show-paren-mode t)

; make clipboard work
(setq x-select-enable-clipboard 0)

; load custom abbreviations
(load-file "~/.emacs.d/abbrevs.el")

; language customizations
(load-file "~/.emacs.d/settings/lang.el")


; load flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

; customizations file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(lk-normal-font)

(provide 'init)

;;; init.el ends here
