;;; init.el --- loads all customizations and packages
;;; Commentary:
;;; Poorly reinventing Cask ;-)

;;; Code:
(load-file "~/.emacs.d/package/init.el")
(load-file "~/.emacs.d/package/evil-init.el")
(projectile-global-mode)

(setq-default projectile-use-git-grep 1)

;fonts
(if window-system
    (set-face-attribute 'default nil :font "Menlo")
    (add-to-list 'default-frame-alist '(font . "Menlo-14")))

; line numbers
(global-linum-mode 1)
; colum number s
(column-number-mode 1)
(whitespace-mode 1)

; yes/no -> y/n
(defalias 'yes-or-no-p 'y-or-n-p)

; set colors
(require 'color-theme)
(require 'color-theme-solarized)
(load-theme 'solarized-dark t)
(rainbow-delimiters-mode 1)

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

; no menu
(menu-bar-mode -1)
; no toolbar
(tool-bar-mode -1)
; no scrollbars
(scroll-bar-mode -1)
; no problem

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(background-color "#042028")
 '(background-mode dark)
 '(cursor-color "#708183")
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(foreground-color "#708183")
 '(inhibit-startup-screen t)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "unknown" :family "Monaco"))))
 '(fixed-pitch ((t (:family "Monaco")))))

(provide 'init)

;;; init.el ends here
