;;;; init --- loads all customizations and packages
;;;; Commentary - yeah!

(add-to-list 'load-path "~/.emacs.d/settings")

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'better-defaults)
(require 'lk/helm)
(require 'lk/customizations)

;; make it pretty!
(setq linum-format " \u2502 ")
(setq linum-format "%4d \u2502 ")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

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
;; show over 80chars

(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;; indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default standard-indent 2)

(setq x-alt-keysym 'meta) ; make alt work as meta in x11
(define-key global-map (kbd "RET") 'newline-and-indent)

;; no backup files
(setq-default make-backup-files nil)

(when window-system
  (set-fontset-font t 'unicode "Apple Symbols" nil 'prepend)
  (scroll-bar-mode -1)
   (add-to-list  'default-frame-alist '(font  . "Droid Sans Mono"))
   (set-face-attribute 'default ' t :font "Droid Sans Mono"))


(setq echo-keystrokes 0.1
      use-dialog-box nil visible-bell nil)

;; always match parens
(show-paren-mode t)

(require 'transpose-frame)

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

(require 'lk/eyebrowse)

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
(setq-default mode-line-format
 (list
   " ⎈ | %l | %c | "
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

  ;; eyebrowse and buffer count status
  '(:eval (propertize
           (format " [B: %s] " (lk/count-buffers))
           'face 'font-lock-comment-face))
  '(:eval (propertize
          (eyebrowse-mode-line-indicator)
          'face 'font-lock-function-face))))


(provide 'init)
;;; init.el ends here
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
