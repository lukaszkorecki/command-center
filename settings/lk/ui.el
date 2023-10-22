;;; ui.el --- ...
;;; Commentary:

;;; Code:
(setq switch-to-buffer-obey-display-actions t) ;; buffer switching? move to UI
(blink-cursor-mode -1) ;; move to UI

(setq default-frame-alist '((fullscreen . maximized)
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)
                            (alpha-background . 50)))



(setq frame-inhibit-implied-resize t)
(setq pixel-scroll-precision-mode t)
(setq-default show-trailing-whitespace nil)
(setq-default indicate-empty-lines nil)
(setq-default indicate-buffer-boundaries 'left)

(defun on-after-init ()
  (when (display-graphic-p)
    (set-face-attribute 'default nil :family "JetBrains Mono" :height 120))
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)


;; yes/no -> y/n
(defalias 'yes-or-no-p 'y-or-n-p)

(setq confirm-kill-emacs 'y-or-n-p)

;; turn off splash screen
(setq inhibit-startup-message t)

;; turn off the bell
(setq ring-bell-function 'ignore)

;; eldoc just needs MAX one line
(setq eldoc-echo-area-use-multiline-p nil)

;; show buffer file name in title bar
(setq frame-title-format
      '((:eval
         (if (buffer-file-name)
             (abbreviate-file-name (buffer-file-name))
           "%b"))))

;; turn off initial scratch buffer message
(setq initial-scratch-message "")

;; always match parens
(show-paren-mode t)
(setq show-paren-delay 0)
(global-set-key (kbd "C-c f") 'toggle-frame-maximized)

(use-package rainbow-delimiters)


(setq echo-keystrokes 0.1 use-dialog-box nil visible-bell nil)

;; Fix ansi-term rendering
(add-hook 'term-mode-hook 'my-term-mode-hook)
(defun my-term-mode-hook ()
  (setq bidi-paragraph-direction 'left-to-right))


(when (not (string-equal system-type "darwin"))
  (use-package color-theme-approximate
    :config (color-theme-approximate-on)))

(use-package bufler
  :init (require 'bufler)
  :bind (("C-x C-b" . bufler-list)))

(use-package unicode-fonts :config (unicode-fonts-setup))

;; just exit if terminated or C-x C-c is invoked
(setq confirm-kill-processes nil)

(use-package twilight-bright-theme
  :init (load-theme 'twilight-bright t))


(defun lk/clean-up-buffers ()
  (interactive)
  (kill-matching-buffers ".*magit.*" 't 't)
  (kill-matching-buffers ".*grep.*" 't 't)
  (kill-matching-buffers ".*flymake.*" 't 't))


(provide 'lk/ui)
;;; ui.el ends here
