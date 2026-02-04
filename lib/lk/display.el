;;; -*- lexical-binding: t -*-
;;; display.el --- Display configuration: frames, fonts, colors, window management
;;; Commentary:
;;; Configures the visual appearance of Emacs including fonts, frame settings,
;;; theme, window management, and custom display functions.

;;; Code:
(setq switch-to-buffer-obey-display-actions t) ;; buffer switching? move to UI
(blink-cursor-mode t) ;; move to UI

(setq warning-minimum-level :error)
(setq ring-bell-function 'ignore)

(setq default-frame-alist
      '((vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (ns-appearance . dark)
        (ns-transparent-titlebar . t)
        (alpha-background . 50)))
(setq echo-keystrokes 0.1 use-dialog-box nil visible-bell nil)
(setq frame-inhibit-implied-resize t)
(setq pixel-scroll-precision-mode t)
(setq-default show-trailing-whitespace nil)
(setq-default indicate-empty-lines nil)
(setq-default indicate-buffer-boundaries 'left)

(defun lk/set-frame-font (height)
  (interactive "nFont size: ")
  (let* ((frame (selected-frame)))
    (set-face-attribute 'default nil :height height)
    (set-frame-font (font-spec :height height) t `(,frame))))

(defvar lk/font-size-regular 120 "Regular font size.")
(defvar lk/font-size-xl 170 "eXtra Large font size.")

(defun lk/standard-font ()
  (interactive)
  (lk/set-frame-font lk/font-size-regular))

(defun lk/screen-sharing-font ()
  (interactive)
  (lk/set-frame-font 170))

(defun on-after-init ()
  (when (display-graphic-p)
    (set-face-attribute 'default nil :family "Menlo" :height lk/font-size-regular))
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)

;; yes/no -> y/n
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)
(setopt use-short-answers t)
;; turn off splash screen
(setq inhibit-startup-message t)

;; turn off the bell
(setq ring-bell-function 'ignore)

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

;; Fix ansi-term rendering
(add-hook 'term-mode-hook 'my-term-mode-hook)
(defun my-term-mode-hook ()
  (setq bidi-paragraph-direction 'left-to-right))

(use-package unicode-fonts :ensure t :config (unicode-fonts-setup))


;; custom transient-back window management thing - transpose/rotate/flip/flop/resize

(use-package transient :ensure t)
(require 'transient)

(defun lk/absolute-resize-window (width)
  "Set the window's size to 80 (or prefix arg WIDTH) columns wide."
  (interactive "P")
  (enlarge-window (- (or width 80) (window-width)) 'horizontal))

(defun lk/proportionally-resize-window (percentage)
  "Set the window's size to percentage of the frame's width."
  (interactive "P")
  (let* ((frame-width (frame-width))
         (current-window-width (window-width))
         (desired-window-width (round (* frame-width percentage)))
         (resize-amount (- desired-window-width current-window-width)))
    (window-resize nil resize-amount t)))

;; preset functions:
(defun lk/resize-window-33pct ()
  (interactive)
  (lk/proportionally-resize-window 0.33))

(defun lk/resize-window-50pct ()
  (interactive)
  (lk/proportionally-resize-window 0.50))

(defun lk/resize-window-75pct ()
  (interactive)
  (lk/proportionally-resize-window 0.75))

(defun lk/resize-window-81chars ()
  (interactive)
  (lk/absolute-resize-window 81))

(defun lk/resize-window-121chars ()
  (interactive)
  (lk/absolute-resize-window 121))

(use-package transpose-frame
  :ensure t
  :config

  (require 'transient)
  (transient-define-prefix lk/window-mgr
    ()
    "Shortcuts for moving windows/frames around"
    ["Window Management"
     ("t" "Transpose" transpose-frame)
     ("r" "Rotate" rotate-frame)
     ("f" "Flip" flip-frame)
     ("F" "Flop" flop-frame)]

    ["Resize Window"
     ("3" "33%"  lk/resize-window-33pct)
     ("5" "50%" lk/resize-window-50pct)
     ("7" "75%" lk/resize-window-75pct)
     ("8" "81 chars" lk/resize-window-81chars)
     ("1" "121 chars" lk/resize-window-121chars)])

  (define-key global-map (kbd "C-c t") 'lk/window-mgr))

(use-package ace-window
  :ensure t
  :config ;
  (setq aw-keys '(?q ?w ?e ?r ?t ?y ?u ?i ?o))
  (setq aw-ignore-current nil)
  (setq aw-dispatch-always t)
  (setq aw-minibuffer-flag t)
  (set-face-foreground 'aw-background-face "gray70")
  (ace-window-display-mode t)
  :hook (term-mode-hook .
                        (lambda ()
                          (define-key term-raw-map (kbd "M-o") 'ace-window)))
  :bind (( "M-o" . ace-window)))

;; Window and buffer management
(global-set-key (kbd "C-x |") 'split-window-horizontally)
(global-set-key (kbd "C-x -") 'split-window-vertically)

(use-package modus-themes
  :ensure t
  :init
  (load-theme 'modus-operandi t))

(provide 'lk/display)
;;; display.el ends here
