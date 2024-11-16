; -*- lexical-binding: t; -*-
;;; ui.el --- ...
;;; Commentary:

;;; Code:
(setq switch-to-buffer-obey-display-actions t) ;; buffer switching? move to UI
(blink-cursor-mode t) ;; move to UI

(setq default-frame-alist
      '((fullscreen . maximized)
        (vertical-scroll-bars . nil)
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

(defun lk/stanard-font ()
  (interactive)
  (lk/set-frame-font 120))

(defun lk/screen-sharing-font ()
  (interactive)
  (lk/set-frame-font 200))

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

(defun lk/resize-window ()
  (interactive)
  (ivy-read "Resize window to: "
            '(("33%" .
               (lambda () (lk/proportionally-resize-window 0.33)))
              ("50%" .
               (lambda () (lk/proportionally-resize-window 0.50)))
              ("75%" .
               (lambda () (lk/proportionally-resize-window 0.75)))
              ("81 chars" . (lambda () (lk/absolute-resize-window 81)))
              ("121 chars" .
               (lambda () (lk/absolute-resize-window 121))))
            :action #'(lambda (x) (funcall (cdr x)))))


(global-set-key (kbd "C-c r") 'lk/resize-window)

;; Fix ansi-term rendering
(add-hook 'term-mode-hook 'my-term-mode-hook)
(defun my-term-mode-hook ()
  (setq bidi-paragraph-direction 'left-to-right))


(use-package unicode-fonts :config (unicode-fonts-setup))

;; just exit if terminated or C-x C-c is invoked
(setq confirm-kill-processes nil)

(use-package auto-dark :config (auto-dark-mode t))

(defun lk/kill-buffers-by-major-mode (mode)
  (interactive "sMajor mode: ")
  "Kill all buffers in the supplied list."
  (mapcar 'kill-buffer
          (seq-filter
           (lambda (buffer)
             (eq (buffer-local-value 'major-mode buffer) mode))
           (buffer-list))))

(defun lk/clean-up-buffers ()
  (interactive)
  (kill-matching-buffers ".*magit.*" 't 't)
  (kill-matching-buffers ".*grep.*" 't 't)
  (kill-matching-buffers ".*XREF.*" 't 't)
  (lk/kill-buffers-by-major-mode 'dired-mode)
  (kill-matching-buffers ".*occur.*" 't 't)
  (kill-matching-buffers ".*Flymake.*" 't 't))

(provide 'lk/ui)
;;; ui.el ends here
