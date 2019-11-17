;; set font size
(when (display-graphic-p)
  (set-face-attribute 'default nil
                      :family "Monaco"
                      :height 110))

(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)

(use-package ansi-color
  :ensure t)

;; yes/no -> y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; turn off splash screen
(setq inhibit-startup-message t)

;; turn off the bell
(setq ring-bell-function 'ignore)

;; show buffer file name in title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; turn off initial scratch buffer message
(setq initial-scratch-message "")

;; always match parens
(show-paren-mode t)
(setq show-paren-delay 0)

(use-package rainbow-delimiters
  :ensure t)

;; show imenu-list
(use-package imenu-list
  :ensure t
  :bind (( "C-c n l" . imenu-list-smart-toggle)))

(use-package ace-window
  :ensure t
  :init
  (setq aw-keys  '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-ignore-current t)
  :bind (( "M-o" . ace-window)))

;; Window and buffer management

(global-set-key (kbd "C-x |") 'split-window-horizontally)
(global-set-key (kbd "C-x -") 'split-window-vertically)

(use-package transpose-frame
  :ensure t
  :bind (( "C-c t" . transpose-frame)))

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :bind (( "C-c s t" . dired-sidebar-toggle-sidebar)))

(setq echo-keystrokes 0.1
      use-dialog-box nil visible-bell nil)

(use-package highlight-indent-guides
  :ensure t
  :init
  (setq highlight-indent-guides-method 'character)
  (add-hook 'yaml-mode 'highlight-indent-guides-mode))

(provide 'lk/ui)
