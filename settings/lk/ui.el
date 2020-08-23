;; set font size


(defun on-after-init ()
  (when (display-graphic-p)
    (set-face-attribute 'default nil
                        :family "Monaco"
                        :height 110))

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


(setq echo-keystrokes 0.1
      use-dialog-box nil visible-bell nil)

(use-package highlight-indent-guides
  :ensure t
  :init
  (setq highlight-indent-guides-method 'character)
  (add-hook 'yaml-mode 'highlight-indent-guides-mode))

;; Fix ansi-term rendering
(add-hook 'term-mode-hook 'my-term-mode-hook)
(defun my-term-mode-hook ()
  (setq bidi-paragraph-direction 'left-to-right))


(use-package color-theme-approximate
  :config
  (color-theme-approximate-on))


;; mmmmmmmmmm

(use-package mmm-mode
  :ensure t
  :defer t
  :bind
  (("C-c m m" . mmm-parse-buffer)
   ("C-c m r" . mmm-reparse-current-region))
  :config
  (progn
    (setq mmm-global-mode 'maybe)
      (setq mmm-submode-decoration-level 0)
  (mmm-add-mode-ext-class 'sql-mode "\\.clj$" 'sql-clj)

  (require 'mmm-auto)
  (mmm-add-classes
   '((markdown-clojure
      :submode clojure-mode
      :front "^```clojure[\n\r]+"
      :back "^```$")))
  ;; trying to have sql-in-clojure a bit more usable
  ;; something like this:
  ;;     (def create-users
  ;;   #_sql :start "
  ;; select * from users
  ;; " #_sql :end)

  (mmm-add-classes
     '((sql-clojure
        :submode sql-mode
        :front "^\s*#_sql :start \"[\n\r]+"
        :back "^\s*\"#_sql :end\)$"

        )))
    (mmm-add-mode-ext-class 'clojure-mode nil 'sql-clojure)))




(provide 'lk/ui)
