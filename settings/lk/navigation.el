;;; navigation.el --- ...
;;; Commentary:

;;; Code:

(use-package ivy
  :diminish ivy-mode
  :config (setq ivy-height 25)
  (ivy-mode 1)
  :custom (ivy-use-virtual-buffers t)
  (counsel-switch-buffer-preview-virtual-buffers nil)
  :bind (("C-c s" . swiper)
         ("C-c C-r" . ivy-resume)))

(use-package ivy-xref
  :ensure t
  :init (when
            (>= emacs-major-version 27)
          (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-c n i" . counsel-imenu)
         ("C-c n b" . counsel-ibuffer)
         ("C-c n y" . counsel-yank-pop)
         ("C-c e i" .  counsel-unicode-char)))

(use-package projectile
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)
              (("C-c C-g" . 'projectile-grep)))
  :config (setq projectile-completion-system 'ivy)
  (setq projectile-git-command "git ls-files -z -c --recurse-submodules")
  (add-to-list 'projectile-globally-ignored-directories "vendor")
  (add-to-list 'projectile-globally-ignored-directories ".git")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "target")
  (setq projectile-project-root-functions
        '(projectile-root-local
          projectile-root-top-down
          projectile-root-bottom-up
          projectile-root-top-down-recurring)))

(use-package counsel-projectile :init (counsel-projectile-mode))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich :ensure t :init (ivy-rich-mode 1))

(use-package ace-window
  :config (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  (setq aw-ignore-current nil)
  (setq aw-dispatch-always t)
  (setq aw-minibuffer-flag t)
  (set-face-foreground 'aw-background-face "gray70")
  (ace-window-display-mode)
  :init (add-hook 'term-mode-hook
                  (lambda ()
                    (define-key term-raw-map (kbd "M-o") 'ace-window)))
  :bind (( "M-o" . ace-window)))

;; Window and buffer management
(global-set-key (kbd "C-x |") 'split-window-horizontally)
(global-set-key (kbd "C-x -") 'split-window-vertically)

(use-package transpose-frame :bind (( "C-c t" . transpose-frame)))


(use-package avy
  :bind (("C-c v c" . avy-goto-char-2)
         ("C-c v l" . avy-goto-line)
         ("C-c v w" . avy-goto-word-1)))

(defun lk/kill-dired-buffers ()
  (interactive)
  (mapc
   (lambda (buffer)
     (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
       (kill-buffer buffer)))
   (buffer-list)))

(use-package emamux)

(defun lk/open-locally ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))


(provide 'lk/navigation)
;;; navigation.el ends here
