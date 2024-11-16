;;; navigation.el --- ...
;;; Commentary:

;;; Code:
(use-package project-rootfile :ensure t)

(defvar lk/home-full-path (getenv "HOME"))

(defun lk/project-find-root (path)
  "Search up the PATH for known project file markers. Throws an error if found path is
  equal to users home directory"
  (when-let ((root
              (if-let ((vc-root (project-try-vc default-directory)))
                  ;; get dir from (vc Git "dir")
                  (car (last vc-root))
                ;; this returns #s(project-rootfile-plain "~/tmp/foobar/") struct,
                ;; we need the :root part
                (project-rootfile-plain--root
                 (project-rootfile-try-detect default-directory)))))
    (let* ((root-exp (expand-file-name root)))
      ;; bail if detected dir is equal to home (can happen!)
      (when (string-equal lk/home-full-path root-exp)
        (message "Root folder is equal to HOME!")
        (throw 'lk/invalid-project-root t))
      ;; otherwise we're good
      root-exp)))


(use-package project
  :straight t
  :ensure t
  :after (project-rootfile)
  :init ;

  (advice-add #'project-find-regexp :override #'counsel-git-grep)
  (advice-add #'project-shell :override #'multi-vterm)
  :bind-keymap ("C-c p" . project-prefix-map)

  :config ; add custom actions when using select-project
  (add-to-list 'project-switch-commands
               '(magit-project-status "Magit" ?m)
               '(counsel-git-grep "Git grep" ?g))
  (add-to-list 'project-find-functions #'project-rootfile-try-detect t))

(use-package ibuffer-project :straight t :after (project))

(use-package ibuffer
  :straight t
  :after (project ibuffer-project )
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (setq ibuffer-filter-groups
                          (ibuffer-project-generate-filter-groups))
                    (unless (eq ibuffer-sorting-mode 'project-file-relative)
                      (ibuffer-do-sort-by-project-file-relative))))
  (setq ibuffer-formats
        '((mark
           modified
           read-only
           " "
           (name 18 18 :left :elide)
           " "
           " "
           (mode 16 16 :left :elide)
           " "

           filename-and-process
           " "
           project-file-relative))))

(use-package ivy
  :diminish ivy-mode
  :config ;
  (setq ivy-height 25)
  (ivy-mode 1)
  :custom ;
  (ivy-use-virtual-buffers t)
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


(use-package ace-window
  :config (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  (setq aw-ignore-current nil)
  (setq aw-dispatch-always t)
  (setq aw-minibuffer-flag t)
  (set-face-foreground 'aw-background-face "gray70")
  (ace-window-display-mode t)
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
