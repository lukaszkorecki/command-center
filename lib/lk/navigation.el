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

  (advice-add #'project-find-regexp :override #'consult-git-grep)
  (advice-add #'project-shell :override #'multi-vterm)
  :bind-keymap ("C-c p" . project-prefix-map)

  :config ; add custom actions when using select-project
  (add-to-list 'project-switch-commands
               '(magit-project-status "Magit" ?m)
               '(consult-git-grep "Git grep" ?g))
  (add-to-list 'project-find-functions #'project-rootfile-try-detect t))

(use-package ibuffer-project :straight t :after (project))

(use-package ibuffer
  :straight t
  :after (project ibuffer-project)
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (setq ibuffer-filter-groups
                          (ibuffer-project-generate-filter-groups))
                    (unless (eq ibuffer-sorting-mode 'project-file-relative)
                      (ibuffer-do-sort-by-project-file-relative))))

  ;; built-in
  (add-to-list 'ibuffer-never-show-predicates "^\\*Messages")
  (add-to-list 'ibuffer-never-show-predicates "^\\*Warnings")
  (add-to-list 'ibuffer-never-show-predicates "^\\*Help\\*")
  (add-to-list 'ibuffer-never-show-predicates "^\\*Apropos")
  ;; major modes
  (add-to-list 'ibuffer-never-show-predicates "^magit")
  (add-to-list 'ibuffer-never-show-predicates "^\\*copilot.events")
  (add-to-list 'ibuffer-never-show-predicates "^\\*EGLOT")
  (add-to-list 'ibuffer-never-show-predicates "^\\*straight")
  (add-to-list 'ibuffer-never-show-predicates "^\\*monroe nrepl server\\*")
  (add-to-list 'ibuffer-never-show-predicates "^\\*monroe-connection")
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

;; Enable Vertico
(use-package vertico :ensure t :init (vertico-mode))

;; Enable richer annotations with Marginalia
(use-package marginalia
  :ensure t
  :after vertico
  :init (marginalia-mode))


(use-package emacs
  :custom ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons
     (format "[CRM%s] %s"
             (replace-regexp-in-string
              "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
              crm-separator)
             (car args))
     (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))


;; Enhanced navigation and search commands with Consult
(use-package consult
  :ensure t
  :after vertico
  :bind (("C-s" . consult-line)          ;; Enhanced search within buffer
         ("C-c n i" . consult-imenu)
         ("M-y" . consult-yank-pop)      ;; Enhanced yank-pop
         ("C-x b" . consult-buffer)      ;; Enhanced buffer switch
         ("M-g g" . consult-goto-line))) ;; Enhanced goto line

;; Flexible completion matching with Orderless
(use-package orderless
  :ensure t
  :init (setq completion-styles '(orderless)))

;; Persist history over Emacs restarts
(use-package savehist :init (savehist-mode))

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




(defun lk/kill-dired-buffers ()
  (interactive)
  (mapc
   (lambda (buffer)
     (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
       (kill-buffer buffer)))
   (buffer-list)))

(defun lk/open-locally ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))


(provide 'lk/navigation)
;;; navigation.el ends here
