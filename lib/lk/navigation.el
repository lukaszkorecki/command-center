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


(defun lk/ibuffer-toggle-never-show ()
  "Clear the list of never show predicates."
  (interactive)

  (if ibuffer-never-show-predicates
      (setq ibuffer-never-show-predicates nil)
    (setq ibuffer-never-show-predicates
          '("^\\*Messages"
            "^\\*Warnings"
            "^\\*Help\\*"
            "^\\*Apropos"
            "^magit"
            "^\\*copilot.events"
            "^\\*EGLOT"
            "^\\*straight"
            "^\\*monroe nrepl server\\*"
            "^\\*monroe-connection")))

  (ibuffer-update nil t))


(use-package ibuffer
  :straight t
  :after (project ibuffer-project)
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("C-c C-t" . lk/ibuffer-toggle-never-show))
  ;; bind in ibuffer mode only?
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

;; Enable Vertico
(use-package vertico :ensure t :init
  (vertico-mode)
  :bind (( "M-RET" . minibuffer-force-complete-and-exit)
         ( "M-TAB"  . minibuffer-complete)))

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
  :bind (("C-c s" . consult-line)          ;; Enhanced search within buffer
         ("C-c n i" . consult-imenu)
         ("M-y" . consult-yank-pop)      ;; Enhanced yank-pop
         ("C-x b" . consult-buffer)      ;; Enhanced buffer switch
         ("M-g M-g" . consult-goto-line))) ;; Enhanced goto line


;; (use-package embark
;;   :ensure t

;;   :bind (("C-." . embark-act)         ;; pick some comfortable binding
;;          ("C-;" . embark-dwim)        ;; good alternative: M-.
;;          ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

;;   :init ;; Optionally replace the key help with a completing-read interface
;;   (setq prefix-help-command #'embark-prefix-help-command)

;;   :config

;;   ;; Hide the mode line of the Embark live/completions buffers
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)         ;; context-sensitive menu
         ("C-;" . embark-dwim)        ;; default action at point
         ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings`

  :init ;; Replace key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; Allow `embark-become` key for switching minibuffer commands (keeps input)
  (define-key minibuffer-local-map (kbd "C-c C-b") #'embark-become)

  ;; Optional: customize common “become” shortcuts for quick switching
  (defvar-keymap embark-become-file+buffer-map
    :doc "Quick Become keymap for buffer/file/line/project switching."
    :parent embark-become-file+buffer-map
    "f" #'find-file
    "p" #'project-switch-project
    "s" #'consult-line
    "x" #'xref-find-definitions)

  ;; Connect it to Embark's become mechanism
  (add-to-list 'embark-become-keymaps 'embark-become-file+buffer-map))


;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Flexible completion matching with Orderless
(use-package orderless
  :after vertico
  :ensure t
  :init (setq completion-styles '(orderless)))



(defun lk/urls-in-buffer->vertico-select->browse ()
  "Find URLs in the current vterm buffer and open the selected one in a browser using vertico/."
  (interactive)

  (let ((urls nil)
        (beg (point-min))
        (end (point-max)))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward
              "\\(https?://\\|ftp://\\|file:///\\)[-A-Za-z0-9+&@#/%?=~_|!:,.;]*[-A-Za-z0-9+&@#/%=~_|]" nil t)
        (push (match-string 0) urls)))
    (when urls
      (message "URLS: %s" urls)
      (let ((selected-url (completing-read "Select URL: " urls nil t)))
        (browse-url selected-url)))))

(global-set-key
 (kbd "C-x c u")
 'lk/urls-in-buffer->vertico-select->browse)

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


(defun lk/kill-dired-buffers ()
  (interactive)
  (mapc
   (lambda (buffer)
     (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
       (kill-buffer buffer)))
   (buffer-list)))


(provide 'lk/navigation)
;;; navigation.el ends here
