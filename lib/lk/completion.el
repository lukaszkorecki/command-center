;;; -*- lexical-binding: t; -*-
;;; completion.el - various completing things

;; Enable Vertico
(use-package vertico
  :ensure t
  :init (vertico-mode)
  :bind (( "M-RET" . minibuffer-force-complete-and-exit)
         ( "M-TAB"  . minibuffer-complete)))

;; Enable richer annotations with Marginalia
(use-package marginalia
  :ensure t
  :after vertico
  :init (marginalia-mode))

;; Enhanced navigation and search commands with Consult
(use-package consult
  :ensure t
  :after vertico
  :bind (("C-c s" . consult-line)          ;; Enhanced search within buffer
         ("C-c n i" . consult-imenu)
         ("M-y" . consult-yank-pop)      ;; Enhanced yank-pop
         ("C-x b" . consult-buffer)      ;; Enhanced buffer switch
         ("M-g M-g" . consult-goto-line))) ;; Enhanced goto line

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
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles partial-completion))))
  ;; Disable defaults, use our settings
  (completion-category-defaults nil)
  ;; we're on Emacs 31 -  partial-completion behaves like substring
  (completion-pcm-leading-wildcard t))

(use-package corfu :ensure t :init (global-corfu-mode))

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

(provide 'lk/completion)
