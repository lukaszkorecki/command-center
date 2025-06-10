;;; lang.el --- programming language customizations
;;; Commentary:
;;; For now all of these are groupped together but if some modes need more
;;; space, they will be moved to separate files

;;; Code:

;; (use-package bash-ts-mode
;;   :straight  (:host github :repo "tree-sitter/tree-sitter-bash")
;;   :ensure t
;;   :init (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode)))

;; Utils

;; When saving a file that starts with `#!', make it executable.
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")

  :init (global-treesit-fold-mode t)
  :bind (("C-c \\" . treesit-fold-toggle)))

(use-package aggressive-indent
  :ensure t
  :init (add-hook 'prog-mode-hook #'aggressive-indent-mode)
  (add-hook 'makefile-mode-hook
            (lambda () (aggressive-indent-mode -1)))
  (add-hook 'dockerfile-mode-hook
            (lambda () (aggressive-indent-mode -1))))

(defun lk/invoke-compile-tool-in-project (command-string-with-format)
  (let* ((pj-dir (lk/project-find-root nil))
         (default-directory pj-dir))
    (compilation-start
     (format command-string-with-format
             (file-relative-name buffer-file-name))
     'compilation-mode)
    (revert-buffer :ignore-auto :noconfirm)))



(use-package python-mode
  :init ;
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode)))

;; helpers for markdown and writing in general
(defun lk/insert-current-date ()
  "Insert current date at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun lk/insert-current-date-time ()
  "Insert current date at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M")))

(defun lk/insert-random-uuid ()
  "Insert a random UUID at point."
  (interactive)
  (insert (uuid-string)))


(defun lk/insert-md-callout (callout-type)
  "Insert a markdown callout of type CALLOUT-TYPE at point."
  (interactive "sCallout type: ")
  (insert (format "> [!%s]\n" callout-type)))

(defun lk/gh-preview-markdown ()
  "Preview markdown file in browser by rendeding it to html using `ghmd-preview` script
  which returns the local file path of the rendered html."
  (interactive)
  (let* ((file-name (buffer-file-name))
         ;;         (tmp-file-name (format "%s.html" (make-temp-file "ghmd-preview")))
         ;; create preview next to the original file but with .html extension added)
         (preview-file-name (concat file-name ".html"))
         (html-file
          (shell-command-to-string
           (format "ghmd-preview -f %s -o %s" file-name preview-file-name)))
         (browseable-file-path (format "file://%s" html-file)))
    (message "Previewing %s" browseable-file-path)
    ;; (xwidget-webkit-browse-url browseable-file-path)
    (browse-url browseable-file-path)))

(use-package markdown-mode
  :ensure t
  :after (copilot)
  :config ;;
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

  (setq markdown-command "~/.emacs.d/etc/bin/markdown")
  (setq markdown-command-needs-filename t)


  (lk/cleanup-keymap markdown-mode-map)

  :bind (:map markdown-mode-map
              (("C-c m c" . lk/insert-md-callout)
               ("C-c m d" . lk/insert-current-date)
               ("C-c m t" . lk/insert-current-date-time)
               ("C-c m p" .  lk/gh-preview-markdown))))

(use-package poly-markdown
  :after (markdown-mode)
  :init :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md$" . markdown-mode)
   ("\\.markdown$" . markdown-mode)))

(use-package edit-indirect)

(use-package jinja2-mode
  :init (add-to-list 'auto-mode-alist '("\\.j2$" . jinja2-mode)))

(use-package dockerfile-mode
  :init (add-to-list 'auto-mode-alist '("Dockerfile.*" . dockerfile-mode)))

(use-package restclient
  :ensure t
  :init (add-to-list 'auto-mode-alist
                     '("\\.restclient\\'" . restclient-mode)))

(use-package terraform-mode
  :bind (:map terraform-mode-map (("C-x c f" . terraform-format-buffer))))


(use-package nginx-mode :init (setq nginx-indent-offset 2))

(use-package yaml-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.yml$". yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$". yaml-mode)))

(use-package swift-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.swift$" . swift-mode)))

(use-package json-mode
  :init (add-to-list 'auto-mode-alist '("\\.avsc$" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
  (setq js-indent-level 2)
  (keymap-local-unset "C-c C-t")
  :bind (:map json-mode-map
              (("C-x c f" . json-pretty-print-buffer )
               ("C-c C-t" . copilot-complete-at-point))))

;; web-mode stuff
(use-package web-mode
  :init (add-to-list 'auto-mode-alist '("\\.hb$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hb$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
  (setq web-mode-engines-alist '(("jinja"    . "\\.j2\\'"))))

;; sh mode

(defun lk/format-current-buffer ()
  "Run shellcheck on current file"
  (interactive)
  (lk/invoke-compile-tool-in-project "shfmt -w -ln bash -i 2 -ci %s"))

(add-hook 'sh-mode-hook
          (lambda ()
            (progn
              (copilot-mode 't)
              (setq sh-basic-offset 2)
              (keymap-local-unset "C-c C-t")
              (define-key sh-mode-map
                          (kbd "C-x c f")
                          'lk/format-current-buffer))))


(use-package go-mode
  :init (add-to-list 'auto-mode-alist '("\\.go$" . go-mode)))


(use-package sqlup-mode
  :init (add-hook 'sql-mode-hook 'sqlup-mode)
  (mapc
   (lambda (kw)
     (require 'sqlup-mode)
     (add-to-list 'sqlup-blacklist kw))
   '("name" "key" "value" "id"  "source" "type" "to" "user" "at" "role" "current_role" )))



;; formatter for elisp

(use-package elfmt
  :straight (:host github :repo "riscy/elfmt" :branch "master")
  :bind (:map emacs-lisp-mode-map
              (("C-x c f" . elfmt)
               ("C-x c e" . eval-region))))


(require 'lk/ruby)
(require 'lk/js)
(require 'lk/clojure)

(use-package hl-todo
  :diminish hl-todo
  :config (setq hl-todo-highlight-punctuation ":"
                hl-todo-keyword-faces
                `(("TODO"       warning bold)
                  ("FIXME"      error bold)
                  ("HACK"       font-lock-constant-face bold)
                  ("XXX"     font-lock-keyword-face bold)
                  ("INFO"       success bold)
                  ("NOTE"       success bold)))
  (add-hook 'prog-mode-hook #'hl-todo-mode)
  (add-hook 'yaml-mode-hook #'hl-todo-mode))


(provide 'lk/lang)
;;; lang.el ends here
