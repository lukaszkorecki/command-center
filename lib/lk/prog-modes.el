;;; -*- lexical-binding: t; -*-
;;; prog-modes.el --- Programming language modes and configurations
;;; Commentary:
;;; Configures various programming language major modes including Python, JavaScript,
;;; Ruby, Clojure, Go, Terraform, Shell, JSON, YAML, and more.
;;; Language-specific configurations are in separate files (ruby, js, clojure, markdown).

;;; Code:

;; When saving a file that starts with `#!', make it executable.
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; saner regex
(require 're-builder)
(setq reb-re-syntax 'string)

(defun lk/invoke-compile-tool-in-project (command-string-with-format)
  (let* ((pj-dir (lk/project-find-root nil))
         (default-directory pj-dir))
    (compilation-start
     (format command-string-with-format
             (file-relative-name buffer-file-name))
     'compilation-mode)
    (revert-buffer :ignore-auto :noconfirm)))

(use-package python-mode
  :ensure t
  :config ;
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode)))

(use-package jinja2-mode :ensure t :mode ("\\.j2$"))

(use-package dockerfile-mode :ensure t :mode ("Dockerfile.*"))

(use-package restclient :ensure t :mode ("\\.restclient\\'"))

(use-package terraform-mode
  :ensure t
  :bind (:map terraform-mode-map (("C-x c f" . terraform-format-buffer))))

(use-package nginx-mode
  :ensure t
  :config ;
  (setq nginx-indent-offset 2)
  :mode "\\.conf$")

(use-package yaml-mode :ensure t :mode ("\\.yml$" "\\.yaml$"))

(use-package swift-mode :ensure t :mode "\\.swift$" )

(use-package json-mode
  :ensure t
  :mode ("\\.avsc$" "\\.json$")
  :config (setq js-indent-level 2)
  :hook (json-mode . (lambda () (keymap-local-unset "C-c C-t")))
  :bind (:map json-mode-map
              (("C-x c f" . json-pretty-print-buffer )
               ("C-c C-t" . copilot-complete-at-point))))

(use-package go-mode :ensure t :mode "\\.go$"  )

(use-package sqlup-mode
  :ensure t
  :hook (sql-mode-hook . sqlup-mode)
  :config ;; Add keywords to blacklist, preventing duplicates with dolist
  (require 'sqlup-mode)
  (dolist (kw
           '("name" "key" "value" "id" "source" "type" "to" "user" "at" "role" "current_role"))
    (add-to-list 'sqlup-blacklist kw)))

;; formatter for elisp

(use-package elfmt
  :ensure t
  :defer t
  :vc (:url  "https://github.com/riscy/elfmt" )
  :bind (:map emacs-lisp-mode-map
              (("C-x c f" . elfmt)
               ("C-x c e" . eval-region))))

(defun lk/format-current-sh-buffer ()
  "Run shellcheck on current file"
  (interactive)
  (lk/invoke-compile-tool-in-project "shfmt -w -ln bash -i 2 -ci %s"))

(use-package sh-mode
  :ensure nil
  :mode ("zshrc" "\\.sh$")

  :config (setq sh-basic-offset 2)
  (setq indent-tabs-mode nil)

  (setq tab-width 2)
  (setq standard-indent 2)

  :bind (:map sh-mode-map (("C-x c f" . lk/format-current-sh-buffer))))

(use-package graphql-mode
  :ensure t
  :mode( "\\.graphql$" "\\.gql$" )
  :config (setq graphql-indent-level 2))

(use-package hl-todo
  :ensure t
  :diminish hl-todo
  :config (setq hl-todo-highlight-punctuation ":"
                hl-todo-keyword-faces
                `(("TODO"       warning bold)
                  ("FIXME"      error bold)
                  ("HACK"       font-lock-constant-face bold)
                  ("XXX"     font-lock-keyword-face bold)
                  ("INFO"       success bold)
                  ("NOTE"       success bold)))
  :hook ((prog-mode-hook . hl-todo-mode)
         (yaml-mode-hook  .hl-todo-mode)))

(use-package lua-mode
  :ensure t
  :mode ("\\.lua$" )
  :config (setq indent-tabs-mode nil )
  (setq lua-indent-level 2))

(use-package sh-script
  :ensure nil
  :mode ("\\.sh$" . sh-mode)
  :config (setq sh-indent-offset 2)
  (setq sh-indentation 2))

(use-package java-ts-mode
  :ensure t
  :mode ("\\.java$" )

  :preface ; nofmt
  (dolist (mapping '((java-mode . java-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  :init ;; Set up treesit language sources early to avoid duplicates
  (add-to-list 'treesit-language-source-alist
               '(java . ("https://github.com/tree-sitter/tree-sitter-java" "v0.23.5" "src"))))

(use-package mermaid-mode
  :ensure t
  :config ;
  (setq mermaid-mmdc-location "docker")
  (setq mermaid-flags "run -u 1000 -v /tmp:/tmp ghcr.io/mermaid-js/mermaid-cli/mermaid-cli:9.1.6"))

(require 'lk/ruby)
(require 'lk/frontend)
(require 'lk/clojure)
(require 'lk/markdown)

(provide 'lk/prog-modes)
;;; prog-modes.el ends here
