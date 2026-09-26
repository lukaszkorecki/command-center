;;; -*- lexical-binding: t; -*-
;;; prog-modes.el --- Programming language modes and configurations
;;; Commentary:
;;; Configures various programming language major modes including Python, JavaScript,
;;; Ruby, Clojure, Go, Terraform, Shell, JSON, YAML, and more.
;;; Language-specific configurations are in separate files (ruby, js, clojure, markdown).

;;; Code:
(require 'lk/utils)

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
  :ensure t)

(use-package nginx-mode
  :ensure t
    :mode "\\.conf$")
  :config ;
  (setq nginx-indent-offset 2)


(use-package yaml-ts-mode
  :ensure t
  :mode ("\\.yml$" "\\.yaml$"))

(use-package swift-mode :ensure t :mode "\\.swift$" )

(use-package json-ts-mode
  :ensure nil
  :mode ("\\.avsc$" "\\.json$")
  :config ;
  (setq js-indent-level 2)
  :hook (json-ts-mode . (lambda () (keymap-local-unset "C-c C-t")))
  :bind (:map json-mode-map (("C-x c f" . json-pretty-print-buffer ))))

(use-package go-ts-mode
  :ensure t
  :mode "\\.go$")

(use-package sqlup-mode
  :ensure t
  :hook (sql-mode . sqlup-mode)
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
  (compilation-start
   (format "shfmt -w -ln bash -i 2 -ci %s"
           (file-relative-name buffer-file-name))
   'compilation-mode)
  (revert-buffer :ignore-auto :noconfirm))

;; NOTE: one block, keyed on `sh-script' -- there is no `sh-mode' feature, so a
;; `use-package sh-mode' declaration never runs its :config or :bind.
;; indent-tabs-mode/tab-width/standard-indent are already set globally in
;; text-editing.el, so they are not repeated here.
(use-package sh-script
  :ensure nil
  :mode (("zshrc" . sh-mode)
         ("\\.sh$" . sh-mode))
  :config ;
  (setq sh-basic-offset 2)
  (setq sh-indent-offset 2)
  :bind (:map sh-mode-map (("C-x c f" . lk/format-current-sh-buffer))))

(use-package hl-todo
  :ensure t
  :diminish hl-todo
  :config (setq hl-todo-highlight-punctuation ":"
                hl-todo-keyword-faces
                `(("TODO" warning bold)
                  ("FIXME" error bold)
                  ("HACK" font-lock-constant-face bold)
                  ("XXX" font-lock-keyword-face bold)
                  ("INFO" success bold)
                  ("NOTE" success bold)))
  :hook ((prog-mode . hl-todo-mode)
         (yaml-mode . hl-todo-mode)))

(use-package lua-ts-mode
  :ensure t
  :mode ("\\.lua$" )
  :config ;
  (setq indent-tabs-mode nil )
  (setq lua-indent-level 2))

(use-package java-ts-mode :ensure nil :mode ("\\.java$" . java-mode))

(use-package toml-ts-mode
  :ensure t
  :mode ("\\.toml$" . toml-ts-mode))

(require 'lk/ruby)
(require 'lk/frontend)
(require 'lk/clojure)
(require 'lk/markdown)

(provide 'lk/prog-modes)
;;; prog-modes.el ends here
