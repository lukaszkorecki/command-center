;;; lang.el --- programming language customizations
;;; Commentary:
;;; For now all of these are groupped together but if some modes need more
;;; space, they will be moved to separate files

;;; Code:

;; Treesitter

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(mapc #'treesit-install-language-grammar
      (mapcar #'car treesit-language-source-alist))


(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (js2-mode . js-ts-mode)
        (makefile-mode . cmake-ts-mode)
        (elisp-mode . elisp-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (python-mode . python-ts-mode)
        (clojure-mode . clojure-ts-mode)))


;; Utils

(defun lk/invoke-compile-tool-in-project (command-string-with-format)
  (let* ((pj-dir (projectile-acquire-root))
         (default-directory pj-dir))
    (compilation-start
     (format command-string-with-format
             (file-relative-name buffer-file-name))
     'compilation-mode)
    (revert-buffer :ignore-auto :noconfirm)))


(use-package python-mode
  :init (add-to-list 'auto-mode-alist '("\\.py$" . python-mode)))

(use-package poly-markdown
  :init :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md$" . markdown-mode)
   ("\\.markdown$" . markdown-mode)))

(use-package edit-indirect)

(use-package jinja2-mode
  :init (add-to-list 'auto-mode-alist '("\\.j2$" . jinja2-mode)))

(use-package dockerfile-mode

  :init (add-to-list 'auto-mode-alist '("Dockerfile.*" . dockerfile-mode)))

(use-package restclient)

(use-package terraform-mode
  :bind (:map terraform-mode-map (("C-x c f" . terraform-format-buffer))))


(use-package scss-mode
  :init (add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
  (add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
  :config (setq css-indent-offset 2))


(use-package rainbow-mode
  :init (add-to-list 'auto-mode-alist '("\\.scss$" . rainbow-mode))
  (add-to-list 'auto-mode-alist '("\\.css$" . rainbow-mode))
  (rainbow-mode))

(use-package nginx-mode :init (setq nginx-indent-offset 2))

(use-package yaml-mode
  :init (add-to-list 'auto-mode-alist '("\\.yml$". yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$". yaml-mode)))


(use-package json-mode

  :init (add-to-list 'auto-mode-alist '("\\.avsc$" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode)))

;; web-mode stuff
(use-package web-mode
  :init (add-to-list 'auto-mode-alist '("\\.hb$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hb$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
  (setq web-mode-engines-alist '(("jinja"    . "\\.j2\\'"))))

;; sh mode

(defun lk/bash-check-current-buffer ()
  "Run shellcheck on current file"
  (interactive)
  (lk/invoke-compile-tool-in-project "docker run --rm -v $PWD:/mnt koalaman/shellcheck:stable %s"))



(use-package go-mode
  :init (add-to-list 'auto-mode-alist '("\\.go$" . go-mode)))


(use-package sqlup-mode
  :init (add-hook 'sql-mode-hook 'sqlup-mode)
  (mapc
   (lambda (kw)
     (require 'sqlup-mode)
     (add-to-list 'sqlup-blacklist kw))
   '("name" "key" "value" "id"  "source" "type" "to" "user" "at" "role" "current_role" )))


(defun lk/swiper-hugsql-names ()
  "List db function names via swiper when working with HugSQL files."
  (interactive)
  (swiper ":name "))

(add-hook
 'sql-mode-hook
 (lambda () (local-set-key (kbd "C-c n i") 'lk/swiper-hugsql-names)))


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
                  ("NOTE"       success bold)))
  (add-hook 'prog-mode-hook #'hl-todo-mode))


(provide 'lk/lang)
;;; lang.el ends here
