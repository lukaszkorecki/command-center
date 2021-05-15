;;; lang.el --- programming language customizations
;;; Commentary:
;;; For now all of these are groupped together but if some modes need more
;;; space, they will be moved to separate files

;;; Code:

;; Utils

(defun lk/invoke-compile-tool-in-project
    (project-file command-string-with-format)
  (let* ((pj-dir (locate-dominating-file default-directory project-file))
         (default-directory pj-dir))
    (compilation-start
     (format command-string-with-format (file-relative-name buffer-file-name))
     'compilation-mode)
    (revert-buffer :ignore-auto :noconfirm)))

(use-package gitignore-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.gitignore$" . gitignore-mode)))

(use-package python-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode)))

(use-package poly-markdown
  :ensure t
  :init
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md$" . markdown-mode)
         ("\\.markdown$" . markdown-mode)))

(use-package markdown-toc
  :ensure t)

(use-package edit-indirect
  :ensure t)

(use-package jinja2-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.j2$" . jinja2-mode)))

(use-package dockerfile-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile.*" . dockerfile-mode)))

(use-package restclient
  :ensure t)

(use-package terraform-mode
  :ensure t)

(use-package poly-ansible
    :ensure t)

(use-package scss-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
  :config
  (setq css-indent-offset 2))



(use-package rainbow-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.scss$" . rainbow-mode))
  (add-to-list 'auto-mode-alist '("\\.css$" . rainbow-mode))
  (rainbow-mode))

(use-package nginx-mode
  :ensure t
  :init
  (setq nginx-indent-offset 2))

(use-package yaml-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.yml$". yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$". yaml-mode)))


(setq nodejs-path
      (if (string-equal system-type "darwin")
        "/Users/lukasz/bin/node/bin/node"
        "/home/ubuntu/.nvm/versions/node/v10.22.1/bin/node"))

;; Javascripts
(defun lk/prettier-format-current-buffer ()
  (interactive)
  (lk/invoke-compile-tool-in-project "package.json" (concat nodejs-path
                                                            " ./node_modules/.bin/prettier --write %s")))

(defun lk/eslint-check-current-buffer ()
  (interactive)
  (lk/invoke-compile-tool-in-project "package.json" (concat nodejs-path
                                                            " ./node_modules/.bin/eslint --fix %s")))

(use-package rjsx-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx$" . rjsx-mode))
  (setq js-indent-level 2)
  ;; settings for js2 mode
  (add-hook 'js2-mode-hook (lambda () (abbrev-mode)))
  (setq-default js-switch-indent-offset 4)
  (setq-default js2-basic-offset 2)
  (setq-default js2-indent-switch-body t)
  ;; es6 is ok with trailing commas
  (setq-default js2-strict-trailing-comma-warning nil)
  :bind
  (:map js2-mode-map
        (( "C-x c f" . lk/prettier-format-current-buffer ))
        (( "C-x c v" . lk/eslint-check-current-buffer ))))


(use-package eslint-fix
  :ensure t)

(defun lk/tslint-check-current-buffer ()
  (interactive)
  (lk/invoke-compile-tool-in-project "tsconfig.json" "tslint -p ./tsconfig.json %s"))

(use-package typescript-mode
  :ensure t
  :init
  (setq typescript-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx$" . typescript-mode))
  :bind
  (:map typescript-mode-map
        (( "C-x c v" . lk/eslint-check-current-buffer)
         ( "C-x c f" . lk/prettier-format-current-buffer ))))

(use-package json-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.avsc$" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode)))

;; web-mode stuff
(use-package web-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.hb$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hb$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb$" . web-mode)))

;; sh mode

(defun lk/bash-check-current-buffer ()
  "Run shellcheck on current file"
  (interactive)
  (lk/invoke-compile-tool-in-project "." "docker run --rm -v $PWD:/mnt koalaman/shellcheck:stable %s"))



(use-package go-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.go$" . go-mode)))

(use-package lua-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode)))


(use-package sqlup-mode
  :ensure t
  :init
  (add-hook 'sql-mode-hook 'sqlup-mode)
  (mapc (lambda (kw)
          (require 'sqlup-mode)
          (add-to-list 'sqlup-blacklist kw))
        '("name" "key" "value" "id"  "source" "type" "to" "user")))


(use-package polymode
  :ensure t
  :mode ("\.clj$" . poly-clojure-sql-mode)
  :config
  (setq polymode-prefix-key (kbd "C-c n"))
  (define-hostmode poly-clojure-hostmode :mode 'clojure-mode))

(define-hostmode poly-clojure-sql-hostmode
  :mode 'clojure-mode
  )

  (define-innermode poly-sql-expr-clojure-innermode
    :mode 'sql-mode
    :head-matcher ".*\"--SQL"
    :tail-matcher ".*--SQL\""
    :head-mode 'host
    :tail-mode 'host)

(define-polymode poly-clojure-sql-mode
  :hostmode 'poly-clojure-hostmode
  :innermodes '(poly-sql-expr-clojure-innermode)
  ;;(define-key poly-clojure-sql-mode-map (kbd "C-c C-c") 'polymode-eval-chunk)
  )

;; continues in clojure.el!

(require 'lk/ruby)
(require 'lk/clojure)

(provide 'lk/lang)
;;; lang.el ends here
