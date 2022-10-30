;;; lang.el --- programming language customizations
;;; Commentary:
;;; For now all of these are groupped together but if some modes need more
;;; space, they will be moved to separate files

;;; Code:

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
  :bind (:map emacs-lisp-mode-map (("C-x c f" . elfmt))))


(require 'lk/ruby)
(require 'lk/js)
(require 'lk/clojure)

(provide 'lk/lang)
;;; lang.el ends here
