;; -*- lexical-binding: t; -*-
;;; clojure.el --- clojure setup
;;; Commentary:
;; inf-clojure/monroe based clj-scratch buffer
;; Adopted from cider's scratch
;;; Code:

;; helpers
(defun lk/failed-tests-in-repl-buffer ()
  (interactive)
  (consult-line "\\(FAIL\\|ERROR\\).in."))


(defun lk/cider-kill-all ()
  (interactive)
  (kill-matching-buffers ".*nrepl.*" t t)
  (kill-matching-buffers ".*cider.*" t t))


(defun lk/portal-open ()
  (interactive)
  (cider-nrepl-sync-request:eval
   "(do (require 'r 'r.portal 'r.webview) (r.webview/browse (r.portal/start! {:launcher false :browse? false})))"))

(defun lk/portal-clear ()
  (interactive)
  (cider-nrepl-sync-request:eval "(r.portal/clear!)"))

(defun lk/portal-close ()
  (interactive)
  (cider-nrepl-sync-request:eval "(r.portal/close!)"))

;; packages

(use-package clojure-mode
  ;; :straight (:host github :repo "clojure-emacs/clojure-mode")
  :init ;
  (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode))


(use-package cider
  :ensure t
  :after (clojure-mode)
  :init ;
  (setq cider-enable-nrepl-jvmti-agent t)
  :bind (("C-x s" . cider-scratch)
         ("C-x c j" . cider-jack-in)
         ("C-x p o" . lk/portal-open)
         ("C-x p c" . lk/portal-clear)
         ("C-x p q" . lk/portal-close)
         ("C-c C-d" . cider-doc)
         ("C-c C-e" . cider-eval-last-sexp)
         ("C-c C-r" . cider-eval-region)
         ("C-c C-b" . cider-eval-buffer)
         ("C-c C-z" . cider-switch-to-repl-buffer)))

(use-package kaocha-runner
  :after (cider-mode))

(provide 'lk/clojure)
;;; clojure.el ends here
