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

(use-package clojure-ts-mode
  :init ;
  (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-ts-mode))
  (add-hook 'clojure-mode-hook #'mise-mode)
  (add-hook 'clojure-mode-hook #'copilot-mode)
  (add-hook 'clojure-ts-mode-hook #'mise-mode)
  (add-hook 'clojure-ts-mode-hook #'copilot-mode)

  :config ;
  (setopt clojure-ts-comment-macro-font-lock-body t)
  (setopt clojure-ts-semantic-indent-rules
          '(("defproject" . ((:block 1)))
            ("cond" . ((:inner 2)))
            ("cond->" . ((:inner 2)))
            ("cond->>" . ((:inner 2)))
            ("require" . ((:inner 2))))))


(use-package cider
  :ensure t
  :after (clojure-ts-mode)
  :init ;
  (setq cider-use-xref nil) ;; use clojure-lsp xref instead
  (setq cider-enable-nrepl-jvmti-agent t)
  (setq cider-repl-display-help-banner nil)
  (setq cider-clojure-cli-aliases ":dev/rumble:dev:test")
  (unbind-key "C-x s" cider-mode-map)
  (unbind-key "C-x s" cider-repl-mode-map)

  (transient-define-prefix lk/cider-transient
    ()
    "Cider transient commands"
    ["Cider"
     ("c" "Clear REPL buffer" cider-repl-clear-buffer)
     ("i" "Inspect" cider-inspect)])


  :bind (:map cider-repl-mode-map
              (("C-c M-o" . cider-repl-clear-buffer)
               ("C-c n i " . lk/failed-tests-in-repl-buffer)
               ("C-c c" . lk/cider-transient))))

(use-package kaocha-runner :after (cider-mode))


(use-package logview :after (cider) :ensure t)


(provide 'lk/clojure)
;;; clojure.el ends here
