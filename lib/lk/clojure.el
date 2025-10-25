;; -*- lexical-binding: t; -*-
;;; clojure.el --- clojure setup

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
  :after (copilot-mode)
  :init ;
  (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-ts-mode))
  (add-hook 'clojure-mode-hook #'mise-mode)
  (add-hook 'clojure-mode-hook #'copilot-mode)
  (add-hook 'clojure-ts-mode-hook #'mise-mode)
  (add-hook 'clojure-ts-mode-hook #'copilot-mode)

  :config ;
  (setopt clojure-ts-comment-macro-font-lock-body t)
  (setopt clojure-ts-semantic-indent-rules
          '(("defproject" .
             ((:block 1)))
            ("cond" . ((:inner 2)))
            ("cond->" . ((:inner 2)))
            ("cond->>" . ((:inner 2)))
            ("require" . ((:inner 2)))
            ("compile-if" . ((:inner 2))))))

;; (transient-define-prefix lk/cider-transient
;;   ()
;;   "Cider transient commands"
;;   ["Cider"
;;    ("c" "Clear REPL buffer" #'cider-repl-clear-buffer)
;;    ("i" "Inspect" #'cider-inspect)])

(use-package cider
  :ensure t
  :after (clojure-ts-mode)
  :demand t
  :config;
  (unbind-key "C-x s" cider-mode-map)
  (unbind-key "C-x s" cider-repl-mode-map)

  (message
   "CIDER INIT -> %s %s"

   cider-use-xref
   cider-repl-display-help-banner   )

  (setq cider-use-xref nil) ;; use clojure-lsp xref instead
  (setq cider-enable-nrepl-jvmti-agent t)
  (setq cider-repl-display-help-banner nil)
  (setq cider-clojure-cli-aliases ":dev/rumble:dev:test")
  (message
   (format "CIDER INIT -> %s %s"

           cider-use-xref
           cider-repl-display-help-banner))

  :bind ;
  (:map cider-repl-mode-map
        (("C-c M-o" . cider-repl-clear-buffer)
         ("C-c n i " . lk/failed-tests-in-repl-buffer)
         ;; ("C-c c" . lk/cider-transient)
         )))

(use-package kaocha-runner :ensure t :after (cider-mode))

(provide 'lk/clojure)
;;; clojure.el ends here
