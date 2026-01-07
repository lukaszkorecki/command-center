;; -*- lexical-binding: t; -*-
;;; clojure.el --- clojure setup

(require 'lk/utils)

;; helpers

(defun lk/cljfmt-builtin-indents ()
  "Return cljfmt's built-in indent rules as a hash table.
These are the default indents from cljfmt for clojure.core forms."
  (let ((indents (make-hash-table :test 'equal)))
    ;; Extracted from https://github.com/weavejester/cljfmt/blob/master/cljfmt/resources/cljfmt/indents/clojure.clj
    (hput indents 'alt! [[:block 0]])
    (hput indents 'alt!! [[:block 0]])
    (hput indents 'are [[:block 2]])
    (hput indents 'as-> [[:block 2]])
    (hput indents 'binding [[:block 1]])
    (hput indents 'bound-fn [[:inner 0]])
    (hput indents 'case [[:block 1]])
    (hput indents 'catch [[:block 2]])
    (hput indents 'comment [[:block 0]])
    (hput indents 'cond [[:block 0]])
    (hput indents 'condp [[:block 2]])
    (hput indents 'cond-> [[:block 1]])
    (hput indents 'cond->> [[:block 1]])
    (hput indents 'def [[:inner 0]])
    (hput indents 'defmacro [[:inner 0]])
    (hput indents 'defmethod [[:inner 0]])
    (hput indents 'defmulti [[:inner 0]])
    (hput indents 'defn [[:inner 0]])
    (hput indents 'defn- [[:inner 0]])
    (hput indents 'defonce [[:inner 0]])
    (hput indents 'defprotocol [[:block 1] [:inner 1]])
    (hput indents 'defrecord [[:block 2] [:inner 1]])
    (hput indents 'defstruct [[:block 1]])
    (hput indents 'deftest [[:inner 0]])
    (hput indents 'deftype [[:block 2] [:inner 1]])
    (hput indents 'delay [[:block 0]])
    (hput indents 'do [[:block 0]])
    (hput indents 'doseq [[:block 1]])
    (hput indents 'dotimes [[:block 1]])
    (hput indents 'doto [[:block 1]])
    (hput indents 'extend [[:block 1]])
    (hput indents 'extend-protocol [[:block 1] [:inner 1]])
    (hput indents 'extend-type [[:block 1] [:inner 1]])
    (hput indents 'fdef [[:inner 0]])
    (hput indents 'finally [[:block 0]])
    (hput indents 'fn [[:inner 0]])
    (hput indents 'for [[:block 1]])
    (hput indents 'future [[:block 0]])
    (hput indents 'go [[:block 0]])
    (hput indents 'go-loop [[:block 1]])
    (hput indents 'if [[:block 1]])
    (hput indents 'if-let [[:block 1]])
    (hput indents 'if-not [[:block 1]])
    (hput indents 'if-some [[:block 1]])
    (hput indents 'let [[:block 1]])
    (hput indents 'let* [[:block 1]])
    (hput indents 'letfn [[:block 1] [:inner 2 0]])
    (hput indents 'locking [[:block 1]])
    (hput indents 'loop [[:block 1]])
    (hput indents 'match [[:block 1]])
    (hput indents 'ns [[:block 1]])
    (hput indents 'proxy [[:block 2] [:inner 1]])
    (hput indents 'reify [[:inner 0] [:inner 1]])
    (hput indents 'struct-map [[:block 1]])
    (hput indents 'testing [[:block 1]])
    (hput indents 'thread [[:block 0]])
    (hput indents 'try [[:block 0]])
    (hput indents 'use-fixtures [[:inner 0]])
    (hput indents 'when [[:block 1]])
    (hput indents 'when-first [[:block 1]])
    (hput indents 'when-let [[:block 1]])
    (hput indents 'when-not [[:block 1]])
    (hput indents 'when-some [[:block 1]])
    (hput indents 'while [[:block 1]])
    (hput indents 'with-local-vars [[:block 1]])
    (hput indents 'with-open [[:block 1]])
    (hput indents 'with-out-str [[:block 0]])
    (hput indents 'with-precision [[:block 1]])
    (hput indents 'with-redefs [[:block 1]])
    indents))

(defun lk/load-clojure-lsp-indent-rules ()
  "Load indent rules from clojure-lsp config and convert to clojure-ts-mode format.
Reads the EDN config file and transforms cljfmt indents to the format expected by
clojure-ts-semantic-indent-rules. Merges cljfmt built-in defaults with custom :extra-indents."
  (require 'parseedn)
  (let* ((config-file (expand-file-name "etc/clojure-lsp-config.edn" user-emacs-directory))
         (config-data (with-temp-buffer
                        (insert-file-contents config-file)
                        (parseedn-read)))
         ;; parseedn returns (hash-table), so we need to get the first element
         (config (if (listp config-data) (car config-data) config-data))
         (cljfmt (hget config :cljfmt))
         ;; Start with cljfmt built-in defaults
         (all-indents (lk/cljfmt-builtin-indents))
         ;; Get custom extra-indents from config
         (extra-indents (hget cljfmt :extra-indents))
         (rules '()))
    ;; Merge extra-indents on top of built-ins (overrides defaults)
    (when extra-indents
      (maphash (lambda (symbol indent-spec)
                 (hput all-indents symbol indent-spec))
               extra-indents))
    ;; Convert from cljfmt format to clojure-ts-mode format
    ;; cljfmt: symbol [[:inner N]] or [[:block N]] (vector of vectors)
    ;; clojure-ts: ("symbol" . ((:inner N))) or ("symbol" . ((:block N)))
    (maphash (lambda (symbol indent-spec)
               (let* ((symbol-name (symbol-name symbol))
                      ;; Convert vector to list
                      (indent-spec-list (append indent-spec nil))
                      (first-spec (car indent-spec-list))
                      (indent-type (aref first-spec 0))  ; :inner or :block
                      (indent-level (aref first-spec 1)) ; the number
                      (ts-rule (cons symbol-name (list (list (cons indent-type indent-level))))))
                 (push ts-rule rules)))
             all-indents)
    (nreverse rules)))

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

;; a dump way of converting Java import statements to Clojure import forms
;; run at 'import' line, it will search for ';' to the end of the line,
;; then search for '.' to the end of that line, delete the '.' and everything after it,
;; then delete spaces, move to beginning of line
;; e.g. 'import java.util.List;' -> '(import java.util List)
(defalias 'lk/convert-java-import
  (kmacro
   "M-m C-s i m p o r t <return> M-m ( C-s ; <return> <backspace> ) C-r \\ . <return> C-d SPC C-a"))

(use-package clojure-ts-mode
  :after (copilot-mode)
  :mode "\\.clj$"

  :hook ((clojure-mode-hook . mise-mode)
         (clojure-mode-hook . copilot-mode)
         (clojure-ts-mode-hook . mise-mode)
         (clojure-ts-mode-hook . copilot-mode))

  :config
  (setopt clojure-ts-comment-macro-font-lock-body t)
  ;; Load indent rules from clojure-lsp config to keep them in sync
  (setopt clojure-ts-semantic-indent-rules (lk/load-clojure-lsp-indent-rules)))

(use-package cider
  :ensure t
  :demand t

  :hook (cider-mode .
                    (lambda ()
                      (unbind-key "C-x s" cider-mode-map)
                      (unbind-key "C-x s" cider-repl-mode-map)))
  :config (setq cider-use-xref nil) ;; use clojure-lsp xref instead
  (setq cider-enable-nrepl-jvmti-agent t)
  (setq cider-repl-display-help-banner nil)
  (setq cider-clojure-cli-aliases ":dev/rumble:dev:test")

  :bind (:map cider-repl-mode-map
              ("C-c M-o" . cider-repl-clear-buffer)
              ("C-c n i " . lk/failed-tests-in-repl-buffer)))

(use-package kaocha-runner :ensure t :after (cider-mode))

(provide 'lk/clojure)
;;; clojure.el ends here
