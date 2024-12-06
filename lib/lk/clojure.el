; -*- lexical-binding: t; -*-
;;; clojure.el --- clojure setup
;;; Commentary:
;; inf-clojure/monroe based clj-scratch buffer
;; Adopted from cider's scratch
;;; Code:


(use-package clojure-mode
  :straight (:host github :repo "clojure-emacs/clojure-mode")
  :init (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
  (add-hook 'clojure-mode-hook #'lk/clj-mode-hook))


(defconst lk/clj-scratch-name "scratch.clj")

(defun lk/init-clojure-scratch ()
  (interactive)
  (let* ((project-root (lk/project-find-root nil))
         (scratch-file
          (format "%s%s" project-root lk/clj-scratch-name)))
    (find-file scratch-file)
    (get-buffer lk/clj-scratch-name)))


(defun lk/clojure-scratch ()
  (interactive)
  (pop-to-buffer
   (or (get-buffer lk/clj-scratch-name) (lk/init-clojure-scratch))
   'display-buffer-reuse-window))


;; (use-package clojure-mode-extra-font-locking)


(defun lk/failed-tests-in-monroe-repl ()
  (interactive)
  (consult-line "\\(FAIL\\|ERROR\\) in"))

(defun lk/switch-to-monroe-repl-or-connect-or-start ()
  (interactive)
  (if (get-buffer (format "*%s*" monroe-nrepl-server-buffer-name ))
      (let* ((nrepl-addr (monroe-locate-running-nrepl-host)))
        (if nrepl-addr
            (if-let (b (monroe-repl-buffer))
                ;; switch to repl
                (pop-to-buffer b)
              ;; we need to connect - server is possibly running
              (monroe nrepl-addr))
          ;; else
          (error
           (format ".nrepl-port file not found in %s" default-directory))))
    ;; else
    (and
     (message "Monroe server buffer not found, gonna start it and you need to try again")
     (monroe-nrepl-server-start))))

(use-package monroe
  :init (require 'monroe)
  :config (setq monroe-nrepl-server-cmd "start-clojure-repl-process")
  ;; optimize memory usage for the REPL buffer, we don't really need undo here
  (add-hook 'monroe-mode-hook
            (lambda () (setq-local undo-outer-limit 10000)))
  :bind (:map clojure-mode-map
              ("C-c C-c" . monroe-eval-expression-at-point)
              ("C-x c j" . lk/switch-to-monroe-repl-or-connect-or-start)
              ("C-x c m" . lk/switch-to-monroe-repl-or-connect-or-start)
              ("C-c C-z" . lk/switch-to-monroe-repl-or-connect-or-start)
              ("C-c C-l" . monroe-load-file)
              ("C-x c l" . lk/init-clojure-scratch)
              ("C-x c s " . lk/clojure-scratch ))
  (:map monroe-mode-map
        (("C-c n i " . lk/failed-tests-in-monroe-repl)
         ("C-x c s " . lk/clojure-scratch ))))


(defun lk/monroe-kill-all ()
  (interactive)
  (kill-matching-buffers ".*monroe.*" t t)
  (kill-matching-buffers "webkit.*monroe" t t))


(defun lk/clj-mode-hook ()
  (require 'monroe)
  (clojure-enable-monroe))

(defun clear-comint-buffer-by-match (buffer-regexp)
  (let* ((repl-buffers
          (cl-remove-if-not
           (lambda (buffer)
             (string-match-p buffer-regexp (buffer-name buffer)))
           (buffer-list))))
    (dolist (repl-buffer repl-buffers)
      (with-current-buffer repl-buffer (comint-clear-buffer)))))

(defun lk/clear-monroe-repl-from-anywhere ()
  "Clear the NREPL server buffer."
  (interactive)
  (clear-comint-buffer-by-match  "\\*monroe: localhost:.*\\*"))


(defun lk/clear-monroe-server-buffer-from-anywhere ()
  "Clear the Clojure REPL buffer."
  (interactive)
  (clear-comint-buffer-by-match  ".*monroe nrepl server.*"))

(defun lk/monroe-eval-code-and-callback-with-value (code-str on-value)
  (monroe-send-eval-string
   code-str
   (lambda (response)
     (condition-case err
         (monroe-dbind-response response
                                (value status id)
                                (when value (funcall on-value value))
                                (when (member "done" status)
                                  (remhash id monroe-requests)))
       (error (message "Eval error %s" err))))))


(defun lk/monroe-portal-start! ()
  (interactive)
  (lk/monroe-eval-code-and-callback-with-value
   "(do
      (require 'r 'r.portal)
      (r.portal/start! {:force? true :browse? false}))"
   (lambda (value)
     (condition-case err
         ;; value is a raw string, so we need to remove " from it
         (let ((url (string-replace "\"" "" value)))
           (message "Opening portal %s" url)
           (condition-case err2
               (xwidget-webkit-browse-url url)
             (error (message "Browse error %s" err2))))
       (error (message "Portal start error %s" err))))))

(defun lk/portal-clear! ()
  (interactive)
  (lk/eval-code-and-callback-with-value
   "(r.portal/clear!)"
   (lambda (value) (message "Portal cleared"))))

(provide 'lk/clojure)
;;; clojure.el ends here
