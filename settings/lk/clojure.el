;;; clojure.el --- clojure setup
;;; Commentary:
;; inf-clojure/monroe based clj-scratch buffer
;; Adopted from cider's scratch
;;; Code:
(defconst lk/clj-scratch-name "scratch.clj")

(defun lk/init-clojure-scratch ()
  (interactive)
  (let* ((project-root (projectile-project-root))
         (scratch-file (concat project-root lk/clj-scratch-name)))
    (if (file-exists-p scratch-file)
        (find-file scratch-file)
      (progn (find-file scratch-file)))
    (clojure-mode)))


(defun lk/clojure-scratch ()
  (interactive)
  (pop-to-buffer
   (or (get-buffer lk/clj-scratch-name) (lk/init-clojure-scratch))))

(use-package clojure-mode-extra-font-locking)


(defun lk/failed-tests-in-monroe-repl ()
  (interactive)
  (swiper "\\(FAIL\\|ERROR\\) in .*"))

(use-package monroe
  :init (require 'monroe)
  :config (setq monroe-nrepl-server-cmd "start-clojure-repl-process")
  :bind (:map clojure-mode-map
              (("C-x c j" . monroe-nrepl-server-start)
               ("C-x c m" . monroe)
               ("C-c C-z" . monroe-switch-to-repl)
               ("C-c C-l" . monroe-load-file)
               ("C-x c m" . monroe)
               ("C-x c l" . lk/init-clojure-scratch)))

  (:map monroe-mode-map
        (("C-c n i " . lk/failed-tests-in-monroe-repl)
         ("C-x c s " . lk/clojure-scratch ))))


(defun lk/monroe-kill-all ()
  (interactive)
  (kill-matching-buffers ".*monroe.*" t t)
  (kill-matching-buffers "webkit.*monroe" t t))


(defun lk/clj-mode-hook ()
  (rainbow-delimiters-mode t)
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

(defun lk/open-url (url)
  (message "opening portal %s" url)
  (xwidget-webkit-browse-url url))

(defun lk/monroe-eval-code-and-callback-with-value (code-str on-value)
  (monroe-send-eval-string
   code-str
   (lambda (response)
     (condition-case err
         (monroe-dbind-response response
                                (id info value status)
                                (when (member "done" status)
                                  (remhash id monroe-requests))
                                (when value
                                  (message "value %s" value)
                                  (lk/open-url value)))
       (error (message "error %s" err))))))

(defun lk/monroe-eval-code-and-callback-with-value-2 (code-str on-value)
  (funcall on-value code-str))

(defun lk/monroe-portal-2 ()
  (interactive)
  (lk/monroe-eval-code-and-callback-with-value-2
   "(r/portal-start!)" 'lk/open-url))


(defun lk/monroe-portal ()
  (interactive)
  ;; initiate portal session
  (let* ((project-root (monroe-get-directory))
         (default-directory project-root)
         (portal-url-file (format "%s.portal-url" project-root)))
    (monroe-input-sender
     (get-buffer-process (monroe-repl-buffer))
     (format
      "(require 'portal.api)
         (spit \"%s\"
           (portal.api/url
               (portal.api/open {:window-title \"monroe portal\" :launcher false})))"
      portal-url-file))
    (sleep-for 1) ;; uh... this is a hack
    (let ((url
           (with-temp-buffer
             (insert-file-contents portal-url-file)
             (buffer-string))))
      (xwidget-webkit-browse-url url))))

(defun lk/open-portal ()
  (interactive)
  ;; initiate portal session
  (let* ((project-root
          (locate-dominating-file default-directory "deps.edn"))
         (default-directory project-root)
         (portal-url-file (format ".portal-url" project-root)))
    (monroe-input-sender
     (get-buffer-process (monroe-repl-buffer))
     "(r/portal-start!)")
    (sleep-for 1) ;; uh... this is a hack
    (let ((url
           (with-temp-buffer
             (insert-file-contents portal-url-file)
             (buffer-string))))
      (xwidget-webkit-browse-url url))))

(use-package clojure-mode
  :init (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
  (add-hook 'clojure-mode-hook #'lk/clj-mode-hook)
  :bind (:map clojure-mode-map
              (("C-x c f" . eglot-format)
               ("C-x c p" . lk/open-portal)
               ("C-x c s" . lk/clojure-scratch)
               ("C-x c i" . lk/init-clojure-scratch)
               ("C-x c c" . lk/clear-monroe-repl-from-anywhere)
               ("C-x c C" . lk/clear-monroe-server-buffer-from-anywhere))))

(provide 'lk/clojure)
;;; clojure.el ends here
