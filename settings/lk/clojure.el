(require 'clojure-mode-extra-font-locking)

(defun lk/clj-mode-hook ()
  (cider-mode t)
  (rainbow-delimiters-mode t)
  (linum-mode 1)
  (clj-refactor-mode 1))

(add-hook 'clojure-mode-hook #'lk/clj-mode-hook)

;; disable cider welcome message
(setq cider-repl-display-help-banner nil)

;; add compojure support

(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))

(defun lk/connect-to-project-nrepl ()
  (interactive)
  (let* ((project-path (expand-file-name (read-file-name "Project root:")))
         (port-file-path (format "%s/.nrepl-port" project-path))
         (port (with-temp-buffer
                 (insert-file-contents port-file-path)
                 (string-to-number (car (split-string (buffer-string) "\n"))))))
    (cider-connect "127.0.0.1" port project-path)))

(global-set-key (kbd "C-x c l") 'lk/connect-to-project-nrepl)

(global-set-key (kbd "C-x c s") 'cider-scratch)
(global-set-key (kbd "C-x c f") 'cider-format-buffer)


(provide 'lk/clojure)
