(require 'clojure-mode-extra-font-locking)
(add-hook 'clojure-mode-hook #'cider-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'linum-mode)

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

(defun lk/fix-with-cljfmt-and-reload ()
  "Run rubocop -a and reloads the buffer."
  (interactive)
  (save-buffer)
  (start-process "cljfmt-fix"
                 (get-buffer-create "*clj-fmt-fix*")
                 "lein"
                 "cljfmt"
                 "fix"
                 buffer-file-name)
  (revert-buffer t t))

(provide 'lk/clojure)
