(require 'clojure-mode-extra-font-locking)

(defun lk/clj-mode-hook ()
  (rainbow-delimiters-mode t))

(add-hook 'clojure-mode-hook #'lk/clj-mode-hook)
(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

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


(defun lk/clojure-scratch ()
  (interactive)
  (let ((buf (generate-new-buffer "clj-scratch.clj")))
        (switch-to-buffer buf)
        (set-buffer-major-mode buf "clojure-mode")))

(global-set-key (kbd "C-x c s") 'lk/clojure-scratch)


(provide 'lk/clojure)
