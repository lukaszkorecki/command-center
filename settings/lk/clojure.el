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

(global-set-key (kbd "C-x c l") 'cider-connect)
(global-set-key (kbd "C-x c s") 'cider-scratch)
(global-set-key (kbd "C-x c f") 'cider-format-buffer)
(global-set-key (kbd "C-x c r") 'cider-refresh)


(provide 'lk/clojure)
