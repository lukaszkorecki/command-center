(require 'clojure-mode-extra-font-locking)

(defun lk/clj-mode-hook ()
  (rainbow-delimiters-mode t))

(add-hook 'clojure-mode-hook #'lk/clj-mode-hook)
(require 'inf-clojure)
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


;; Adopted from cider's scratch buffer

(defconst lk/clj-scratch-name "*clj-scratch*")
(defconst lk/clj-scratch-start-text
  "(ns scratch
  (:require [clojure.test :as test]
            [clojure.tools.namespace.find :as ns.find]
            [clojure.tools.namespace.repl :as ns.repl]
            [eftest.runner :as ef]))
;; (ef/run-tests (ef/find-tests \"some.thing\"))

")

(defun lk/create-clojure-scratch ()
  (with-current-buffer (get-buffer-create lk/clj-scratch-name)
    (clojure-mode)
    (insert lk/clj-scratch-start-text)
    (current-buffer)))

(defun lk/clojure-scratch ()
  (interactive)
  (pop-to-buffer (or (get-buffer lk/clj-scratch-name)
                     (lk/create-clojure-scratch))))

(global-set-key (kbd "C-x c s") 'lk/clojure-scratch)


(provide 'lk/clojure)
