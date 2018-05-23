(require 'clojure-mode-extra-font-locking)

(require 'inf-clojure)
(defun lk/clj-mode-hook ()
  (rainbow-delimiters-mode t)
  (inf-clojure-minor-mode t))

(add-hook 'clojure-mode-hook #'lk/clj-mode-hook)

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

;; inf-clojure/monroe based clj-scratch buffer
;; Adopted from cider's scratch
(defconst lk/clj-scratch-name "*clj-scratch*")
(defconst lk/clj-scratch-start-text
  "(ns scratch
  (:require [clojure.test :as test]
            [clojure.repl :as repl]
            [clojure.tools.namespace.find :as ns.find]
            [clojure.tools.namespace.repl :as ns.repl]))
(defn t [p]
  (test/run-tests (re-pattern p)))

;; Have fun!

")

(defun lk/create-clojure-scratch ()
  "Creates a scratch buffer, similar to Emacs' *scratch*
     and injects template from lk/clj-scratch-start-text"
  (with-current-buffer (get-buffer-create lk/clj-scratch-name)
    (clojure-mode)
    (insert lk/clj-scratch-start-text)
    (current-buffer)))

(defun lk/clojure-scratch ()
  "Command to create clojure scratch buffer or to switch to it
     if we have one already"
  (interactive)
  (pop-to-buffer (or (get-buffer lk/clj-scratch-name)
                     (lk/create-clojure-scratch))))

(global-set-key (kbd "C-x c s") 'lk/clojure-scratch)

(defun lk/clojure-format-current-buffer ()
  "Format current buffer with CljFmt - assume it's installed already
     (it is as it was added to ~/.lein/profiles.clj)"
  (interactive)
  (let ((file-name (buffer-file-name (current-buffer))))
    (compilation-start
     (format "lein cljfmt fix %s" file-name)
     'compilation-mode)))

(global-set-key (kbd "C-x c f") 'lk/clojure-format-current-buffer)

(provide 'lk/clojure)
