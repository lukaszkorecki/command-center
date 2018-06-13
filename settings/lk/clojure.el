(use-package clojure-mode-extra-font-locking)
(use-package clojure-mode)
(use-package monroe)

(defun lk/clj-mode-hook ()
  (rainbow-delimiters-mode t)
  (clojure-enable-monroe))

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
(defun lk/clj-scratch-start-text ()
  (insert-file-contents "~/.emacs.d/settings/lk/scratch-template.clj"))


(defun lk/create-clojure-scratch ()
  "Creates a scratch buffer, similar to Emacs' *scratch*
     and injects template from lk/clj-scratch-start-text"
  (with-current-buffer (get-buffer-create lk/clj-scratch-name)
    (clojure-mode)
    (lk/clj-scratch-start-text)
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


(defun lk/start-lein-nrepl ()
  "Starts lein repl for the project it can find"
  (interactive)
  (require 'term)
  (let* ((cmd "lein")
         (args "repl :headless")
         (switches (split-string-and-unquote args))
         (termbuf (apply 'make-term "lein repl" cmd nil switches)))
    (set-buffer termbuf)
    (term-mode)
    (term-char-mode)
    (switch-to-buffer termbuf)))

(provide 'lk/clojure)
