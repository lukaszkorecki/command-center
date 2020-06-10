;; inf-clojure/monroe based clj-scratch buffer
;; Adopted from cider's scratch
(defconst lk/clj-scratch-name "*monroe-clj-scratch*")

(defun lk/init-clojure-scratch ()
  (interactive)
  "Creates a scratch buffer, similar to Emacs' *scratch*
     and injects template from lk/clj-scratch-start-text"
  (with-current-buffer (get-buffer-create lk/clj-scratch-name)
    (clojure-mode)
    (monroe-load-file "~/.emacs.d/etc/repl.clj")
    (current-buffer)))

(defun lk/clojure-scratch ()
  "Command to create clojure scratch buffer or to switch to it
     if we have one already"
  (interactive)
  (pop-to-buffer (or (get-buffer lk/clj-scratch-name)
                     (lk/init-clojure-scratch))))

(defun lk/clojure-format-current-buffer ()
  "Format current buffer with CljFmt - assume it's installed already
     (it is as it was added to ~/.lein/profiles.clj)"
  (interactive)
  (lk/invoke-compile-tool-in-project "project.clj" "cljstyle fix %s"))

(defun lk/clojure-slamhound-current-buffer ()
  "Infer imports for current file via slamhound - assume it's installed already
     (it is as it was added to ~/.lein/profiles.clj)"
  (interactive)
  (lk/invoke-compile-tool-in-project "project.clj" "lein slamhound %s"))

(defun lk/clojure-check-project ()
  (interactive)
    (let* ((dir (locate-dominating-file default-directory "project.clj"))
         (cmd-string (format "clj-kondo --lint %s" dir)))
    (lk/invoke-compile-tool-in-project "project.clj" cmd-string)))

(defun lk/clojure-check-current-buffer ()
  "Format current buffer with clj-kondo - assume it's installed already
     (it is as it was added to ~/.lein/profiles.clj)"
  (interactive)
  (lk/invoke-compile-tool-in-project "project.clj" "clj-kondo --lint %s"))

(use-package clojure-mode-extra-font-locking
  :ensure t)


(use-package clojure-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
  :bind
  (:map clojure-mode-map
        (("C-x c f" .  lk/clojure-format-current-buffer)
         ("C-x c v" . lk/clojure-check-current-buffer)
         ("C-x c p" . lk/clojure-check-project)
         ("C-x c s" . lk/clojure-scratch)
         ("C-x c j" . monroe-nrepl-server-start)
         ("C-x c m" . monroe)
         ("C-x c i" . lk/init-clojure-scratch))))

(use-package monroe
  :ensure t
  :pin melpa
  :init
  (setq monroe-nrepl-server-cmd "lein-run")
  :bind
  (:map monroe-mode-map
        (("C-x c s" . lk/clojure-scratch)
         ("C-x c l" . lk/init-clojure-scratch))))

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

(provide 'lk/clojure)
