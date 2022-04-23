;;; clojure.el --- clojure setup
;;; Commentary:
;; inf-clojure/monroe based clj-scratch buffer
;; Adopted from cider's scratch
;;; Code:
(defconst lk/clj-scratch-name "*monroe-clj-scratch*")

(defun lk/init-clojure-scratch ()
  (interactive)
  (with-current-buffer (get-buffer-create lk/clj-scratch-name)
    (clojure-mode)
    (message (pwd))
    (if (string-match ".+ssh:.+" (pwd))
        ;; assume TRAMP and load the repl file from a remote host
        (monroe-load-file "/home/ubuntu/.emacs.d/etc/repl.clj")
      (monroe-load-file "~/.emacs.d/etc/repl.clj"))
    (current-buffer)))

(defun lk/clojure-scratch ()
  (interactive)
  (pop-to-buffer
   (or (get-buffer lk/clj-scratch-name) (lk/init-clojure-scratch))))

(defun lk/clojure-format-current-buffer ()
  (interactive)
  (lk/invoke-compile-tool-in-project "project.clj" "cljstyle fix %s"))

(defun lk/clojure-check-project ()
  (interactive)
  (let* ((dir (locate-dominating-file default-directory "project.clj"))
         (cmd-string (format "clj-kondo --parallel --lint %s" dir)))
    (lk/invoke-compile-tool-in-project "project.clj" cmd-string)))

(defun lk/clojure-check-current-buffer ()
  (interactive)
  (lk/invoke-compile-tool-in-project "project.clj" "clj-kondo --lint %s 2>&1"))

(defun lk/clj-rebuild-tags ()
  (interactive)
  (lk/invoke-compile-tool-in-project "Makefile"
                                     "git ls-files | egrep  '*.clj.+' | xargs etags --regex=@/home/ubuntu/.emacs.d/settings/lk/clojure.etags TAGS"))


(use-package clojure-mode-extra-font-locking)

(use-package monroe
  :init (setq monroe-nrepl-server-cmd "lein-run")
  :bind (("C-x c m" . monroe)
         :map monroe-mode-map
         (("C-x c l" . lk/init-clojure-scratch))))

(defun lk/monroe-kill-all ()
  (interactive)
  (kill-matching-buffers ".*monroe.*"))

(use-package clojure-mode
  :init (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
  (require 'monroe)
  (rainbow-delimiters-mode t)
  (clojure-enable-monroe)
  :bind (:map clojure-mode-map
              (("C-x c f" .  lk/clojure-format-current-buffer)
               ("C-x c v" . lk/clojure-check-current-buffer)
               ("C-x c p" . lk/clojure-check-project)
               ("C-x c s" . lk/clojure-scratch)
               ("C-x c j" . monroe-nrepl-server-start)
               ("C-x c m" . monroe)
               ("C-c C-z" . monroe-switch-to-repl)
               ("C-c C-l" . monroe-load-file)
               ("C-c m l " . lsp-clojure-move-to-let)
               ("C-x c s" . lk/clojure-scratch)
               ("C-x c t" . lk/clj-rebuild-tags)
               ("C-x c c v" . clojure-convert-collection-to-vector)
               ("C-x c c s" . clojure-convert-collection-to-set)
               ("C-x c i" . lk/init-clojure-scratch))))



(defun lk/clj-mode-hook ()
  (rainbow-delimiters-mode t)
  (require 'monroe)
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
;;; clojure.el ends here
