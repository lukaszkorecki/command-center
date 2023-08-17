;;; clojure.el --- clojure setup
;;; Commentary:
;; inf-clojure/monroe based clj-scratch buffer
;; Adopted from cider's scratch
;;; Code:
(defconst lk/clj-scratch-name "scratch.clj")

(defun lk/init-clojure-scratch ()
  (interactive)
  (let* ((project-root (projectile-project-root))
         (scratch-file (concat project-root "scratch.clj")))
    (if (file-exists-p scratch-file)
        (find-file scratch-file)
      (progn
        (find-file scratch-file)))
    (clojure-mode)))


(defun lk/clojure-scratch ()
  (interactive)
  (pop-to-buffer
   (or (get-buffer lk/clj-scratch-name) (lk/init-clojure-scratch))))

(defun lk/clojure-format-current-buffer ()
  (interactive)
  (lk/invoke-compile-tool-in-project "clojure-lsp format --filenames %s"))

(defun lk/clojure-check-project ()
  (interactive)
  (let* ((dir (projectile-acquire-root))
         (cmd-string (format "clj-kondo --parallel --lint %s" dir)))
    (lk/invoke-compile-tool-in-project cmd-string)))

(defun lk/clojure-check-current-buffer ()
  (interactive)
  (lk/invoke-compile-tool-in-project  "clj-kondo --lint %s 2>&1"))


(use-package clojure-mode-extra-font-locking)


(defun lk/failed-tests-in-monroe-repl ()
  (interactive)
  (swiper "\\(FAIL\\|ERROR\\) in .*\\\\.\\(clj\\|java\\)"))

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
  (kill-matching-buffers ".*monroe.*" 't 't))


(defun lk/clj-mode-hook ()
  (rainbow-delimiters-mode t)
  (require 'monroe)
  (clojure-enable-monroe))

(use-package clojure-mode
  :init (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
  (add-hook 'clojure-mode-hook #'lk/clj-mode-hook)
  :bind (:map clojure-mode-map
              (("C-x c f" . eglot-format)
               ("C-x c v" . lk/clojure-check-current-buffer)
               ("C-x c p" . lk/clojure-check-project)
               ("C-x c s" . lk/clojure-scratch)
               ("C-x c i" . lk/init-clojure-scratch)
               ("C-x c s" . lk/clojure-scratch))))

(provide 'lk/clojure)
;;; clojure.el ends here
