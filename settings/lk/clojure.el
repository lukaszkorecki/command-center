(use-package clojure-mode-extra-font-locking
  :ensure t)

(use-package clojure-mode
  :ensure t)

(use-package monroe
  :ensure t
  :pin melpa)

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
(defconst lk/clj-scratch-name "*monroe-clj-scratch*")

(defun lk/init-clojure-scratch ()
  (interactive)
  "Creates a scratch buffer, similar to Emacs' *scratch*
     and injects template from lk/clj-scratch-start-text"
  (with-current-buffer (get-buffer-create lk/clj-scratch-name)
    (clojure-mode)
    (monroe-load-file "~/.emacs.d/etc/scratch.clj")
    (current-buffer)))

(defun lk/clojure-scratch ()
  "Command to create clojure scratch buffer or to switch to it
     if we have one already"
  (interactive)
  (pop-to-buffer (or (get-buffer lk/clj-scratch-name)
                     (lk/init-clojure-scratch))))

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

(defun lk/clojure-check-current-buffer ()
  "Format current buffer with Kibit - assume it's installed already
     (it is as it was added to ~/.lein/profiles.clj)"
  (interactive)
  (let ((file-name (buffer-file-name (current-buffer))))
    (compilation-start
     (format "lein kibit %s" file-name)
     'compilation-mode)))

(global-set-key (kbd "C-x c v") 'lk/clojure-check-current-buffer)

(defun lk/clojure-tags ()
 "Create tags file."
 (interactive)
 (let ((root-directory (locate-dominating-file default-directory
                                               "project.clj"))
       (script-path "~/.emacs.d/settings/lk/clj-tags.sh"))
   (shell-command
    (format
     "%s %s" script-path root-directory))))


(provide 'lk/clojure)
