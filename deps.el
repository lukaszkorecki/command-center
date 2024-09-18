;;; deps.el --- deps.el loads straight.el and sets up use-package
;;; Commentary:

;;; Code:


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(add-to-list 'load-path "~/.emacs.d/lib")

(use-package straight
  :custom
  ;; add project and flymake to the pseudo-packages variable so straight.el doesn't download a separate version than what eglot downloads.
  (straight-built-in-pseudo-packages '(emacs nadvice python image-mode project flymake xref))
  (straight-use-package-by-default t))

(provide 'deps)
;;; deps.el ends here
