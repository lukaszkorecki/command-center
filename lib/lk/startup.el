
(setenv "INSIDE_EMACS" "TRUE")

(use-package better-defaults
  :ensure t)

;; reduce GC thrash
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 20000000
      read-process-output-max
      (* 1024 1024))

(when (>= emacs-major-version 25)
  (eval-after-load 'bytecomp
    (lambda ()
      (add-to-list 'byte-compile-not-obsolete-funcs 'preceding-sexp))))


(use-package mise
  :ensure t
  :hook (prog-mode-hook . mise-mode))

;;; startup.el --- Initial environment setup, GC settings, mise
;;; Commentary:
;;; Sets up the initial Emacs environment with better defaults,
;;; configures garbage collection for better performance, and loads mise.

(provide 'lk/startup)
;;; startup.el ends here
