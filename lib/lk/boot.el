
(setenv "INSIDE_EMACS" "TRUE")
(use-package better-defaults)

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
  :ensure t)

(provide 'lk/boot)
