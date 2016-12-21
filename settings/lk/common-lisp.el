
;; setup Slime and use SBCL as the lisp implementation


(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")

(require 'slime)

(slime-setup '(slime-fancy slime-tramp slime-asdf))
(slime-require :swank-listener-hooks)


(provide 'lk/common-lisp)
