;;; Stuff for Scheme, 99% chance it's Chicken Scheme
;;; Mostly stolen from: http://wiki.call-cc.org/emacs
(setq scheme-program-name "csi -:c")

(defun scheme-module-indent (state indent-point normal-indent) 0)
(put 'module 'scheme-indent-function 'scheme-module-indent)

(put 'and-let* 'scheme-indent-function 1)
(put 'parameterize 'scheme-indent-function 1)
(put 'handle-exceptions 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'match 'scheme-indent-function 1)

(require 'quack)
