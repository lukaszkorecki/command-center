;; -*- lexical-binding: t; -*-

  (unless (fboundp 'set-local)
    (defun set-local (variable value)
      "Make VARIABLE buffer-local, then set its value in the current buffer to VALUE."
      (set (make-local-variable variable) value)))
