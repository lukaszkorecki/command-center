(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; strip whitespace
(use-package whitespace
  :ensure t
  :init
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  :config
  (setq whitespace-style '(face empty tabs lines-tail trailing))
  (setq whitespace-line-column 80))

;; add final newline automaticaly
(setq require-final-newline t)

;; indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default standard-indent 2)

(define-key global-map (kbd "RET") 'newline-and-indent)

(provide 'lk/editing)
