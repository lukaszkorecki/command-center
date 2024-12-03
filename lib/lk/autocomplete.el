;;; autocomplete.el --- ...
;;; Commentary:

;;; Code:


(use-package company
  :config (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode)
  :bind (( "C-c M-c" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-d" . company-show-doc-buffer)
         ("<tab>" . company-complete-selection)
         :map company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)))


(use-package company-box
  :ensure t
  :after company
  :hook (company-mode . company-box-mode))


(global-unset-key (kbd "C-c C-t"))


(use-package yasnippet
  :init (yas-global-mode t)
  :bind (("C-c i" . yas-insert-snippet)))

(use-package editorconfig)

(use-package copilot
  :straight (:host github
                   :repo "copilot-emacs/copilot.el"
                   :files ("dist" "*.el"))
  :ensure t
  :config (setq copilot-max-char 1000000)

  (add-to-list 'warning-suppress-types
               '((copilot copilot-exceeds-max-char)))
  (add-to-list 'warning-suppress-types
               '((copilot copilot-no-mode-indent)))
  (add-to-list 'warning-suppress-types
               '((copilot--infer-indentation-offset)))


  (add-hook 'emacs-lisp-mode-hook 'copilot-mode)
  (add-hook 'clojure-mode-hook 'copilot-mode)
  (add-hook 'typescript-ts-mode-hook 'copilot-mode)
  (add-hook 'tsx-ts-mode-hook 'copilot-mode)
  (add-hook 'terraform-mode-hook 'copilot-mode)
  (add-hook 'python-ts-mode-hook 'copilot-mode)
  (add-hook 'go-ts-mode-hook 'copilot-mode)
  (add-hook 'json-mode-hook 'copilot-mode)
  (add-hook 'javascript-ts-mode-hook 'copilot-mode)
  (add-hook 'js2-mode-hook 'copilot-mode)
  (add-hook 'ruby-ts-mode-hook 'copilot-mode)
  (add-hook 'markdown-mode-hook 'copilot-mode)
  :bind (("C-c C-t" . copilot-accept-completion)
         ("C-c C-a" . copilot-accept-completion)))


(global-set-key (kbd "C-c TAB") 'copilot-accept-completion)


(provide 'lk/autocomplete)

;;; autocomplete.el ends here
