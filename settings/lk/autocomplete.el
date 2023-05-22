;;; autocomplete.el --- ...
;;; Commentary:

;;; Code:



(use-package company
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))


(use-package yasnippet
  :init (yas-global-mode t)
  :bind (("C-c i" . yas-insert-snippet)))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :init
   (add-hook 'clojure-mode-hook 'copilot-mode)
   (add-hook 'typescript-mode-hook 'copilot-mode)
   (add-hook 'shell-mode-hook 'copilot-mode)
   (add-hook 'terraform-mode-hook 'copilot-mode)
   (add-hook 'ruby-mode-hook 'copilot-mode)

   :bind
   (("C-c C-t" . copilot-accept-completion)))

(provide 'lk/autocomplete)

;;; autocomplete.el ends here
