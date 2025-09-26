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

(use-package yasnippet
  :init (yas-global-mode t)
  :bind (("C-c i" . yas-insert-snippet)))

(use-package editorconfig)
(use-package jsonrpc)
(use-package f)

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :after (editorconfig jsonrpc f)
  :ensure t
  :config (setq copilot-max-char 1000000)
  (setq copilot-max-char-warning-disable t)

  ;; NOTE: verify if this is necessary
  (add-to-list 'warning-suppress-types
               '((copilot copilot-exceeds-max-char)))
  (add-to-list 'warning-suppress-types
               '((copilot copilot-no-mode-indent)))
  (add-to-list 'warning-suppress-types
               '((copilot--infer-indentation-offset)))

  (add-hook 'emacs-lisp-mode-hook 'copilot-mode)
  (add-hook 'clojure-ts-mode-hook 'copilot-mode)
  (add-hook 'clojure-mode-hook 'copilot-mode)
  (add-hook 'typescript-ts-mode-hook 'copilot-mode)
  (add-hook 'tsx-ts-mode-hook 'copilot-mode)
  (add-hook 'terraform-mode-hook 'copilot-mode)
  (add-hook 'python-mode-hook 'copilot-mode)
  (add-hook 'go-ts-mode-hook 'copilot-mode)
  (add-hook 'json-mode-hook 'copilot-mode)
  (add-hook 'javascript-ts-mode-hook 'copilot-mode)
  (add-hook 'rjsx-mode-hook 'copilot-mode)
  (add-hook 'ruby-ts-mode-hook 'copilot-mode)
  (add-hook 'markdown-ts-mode-hook 'copilot-mode)

  ;; XXX: this basically is here because some major modes set C-c C-t to something else
  (global-unset-key (kbd "C-c C-t"))
  (global-set-key (kbd "C-c TAB") 'copilot-accept-completion)

  :bind (("C-x c c" . copilot-accept-completion)))

(use-package shell-maker
  :straight (; use latest
             :host github ;
             :repo "xenodium/shell-maker" ;
             :files ("shell-maker.el" "markdown-overlays.el"))
  :ensure t)

(use-package acp
  :straight (; use latest
             :host github
             :repo "xenodium/acp.el")
  :after (shell-maker)
  :ensure t)


(use-package agent-shell
  :straight (; use latest
             :host github :repo "xenodium/agent-shell")
  :after (acp)
  :ensure t
  :init)

(defun agent-shell-start-gemini-agent ()
  "Start an interactive Gemini CLI agent shell."
  (interactive)
  (agent-shell--start
   :new-session t
   :mode-line-name "Gemini"
   :buffer-name "Gemini"
   :shell-prompt "Gemini> "
   :shell-prompt-regexp "Gemini> "
   :icon-name "gemini.png"
   :needs-authentication t
   :welcome-function #'agent-shell--gemini-welcome-message
   :authenticate-request-maker (lambda
                                 ()
                                 (acp-make-authenticate-request :method-id "vertex-ai"))
   :client-maker (lambda
                   ()
                   (acp-make-client :command "gemini"
                                    :command-params '("--experimental-acp")))))

(provide 'lk/autocomplete)

;;; autocomplete.el ends here
