(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("351473429145eddc87779a0d9700261073fe480c1e4e35b066a01d6a10c0eedb" "b290c815faa375c1e84973e8c71309459d84e33ad51ded96667f6b62027d8ce8" "70e46ae534129d863df77c3b7c05a83617d7992bc2d47646e34d229cb7d62339" default))
 '(ignored-local-variable-values
   '((eval progn
           (make-variable-buffer-local 'cider-jack-in-nrepl-middlewares)
           (add-to-list 'cider-jack-in-nrepl-middlewares "cider.nrepl/cider-middleware")
           (add-to-list 'cider-jack-in-nrepl-middlewares "portal.nrepl/wrap-portal")
           (add-to-list 'cider-jack-in-nrepl-middlewares "shadow.cljs.devtools.server.nrepl/middleware"))
     (cider-repl-display-help-banner)
     (cider-redirect-server-output-to-repl . t)
     (cider-preferred-build-tool . clojure-cli)
     (cider-default-cljs-repl . custom)
     (cider-clojure-cli-aliases . ":dev:cljs:shadow")))
 '(package-selected-packages
   '(caddyfile-mode lsp-java unicode-fonts moom yasnippet window-number which-key web-mode visual-regexp use-package undo-tree typescript-mode treemacs-projectile transpose-frame todoist terraform-mode sqlup-mode solarized-theme scss-mode sane-term rjsx-mode restclient rainbow-mode rainbow-delimiters python-mode poly-markdown poly-ansible nginx-mode move-text monroe markdown-toc magit lua-mode lsp-ui lsp-treemacs lsp-ivy keychain-environment json-mode ibuffer-vc ibuffer-git highlight-indent-guides go-mode gitignore-mode git flycheck exec-path-from-shell eslint-fix emamux edit-indirect dumb-jump dockerfile-mode darcula-theme counsel-etags company color-theme-approximate clojure-mode-extra-font-locking bufler better-defaults))
 '(warning-suppress-log-types
   '((auto-save)
     (auto-save)
     (auto-save)
     (comp)
     (use-package)
     (use-package)
     (use-package)
     (use-package)
     (use-package)))
 '(warning-suppress-types
   '((lsp-mode)
     (auto-save)
     (auto-save)
     (comp)
     (use-package)
     (use-package)
     (use-package)
     (use-package)
     (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#FFFFFF" :foreground "#505050" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "JetBrains Mono")))))
