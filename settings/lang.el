(add-to-list 'auto-mode-alist '("\\.gitignore$" . gitignore-mode))
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

; markdown!
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . flyspell-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . flyspell-mode))

;
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.hb$" . handlebars-mode))
(add-to-list 'auto-mode-alist '("\\.yml$". yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$". yaml-mode))

; Javascripts
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

; Rubby
(add-to-list 'auto-mode-alist '("\\.(rb|rake)$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile|Guardfile)$" . ruby-mode))
(add-to-list 'auto-mode-alist '("^(Vagrantfile|Gemfile|Rakefile|Guardfile)$" . ruby-mode))

; web-mode stuff
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.hb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
