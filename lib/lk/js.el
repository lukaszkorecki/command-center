;;; lk/js.el --- Javascript & TS packages, customizations etc
;;; Commentary:

;;; Code:

(use-package web-mode
  :ensure t
  :mode ( "\\.liquid$" "\\.js$" "\\.ts$" "\\.jsx$" "\\.tsx$" "\\.html$" "\\.hb$" "\\.mustache$" "\\.erb$" )
  :config (setq web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2
                web-mode-engines-alist
                '(("jinja"    . "\\.j2\\'"))))

(use-package js3-mode
  :ensure t)

;; json-mode for whatever reason expects this (???)
(provide 'js)
(provide 'lk/js)
;;; js.el ends here
