;;; js.el --- Javascript & TS packages, customizations etc
;;; Commentary:

;;; Code:

;; Javascripts
(defun lk/prettier-format-current-buffer ()
  "."
  (interactive)
  (lk/invoke-compile-tool-in-project "FORCE_COLOR=0 node ./node_modules/.bin/prettier --write %s"))

(defun lk/eslint-check-current-buffer ()
  "."
  (interactive)
  (lk/invoke-compile-tool-in-project "node ./node_modules/.bin/eslint --fix %s"))

(use-package rjsx-mode
  :init (add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx$" . rjsx-mode))
  (setq js-indent-level 2)
  ;; settings for js2 mode
  (add-hook 'js2-mode-hook (lambda () (abbrev-mode)))
  (setq-default js-switch-indent-offset 4)
  (setq-default js2-basic-offset 2)
  (setq-default js2-indent-switch-body t)
  ;; es6 is ok with trailing commas
  (setq-default js2-strict-trailing-comma-warning nil)
  :bind (:map js2-mode-map
              (( "C-x c f" . lk/prettier-format-current-buffer ))
              (( "C-x c v" . lk/eslint-check-current-buffer ))))


(use-package eslint-fix)

(defun lk/tslint-check-current-buffer ()
  "."
  (interactive)
  (lk/invoke-compile-tool-in-project "tslint -p ./tsconfig.json %s"))

(use-package typescript-mode
  :init (setq typescript-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx$" . typescript-mode))
  :bind (:map typescript-mode-map
              (( "C-x c v" . lk/eslint-check-current-buffer)
               ( "C-x c f" . eglot-format ))))


(define-derived-mode typescript-tsx-mode typescript-mode "tsx")
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))


(provide 'lk/js)
;;; js.el ends here
