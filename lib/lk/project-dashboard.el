;;; project-dashboard.el --- Transient-based project management dashboard
;;; Code:

(use-package transient :ensure t)

(require 'transient)

(require 'dash)
(require 'lk/utils)

(defun lk/view-pr-web ()
  (interactive)
  (lk/invoke-cli "*gh-pr-create*" "gh pr view --web"))

(defun lk/create-pr-web ()
  (interactive)
  (lk/invoke-cli "*gh-pr-create*" "gh pr create --web"))

(defun lk/view-repo-web ()
  (interactive)
  (lk/invoke-cli "*gh-repo-view*" "gh repo view --web"))

(defun lk/view-or-create-pr ()
  (interactive)
  (let* ((pr-info-maybe?
          (lk/invoke-cli "*gh-pr-info*" "gh pr view --json 'number,url'"))
         (has-pr? (equal 0 (hget pr-info-maybe? :status))))
    (if has-pr?
        (progn
          (message "PR exists, opening in browser")
          (lk/view-pr-web))
      (progn
        (message "No PR found, creating one... ")
        (lk/create-pr-web)))))

(use-package disproject
  :ensure t
  :after (multi-vterm magit consult)
  ;; Replace `project-prefix-map' with `disproject-dispatch'.
  :bind ( :map ctl-x-map ("p" . disproject-dispatch))
  :custom
  (disproject-shell-command #'multi-vterm-project)
  :config (transient-insert-suffix 'disproject-dispatch
            '(-1)
            ["Tools"
             :advice disproject-with-env-apply
             ("M" "magit status" magit-status)
             ("P" "view or create PR in browser" lk/view-or-create-pr)
             ("V" "view repo in the browser" lk/view-repo-web)]))

(provide 'lk/project-dashboard)
;;; project-dashboard.el ends here
