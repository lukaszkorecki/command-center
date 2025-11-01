;;; project-dashboard.el --- Transient-based project management dashboard
;;; Commentary:
;;; Provides a transient-powered coding project management dashboard with
;;; project-aware actions, git integration, and repository utilities.
;;; Currently supports Clojure projects with plans for Terraform, JavaScript, and shell.

;;; Code:

(use-package transient :ensure t :after (consult magit cider))

(require 'transient)

(require 'dash)
(require 'lk/utils)

(defun lk/view-pr-web ()
  (interactive)
  (lk/invoke-cli "*gh-pr-create*" "gh pr create --web"))

(defun lk/create-pr-web ()
  (interactive)
  (lk/invoke-cli "*gh-pr-create*" "gh pr view --web"))

(defun lk/view-or-create-pr ()
  (interactive)
  (let* ((pr-info-maybe?
          (lk/invoke-cli "*gh-pr-info*" "gh pr view --json 'number,url'"))
         (pr-info
          (when (equal 0 (hget pr-info-maybe? :status))
            (json-parse-string (hget pr-info-maybe? :output)))))
    (if pr-info (lk/-view-pr-web) (lk/create-pr-web))))

(use-package disproject
  :ensure t
  ;; Replace `project-prefix-map' with `disproject-dispatch'.
  :bind ( :map ctl-x-map ("p" . disproject-dispatch))

  :config (transient-insert-suffix 'disproject-dispatch
            '(-1)
            ["Tools"
             :advice disproject-with-env-apply
             ("t"  "start vterm"  multi-vterm-project)
             ("s" "magit status" magit-status)
             ("g" "git grep" consult-git-grep)
             ("b" "view repo in browser" lk/open-repo-in-gh)
             ("p" "view or create PR in browser" lk/view-or-create-pr)

             ]))

(provide 'lk/project-dashboard)
;;; project-dashboard.el ends here
