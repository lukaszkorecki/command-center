;;; proj-mgr.el --- ...
;;; Commentary:
;; Transient powered coding project management dashboard. Currently supports:
;; - Clojure
;; Planned
;; - Terraform
;; - Javascript (?)
;; - shell (?)


;;; Code:

(use-package transient :ensure t :after (consult magit cider))

(require 'transient)

(require 'dash)
(require 'lk/utils)


;; Utils to make creating suffixes easier
(defun pjmgr--mk-suffix (a-list)
  "Util for creating suffixes for Transient"
  (transient-parse-suffix 'transient--prefix a-list))

(defun pjmgr--list->suffixes (a-list)
  (->> a-list -non-nil (mapcar #'pjmgr--mk-suffix)))


;; these are in order!
(setq pjmgr--pj-file->type
      '(( "deps.edn".  "clojure")
        ( "project.clj" . "clojure")
        ( "init.el" . "elisp")
        ( "main.tf" . "terraform")
        ( "bb.edn" . "clojure")
        ( "package.json" . "js")))

(defun pjmgr--loc-dom-file->name (dir name)
  "Finds `name` in given `dir`, if found: returns it otherwise nil"
  (when (locate-dominating-file dir name) name))

(defun pjmgr--find-first-pj-file (dir)
  "Gets first match for dominating project file. The order happens to prioritze Clojure :-)"
  (->> pjmgr--pj-file->type
       (mapcar #'first)
       (mapcar
        (lambda (fname) (pjmgr--loc-dom-file->name dir fname)))
       (-non-nil)
       (first)))

(defun pjmgr--project-name-or-nil ()
  "Get current project's name or nil"
  (when-let ((pj (project-current)))
    (project-name pj)))

(defun pjmgr--get-project-info-maybe ()
  "Create a hash map of {name root cw type project-file repo clj-nrepl-running?}"
  (let* ((pj-root
          (condition-case root-file-err
              (lk/project-find-root nil)
            (error
             (message "Couldn't find project root in %s" default-directory)
             default-directory)))
         (pj-file (pjmgr--find-first-pj-file default-directory)))
    (->
     (make-hash-table)
     (hput :name (pjmgr--project-name-or-nil))
     (hput :root  pj-root)
     (hput :cwd  default-directory)
     (hput :repo
           (pjmgr--loc-dom-file->name default-directory ".git")))))

;; -----

(defun pjmgr--project-suffix (_)
  (let* ((pj-name (pjmgr--project-name-or-nil))
         (pj-info (when pj-name (pjmgr--get-project-info-maybe)))
         (items
          (-concat
           (if pj-name
               (list (when pj-info (list :info pj-name)))
             ;; else
             (list '(:info "<not in a project>")))
           ;; always add project switcher
           (list
            '("p" "select a different project" project-switch-project)))))
    (pjmgr--list->suffixes items)))

;; Generic actions
(defun pjmgr--actions-suffix (_)
  (pjmgr--list->suffixes
   (list
    '("t"  "start vterm"  multi-vterm-project)
    '("d" "open dired" dired-jump))))

;; GH-CLI based actions
(defun pjmgr---view-pr-web ()
  (interactive)
  (lk/invoke-cli "*gh-pr-create*" "gh pr create --web"))

(defun prmgr--create-pr-web ()
  (interactive)
  (lk/invoke-cli "*gh-pr-create*" "gh pr view --web"))

(defun pjmgr--get-gh-pr-actions ()
  (let* ((pr-info-maybe?
          (lk/invoke-cli "*gh-pr-info*" "gh pr view --json 'number,url'"))
         (pr-info
          (when (equal 0 (hget pr-info-maybe? :status))
            (json-parse-string (hget pr-info-maybe? :output)))))
    (if pr-info
        (list
         (list :info
               (format "PR #%s (%s)"
                       (hget pr-info "number")
                       (hget pr-info "url")))
         '("v" "view PR in browser" prmgr--create-pr-web))
      (list '("c" "create PR in web" pjmgr---view-pr-web)))))


;; Dispatch project actions, with conditionals
(defun pjmgr--repo-actions-suffix (_)
  (let* ((pj-info (pjmgr--get-project-info-maybe))
         (is-git-repo? (hget pj-info :repo))
         (git-branch
          (when is-git-repo?

            (format "Branch: %s"
                    (->
                     (lk/invoke-cli "*git-branch*" "git cb")
                     (hget :output))))))
    (if is-git-repo?
        (pjmgr--list->suffixes
         (-concat
          (list (list :info git-branch))
          (pjmgr--get-gh-pr-actions)
          (list
           '("s" "magit status" magit-status)
           '("g" "git grep" consult-git-grep)
           '("b" "view repo in browser" lk/open-repo-in-gh))))
      '())))


;; main transient config
(transient-define-prefix lk/proj-mgr
  ()
  "Manages current project, and shows its info"
  [["Project"
    :setup-children pjmgr--project-suffix]

   ["Actions" ;; dispatch generic commands
    :setup-children pjmgr--actions-suffix]]

  [["Repo" :setup-children pjmgr--repo-actions-suffix]])

(define-key global-map (kbd "C-c d") 'lk/proj-mgr)


(use-package disproject
  ;; Replace `project-prefix-map' with `disproject-dispatch'.
  :bind ( :map ctl-x-map
          ("p" . disproject-dispatch)))


(provide 'lk/proj-mgr)
