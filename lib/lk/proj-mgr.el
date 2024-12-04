;;; proj-mgr.el --- ...
;;; Commentary:
;; Transient powered coding project management dashboard. Currently supports:
;; - Clojure
;; Planned
;; - Terraform
;; - Javascript (?)
;; - shell (?)


;;; Code:

(use-package transient :ensure t
  :after (consult magit monroe))

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
        ( "bb.edn" . "bb")
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
         (pj-file (pjmgr--find-first-pj-file default-directory))

         (pj-type
          (when pj-file (alist-get pj-file pjmgr--pj-file->type)))

         (pj-clj-nrepl-running?
          (and
           (equal "clojure" pj-type)
           (locate-dominating-file default-directory ".nrepl-port"))))

    (->
     (make-hash-table)
     (hput :name (pjmgr--project-name-or-nil))
     (hput :root  pj-root)
     (hput :cwd  default-directory)
     (hput :type  pj-type)
     (hput :project-file  pj-file)
     (hput :repo
           (pjmgr--loc-dom-file->name default-directory ".git"))
     (hput  :clj-nrepl-running?  pj-clj-nrepl-running?))))

;; -----

(defun pjmgr--project-suffix (_)
  (let* ((pj-name (pjmgr--project-name-or-nil))
         (pj-info (when pj-name (pjmgr--get-project-info-maybe)))
         (items
          (-concat
           (if pj-name
               (list
                (when pj-info
                  (list :info
                        (format "%s [%s]" pj-name (hget pj-info :type)))))
             ;; else
             (list '(:info "<not in a project>")))
           ;; always add project switcher
           (list
            '("p" "select a different project" project-switch-project)))))
    (pjmgr--list->suffixes items)))


(defun pjmgr--actions-suffix (_)
  (pjmgr--list->suffixes
   (list
    '("t"  "start vterm"  multi-vterm-project)
    '("d" "open dired" dired-jump))))

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
           '("g" "git grep" consult-git-grep))))
      '())))

(defun pjmgr--clojure-cmds (pj-info)
  (let* ((pj-clj-nrepl-running?
          (when pj-is-clojure? (hget pj-info :clj-nrepl-running?)))
         (items
          (if pj-clj-nrepl-running?
              (list
               (list :info
                     (format "nREPL server is running: %s"
                             (monroe-locate-running-nrepl-host)))

               '("R" "Switch to the REPL buffer" lk/switch-to-monroe-repl-or-connect-or-start)
               '("S" "Jump to scratch file" lk/clojure-scratch)
               '("K"  "Kill monroe server & REPL buffer" lk/monroe-kill-all)
               '("P" "Start portal session" lk/monroe-portal-start!) ;; TODO: or switch to portal
               )
            ;; we can only start the nREPL first
            (list
             '(:info "nREPL server not running")
             '("M" "start Monroe & nREPL server" lk/switch-to-monroe-repl-or-connect-or-start)))))
    (pjmgr--list->suffixes items)))


(defun pjmgr--cmds-suffix (_)
  (let* ((pj-info (pjmgr--get-project-info-maybe))
         (pj-is-clojure?
          (and pj-info (equal "clojure" (hget pj-info :type)))))
    (if pj-is-clojure?
        ;; TODO: this eventually will be a cond-powered dispatch and cond
        (pjmgr--clojure-cmds pj-info)
      '())))

;; main transient config
(transient-define-prefix lk/proj-mgr
  ()
  "Manages current project, and shows its info"
  [["Project"
    :setup-children pjmgr--project-suffix]

   ["Actions" ;; dispatch generic commands
    :setup-children pjmgr--actions-suffix]]

  [["Repo" :setup-children pjmgr--repo-actions-suffix]

   ["Commands" :setup-children pjmgr--cmds-suffix]])

(define-key global-map (kbd "C-c d") 'lk/proj-mgr)

(provide 'lk/proj-mgr)
