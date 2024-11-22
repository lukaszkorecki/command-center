;;; proj-mgr.el --- ...
;;; Commentary:
;; Transient powered coding project management dashboard. Currently supports:
;; - Clojure
;; Planned
;; - Terraform
;; - Javascript (?)
;; - shell (?)


;;; Code:

(require 'transient)
(require 'dash)


(defun hget (alist k)
  "Get element from a hash table but in a sane way"
  (gethash k alist))

(defun hput (hm k v)
  "Modifies hast table in place, but also returns it so its easier to thread"
  (puthash  k v hm) hm)


;; these are in order
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
           (locate-dominating-file default-directory ".nrepl-port")))

         )

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

(defun pjmgr--overview-str (pj-name pj-info)
  (let* ((pj-type (hget pj-info :type))
         (pj-root (hget pj-info :root)))
    (if pj-type
        (format  "Project: %s [%s] (%s)" pj-name pj-type pj-root)
      (format "In %s" pj-root))))


;; XXX: I wonder if I can plug in magit's own suffixes/groups here?
(defun pjmgr--git-info ()
  (if-let ((branch (vc-status-mode-line)))
      (format "Branch: %s" (s-replace "Git-" "" (first branch)))
    "Not a git repo"))


(defun pjmgr--mk-suffix (a-list)
  "Util for creating suffixes for Transient"
  (transient-parse-suffix transient--prefix a-list))

(defun pjmgr--list->suffixes (a-list)
  (->> a-list -non-nil (mapcar #'pjmgr--mk-suffix)))


(defun pjmgr--start-group (_)
  (let* ((pj-name (pjmgr--project-name-or-nil))
         (pj-info (when pj-name (pjmgr--get-project-info-maybe)))

         (pj-overview
          (when pj-name (pjmgr--overview-str pj-name pj-info)))

         (is-git-repo? (when pj-info (hget pj-info :repo)))

         (items
          (if pj-name
              (list
               (when pj-info '(:info pj-overview))
               '(:info #'pjmgr--git-info))
            ;; else
            (list '(:info "<not in a project>")))))
    (pjmgr--list->suffixes items)))


(defun pjmgr--actions-group (_)
  (let* ((pj-info (pjmgr--get-project-info-maybe))
         (is-git-repo? (hget pj-info :repo)))
    (pjmgr--list->suffixes
     (list
      '("p" "select a different project" project-switch-project)
      '("t"  "start vterm"  multi-vterm-project)
      (when is-git-repo? '("s" "magit status" magit-status))
      (when is-git-repo? '("g" "git grep" counsel-git-grep))))))


(defun pjmgr--clojure-cmds-group (pj-info)
  (let* ((pj-clj-nrepl-running?
          (when pj-is-clojure? (hget pj-info "clj-nrepl-running?")))
         (items
          (if pj-clj-nrepl-running?
              (list
               '("m" :info
                 (format "nREPL server is running: %s"
                         (monroe-locate-running-nrepl-host)))

               '("r" "Switch to the REPL buffer" lk/switch-to-monroe-repl-or-connect-or-start)
               '("s" "Jump to scratch file" lk/clojure-scratch)
               '("K"  "Kill monroe server & REPL buffer" lk/monroe-kill-all)
               ;; TODO: add portal
               )
            ;; we can only start
            (list
             '(:info "nREPL server not running")
             '("m" "start Monroe & nREPL server" lk/switch-to-monroe-repl-or-connect-or-start)))))

    (pjmgr--list->suffixes items)))


(defun pjmgr--cmds-group (_)
  (let* ((pj-info (pjmgr--get-project-info-maybe))
         (pj-is-clojure?
          (and pj-info (equal "clojure" (hget pj-info :type)))))
    (if pj-is-clojure?
        ;; TODO: this eventually will be a cond-powered dispatch and cond
        (pjmgr--clojure-cmds-group pj-info)
      '())))

;; main transient config
(transient-define-prefix lk/proj-mgr
  ()

  "Manages current project, and shows its info"
  [["🌊 Start"
    :setup-children pjmgr--start-group]

   ["Actions" ;; dispatch generic commands
    :setup-children pjmgr--actions-group]]

  ["Commands" :setup-children pjmgr--cmds-group])

(define-key global-map (kbd "C-c d") 'lk/proj-mgr)

(provide 'lk/proj-mgr)
