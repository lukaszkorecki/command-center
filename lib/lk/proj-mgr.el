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

(defun hm--put (h k v) (puthash k v h))

(defun hm--get (h k) (gethash k h))


(defvar pjmgr--project-file-associations
  (let ((pj-file->type (make-hash-table :test 'equal)))
    (hm--put pj-file->type "deps.edn" 'clojure)
    (hm--put pj-file->type "project.clj" 'clojure)
    ;; TODO: bb.edn, main.tf, package.json, Gemfile, possibly more?
    ;; Add more associations as needed
    pj-file->type))

(defun pjmgr--loc-dom-file->name (dir name)
  (when (locate-dominating-file dir name) name))

(defun pjmgr--project-name-or-nil ()
  (when-let ((pj (project-current)))
    (project-name pj)))


(defun pjmgr--get-project-info-maybe ()
  (let* ((pj-info (make-hash-table :test 'equal))
         (pj-root
          (condition-case root-file-err
              (lk/project-find-root nil)
            (error
             (message "Couldn't find project root in %s" default-directory)
             default-directory)))
         (pj-file
          (or
           (pjmgr--loc-dom-file->name default-directory "deps.edn")
           (pjmgr--loc-dom-file->name default-directory "project.clj")))

         (pj-type
          (when pj-file
            (gethash pj-file pjmgr--project-file-associations nil)))

         (pj-clj-nrepl-running?
          (and
           (equal 'clojure pj-type)
           (locate-dominating-file default-directory ".nrepl-port"))))

    (hm--put pj-info "name" (pjmgr--project-name-or-nil))
    (hm--put pj-info "root" pj-root)
    (hm--put pj-info "cwd" default-directory)
    (hm--put pj-info "type" pj-type)
    (hm--put pj-info "project-file" pj-file)
    (hm--put pj-info "repo" (vc-root-dir))
    (hm--put pj-info "clj-nrepl-running?" pj-clj-nrepl-running?)
    pj-info))


;; -----

(defun pjmgr--overview-str (pj-name pj-info)
  (let* ((pj-type (hm--get pj-info "type"))
         (pj-root (hm--get pj-info "root")))
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

         (is-git-repo? (when pj-info (hm--get pj-info "repo"))))

    (if pj-name
        (mapcar #'pjmgr--mk-suffix
                (-non-nil
                 (list
                  (when pj-info '(:info pj-overview))
                  (when is-git-repo? '(:info #'pjmgr--git-info)))))

      ;; else
      (list (pjmgr--mk-suffix '(:info "<not in a project>"))))))


(defun pjmgr--actions-group (_)
  (let* ((pj-info (pjmgr--get-project-info-maybe))
         (is-git-repo? (hm--get pj-info "repo")))

    (mapcar #'pjmgr--mk-suffix
            (-non-nil
             (list
              '("p" "select a different project" project-switch-project)

              '("t"  "start vterm"  multi-vterm-project)

              (when is-git-repo? '("s" "magit status" magit-status))

              (when is-git-repo? '("g" "git grep" counsel-git-grep)))))))


(defun pjmgr--clojure-cmds-group (pj-info)
  (let* ((pj-clj-nrepl-running?
          (when pj-is-clojure? (hm--get pj-info "clj-nrepl-running?"))))

    (if pj-clj-nrepl-running?
        (mapcar #'pjmgr--mk-suffix
                (list

                 '("m" :info
                   (format "nREPL server is running: %s"
                           (monroe-locate-running-nrepl-host)))

                 '("r" "Switch to the REPL buffer" lk/switch-to-monroe-repl-or-connect-or-start)

                 '("s" "Jump to scratch file" lk/clojure-scratch)

                 '("K"  "Kill monroe server & REPL buffer" lk/monroe-kill-all)))

      ;; we can only start
      (mapcar #'pjmgr--mk-suffix
              (list
               '(:info "nREPL server not running")
               '("m" "start Monroe & nREPL server" lk/switch-to-monroe-repl-or-connect-or-start))))))


(defun pjmgr--cmds-group (_)
  (let* ((pj-info (pjmgr--get-project-info-maybe))
         ;; TODO: this eventually will be a cond-powered dispatch
         (pj-is-clojure?
          (and pj-info (equal 'clojure (hm--get pj-info "type")))))
    (if pj-is-clojure? (pjmgr--clojure-cmds-group pj-info)

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
