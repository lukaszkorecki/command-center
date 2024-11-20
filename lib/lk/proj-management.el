;;; proj-management.el --- ...
;;; Commentary:
;; Transient powered coding project management dashboard. Currently supports:
;; - Clojure
;; Planned
;; - Terraform
;; - Javascript (?)
;; - shell (?)


;;; Code:

(defun hput (h k v) (puthash k v h))

(defun hget (h k) (gethash k h))


(defvar lk/project-file-associations
  (let ((pj-file->type (make-hash-table :test 'equal)))
    (hput pj-file->type "deps.edn" 'clojure)
    (hput pj-file->type "project.clj" 'clojure)
    ;; TODO: bb.edn, main.tf, package.json, Gemfile, possibly more?
    ;; Add more associations as needed
    pj-file->type))

(defun lk/loc-dom-file->name (dir name)
  (when (locate-dominating-file dir name) name))


(defun lk/get-root-file-and-project-type ()
  (let* ((pj-info (make-hash-table :test 'equal))
         (pj-root
          (condition-case root-file-err
              (lk/project-find-root nil)
            (error
             (message "Couldn't find project root in %s" default-directory)
             default-directory)))
         (pj-file
          (or
           (lk/loc-dom-file->name default-directory "deps.edn")
           (lk/loc-dom-file->name default-directory "project.clj")))

         (pj-type
          (when pj-file
            (gethash pj-file lk/project-file-associations nil)))

         (pj-clj-nrepl-running?
          (and
           (equal 'clojure pj-type)
           (locate-dominating-file default-directory ".nrepl-port"))))

    (hput pj-info "name"
          (when pj-file (project-name (project-current))))
    (hput pj-info "root" pj-root)
    (hput pj-info "cwd" default-directory)
    (hput pj-info "type" pj-type)
    (hput pj-info "project-file" pj-file)
    (hput pj-info "repo" (vc-root-dir))
    (hput pj-info "clj-nrepl-running?" pj-clj-nrepl-running?)
    pj-info))


;; -----

(defun lk/print-project-info ()
  (let* ((pj-info (lk/get-root-file-and-project-type))
         (pj-type (hget pj-info "type"))
         (pj-cwd (hget pj-info "cwd"))
         (pj-root (hget pj-info "root"))
         (is-repo? (hget pj-info "repo")))
    (if pj-type
        (format  "%s [%s->%s]"
                 pj-cwd
                 (hget pj-info "type")
                 (hget pj-info "project-file"))
      (format "in %s" (hget pj-info "root")))))


;; XXX: I wonder if I can plug in magit's own suffixes/groups here?
(defun lk/print-project-git-branch ()
  (if-let ((branch (vc-status-mode-line)))
      (format "Branch: %s" (s-replace "Git-" "" (first branch)))
    "Not a git repo"))


(defun lk/mk-sffx (a-list)
  "Util for creating suffixes for Transient"
  (transient-parse-suffix transient--prefix a-list))

;; main transient config
(transient-define-prefix lk/proj-management
  ()

  "Manages current project, and shows its info"
  [[:description
    (lambda ()
      (if-let ((name (hget (lk/get-root-file-and-project-type) "name")))
          (format "Project: %s" name)
        "<not in a project>"))

    (:info #'lk/print-project-info)
    (:info #'lk/print-project-git-branch)
    ""
    ]


   ["Actions" ;; dispatch generic commands
    :setup-children (lambda
                      (_)
                      (let* ((pj-info (lk/get-root-file-and-project-type))
                             (is-git-repo? (hget pj-info "repo")))

                        (-non-nil
                         (list
                          (lk/mk-sffx
                           '("t" "start vterm in project root" multi-vterm-project))

                          (when is-git-repo?
                            (lk/mk-sffx '("s" "magit status" magit-status)))

                          (when is-git-repo?
                            (lk/mk-sffx '("g" "git grep" counsel-git-grep)))))))
    ]]

  ;; TODO: re-organize this to be dynamic based on the project type, Clj only for now!
  ;; use :setup-children (lambda (_) (list ....
  ;; to dynamically generate language specific commands

  ["Commands"
   :setup-children (lambda
                     (_)
                     (let* ((pj-info (lk/get-root-file-and-project-type))
                            (pj-is-clojure? (equal 'clojure (hget pj-info "type")))
                            (pj-clj-nrepl-running?
                             (when pj-is-clojure? (hget pj-info "clj-nrepl-running?"))))

                       (if pj-is-clojure?
                           (if pj-clj-nrepl-running?
                               (list
                                (lk/mk-sffx
                                 '("m" :info
                                   (format "nREPL server already: %s"
                                           (monroe-locate-running-nrepl-host))))

                                (lk/mk-sffx
                                 '("r" "Switch to the REPL buffer" lk/switch-to-monroe-repl-or-connect-or-start))

                                (lk/mk-sffx
                                 '("s" "Jump to scratch file" lk/clojure-scratch))

                                (lk/mk-sffx
                                 '("K"  "Kill monroe server & REPL buffer" lk/monroe-kill-all)))

                             ;; we can only start
                             (list
                              (lk/mk-sffx
                               '("m" "start Monroe & nREPL server" lk/switch-to-monroe-repl-or-connect-or-start))))
                         ;; not a Clj project, do nothing (for now!)
                         '())))
   ;; end Commands here
   ])

(define-key global-map (kbd "C-c d") 'lk/proj-management)

(provide 'proj-management)
;;; proj-management.el ends here
