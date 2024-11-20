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
  (let ((table (make-hash-table :test 'equal)))
    (puthash "deps.edn" 'clojure table)
    (puthash "project.clj" 'clojure table)
    ;; TODO: bb.edn, main.tf, package.json, Gemfile, possibly more?
    ;; Add more associations as needed
    table))

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
        (format  "%s (%s) [%s->%s]"
                 (hget pj-info "name")
                 pj-cwd
                 (hget pj-info "type")
                 (hget pj-info "project-file"))
      (format "in %s" (hget pj-info "root")))))

(defun lk/print-project-git-branch ()
  (if-let ((branch (vc-status-mode-line)))
      (s-replace "Git-" "" (first branch))
    "Not a git repo"))

;; main transient config

(transient-define-prefix lk/proj-management
  ()

  "Manages current project, and shows its info"
  ["Project" ;; current project info, if any

   (:info #'lk/print-project-info)
   (:info #'lk/print-project-git-branch)
   ""
   ]

  ;; [:description current-time-string
  ;;               ("-s" "--switch" "switch=") ; switch just to cause updates
  ;;               ;; single suffix with dynamic description
  ;;               ("wa" tsc-suffix-wave
  ;;                :description (lambda
  ;;                               ()
  ;;                               (format "Wave at %s" (current-time-string))))
  ;;               ]
  ;; ""

  ["Actions" ;; dispatch generic commands
   ("t" "start vterm in project root" multi-vterm-project)
   ("s" "magit status" magit-status) ;; TODO: this needs to be conditional in case we're not in a git repo
   ]

  ;; TODO: re-organize this to be dynamic based on the project type, Clj only for now!
  ;; use :setup-children (lambda (_) (list ....
  ;; to dynamically generate language specific commands

  ;; ["Commands"
  ;;  :setup-children (lambda
  ;;                    (_)
  ;;                    (let* ((pj-info (lk/get-root-file-and-project-type))
  ;;                           (pj-clj-nrepl-running? (hget pj-info "clj-nrepl-running?")))

  ;;                      (if pj-clj-nrepl-running?
  ;;                          (list
  ;;                           (transient-parse-suffix
  ;;                            transient--prefix

  ;;                            '("m" :info "nREPL server already running")
  ;;                            ;; TODO:
  ;;                            ;; - kill monroe stuff
  ;;                            ;; - switch to monroe REPL
  ;;                            ;; - switch to scratch.clj
  ;;                            ))

  ;;                        (list
  ;;                         (transient-parse-suffix
  ;;                          transient--prefix

  ;;                          '("m" "start Monroe & nREPL server"  #'monroe-nrepl-server-start))))))
  ;;  ]
  )

(define-key global-map (kbd "C-c d") 'lk/proj-management)

(provide 'proj-management)
;;; proj-management.el ends here
