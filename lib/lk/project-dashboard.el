;;; -*- lexical-binding: t; -*-
;;; project-dashboard.el --- Transient-based project management dashboard
;;; Code:

(use-package project-rootfile :ensure t)

(defvar lk/home-full-path (file-truename (getenv "HOME")))

(defvar lk/project-root-files
  '( "package.json" "deps.edn" "project.clj" "main.tf" "go.mod" "mise.toml"))

(defun lk/project-find-root (path)
  "Searches for project root starting from PATH. Picks the root directory based on:

  1. Version control system root (if any)
  2. Presence of known project root files (e.g., .git, package.json, etc.
  3. fails if the detected root is equal to user's home directory
  Shortest path wins. Returns the project root path or nil if not found."
  (let* ((vc-root (vc-root-dir))          ;; as fallback
         ;; iterate over project files and find their locations:
         (paths (mapcar ; nofmt
                 (lambda (file)
                   (when-let* ((path (locate-dominating-file path file)))
                     (expand-file-name file)))
                 lk/project-root-files))
         ;; now filter out nils and home directory
         (filterd-paths (seq-filter ; nofmt
                         (lambda (p)
                           (and p (not (string= (file-truename p) lk/home-full-path)))) ; nofmt
                         paths)))

    ;; return shortest path if any, fallback to VC root
    (if filterd-paths (car (sort filterd-paths)) vc-root)))

(use-package project
  :ensure t
  :after (project-rootfile)
  :config ; nofmt
  (advice-add #'project-find-regexp :override #'consult-git-grep)
  (advice-add #'project-shell :override #'multi-vterm)
  (add-to-list 'project-switch-commands
               '(magit-project-status "Magit" ?m)
               '(consult-git-grep "Git grep" ?g))
  (add-to-list 'project-find-functions #'lk/project-find-root t)
  :bind-keymap ("C-c p" . project-prefix-map))

(use-package ibuffer-project :ensure t :after (project))

(defun lk/ibuffer-toggle-never-show ()
  "Clear the list of never show predicates."
  (interactive)

  (if ibuffer-never-show-predicates
      (setq ibuffer-never-show-predicates nil)
    (setq ibuffer-never-show-predicates
          '("^\\*Messages"
            "^\\*Warnings"
            "^\\*Help\\*"
            "^\\*Apropos"
            "^magit"
            "^\\*copilot.events"
            "^\\*EGLOT")))
  (ibuffer-update nil t))

(use-package ibuffer
  :ensure t
  :after (project ibuffer-project)
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("C-c C-t" . lk/ibuffer-toggle-never-show))
  ;; bind in ibuffer mode only?
  :init (add-hook 'ibuffer-hook
                  (lambda ()
                    (setq ibuffer-filter-groups
                          (ibuffer-project-generate-filter-groups))
                    (unless (eq ibuffer-sorting-mode 'project-file-relative)
                      (ibuffer-do-sort-by-project-file-relative))))

  (setq ibuffer-formats
        '((mark
           modified
           read-only
           " "
           (name 18 18 :left :elide)
           " "
           " "
           (mode 16 16 :left :elide)
           " "

           filename-and-process
           " "
           project-file-relative))))

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
  :after (multi-vterm magit consult project)
  ;; Replace `project-prefix-map' with `disproject-dispatch'.
  :bind ( :map ctl-x-map ("p" . disproject-dispatch))
  :custom (disproject-shell-command #'multi-vterm-project)
  :config ;; Remove existing "Tools" suffix if present to avoid duplicates on reload
  (ignore-errors
    (transient-remove-suffix 'disproject-dispatch '("Tools")))

  (transient-insert-suffix 'disproject-dispatch
    '(-1)
    ["Tools"
     :advice disproject-with-env-apply
     ("M" "magit status" magit-status)
     ("P" "view or create PR in browser" lk/view-or-create-pr)
     ("V" "view repo in the browser" lk/view-repo-web)]))

(global-set-key (kbd "C-x p") 'disproject-dispatch)

(provide 'lk/project-dashboard)
;;; project-dashboard.el ends here
