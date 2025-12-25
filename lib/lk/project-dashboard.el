;;; -*- lexical-binding: t; -*-
;;; project-dashboard.el --- Transient-based project management dashboard
;;; Code:

(use-package project-rootfile :ensure t)

(defvar lk/home-full-path (getenv "HOME"))

(defun lk/project-find-root (path)
  "Search up the PATH for known project file markers. Throws an error if found path is
  equal to users home directory"
  (when-let* ((root
              (if-let* ((vc-root (project-try-vc default-directory)))
                  ;; get dir from (vc Git "dir")
                  (car (last vc-root))
                ;; this returns #s(project-rootfile-plain "~/tmp/foobar/") struct,
                ;; we need the :root part
                (project-rootfile-plain--root
                 (project-rootfile-try-detect default-directory)))))
    (let* ((root-exp (expand-file-name root)))
      ;; bail if detected dir is equal to home (can happen!)
      (when (string-equal lk/home-full-path root-exp)
        (message "Root folder is equal to HOME!")
        (throw 'lk/invalid-project-root t))
      ;; otherwise we're good
      root-exp)))


(use-package project
  :ensure t
  :after (project-rootfile)
  :config
  (advice-add #'project-find-regexp :override #'consult-git-grep)
  (advice-add #'project-shell :override #'multi-vterm)
  (add-to-list 'project-switch-commands
               '(magit-project-status "Magit" ?m)
               '(consult-git-grep "Git grep" ?g))
  (add-to-list 'project-find-functions #'project-rootfile-try-detect t)
  :bind-keymap ("C-c p" . project-prefix-map))

(use-package ibuffer-project
  :ensure t
  :after (project))


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
  :custom
  (disproject-shell-command #'multi-vterm-project)
  :config (transient-insert-suffix 'disproject-dispatch
            '(-1)
            ["Tools"
             :advice disproject-with-env-apply
             ("M" "magit status" magit-status)
             ("P" "view or create PR in browser" lk/view-or-create-pr)
             ("V" "view repo in the browser" lk/view-repo-web)]))


(global-set-key (kbd "C-x p") 'disproject-dispatch)

(provide 'lk/project-dashboard)
;;; project-dashboard.el ends here
