;;; git.el --- git tools, Magit mostly
;;; Commentary:

;;; Code:



;; Git and git nav helpers

(defun lk/git-grep+ (regex)
  "Like vc-git-grep but project current directory and any extension, Pass REGEX.."
  (interactive "sRegex to search for: ")
  (vc-git-grep regex "*" (lk/project-find-root default-directory)))

(use-package git
  :ensure t
  :bind (( "C-c g g" . lk/git-grep+)
         ( "C-c g s" . vc-git-grep)))

(use-package git-link :ensure t)

(defun lk/open-current-file-in-gh ()
  (interactive)
  (browse-url (call-interactively #'git-link)))

(defun lk/open-repo-in-gh ()
  (interactive)
  (browse-url (call-interactively #'git-link-homepage)))

(defun lk/open-current-pr-in-gh ()
  (interactive)
  (shell-command "gh pr view --web"))

(defun lk/git-repo-home ()
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (browse-url (call-interactively #'git-link-homepage))))

(use-package magit
  :ensure t
  :after (project)
  :config ;
  (add-to-list 'project-switch-commands
               '(lk/git-repo-home "Homepage" "b"))
  (add-to-list 'project-switch-commands
               '(magit-project-status "Magit" "m"))
  (setq magit-clone-set-remote.pushDefault t)
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1)
  (setq magit-bury-buffer-function 'magit-restore-window-configuration)

  (setq magit-git-executable "/usr/bin/git")
	:bind (( "C-c m s" . magit-status)))

(defun lk/magit-clear-buffers ()
  (interactive)
  (kill-matching-buffers ".*magit.*" 't 't))



(provide 'lk/git)
;;; git.el ends here
