;;; git.el --- git tools, Magit mostly
;;; Commentary:

;;; Code:



;; Git and git nav helpers

(defun lk/git-grep+ (regex)
  "Like vc-git-grep but project current directory and any extension, Pass REGEX.."
  (interactive "sRegex to search for: ")
  (vc-git-grep regex "*" (lk/project-find-root default-directory)))

(use-package git
  :bind (( "C-c g g" . lk/git-grep+)
         ( "C-c g s" . vc-git-grep)))

(use-package git-link :ensure t)

(defun lk/open-current-file-in-gh ()
  (interactive)
  (browse-url (call-interactively #'git-link)))

(global-set-key (kbd "C-c g f") 'lk/open-current-file-in-gh)

(defun lk/open-repo-in-gh ()
  (interactive)
  (browse-url (call-interactively #'git-link-homepage)))

(global-set-key (kbd "C-c g p") 'lk/open-repo-in-gh)

(defun lk/open-current-pr-in-gh ()
  (interactive)
  (shell-command "gh pr view --web"))

(global-set-key (kbd "C-c g h") 'lk/open-current-pr-in-gh)


(defun lk/git-repo-home ()
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (browse-url (call-interactively #'git-link-homepage))))

(use-package magit
  :ensure t
  :after project
  :init (add-to-list 'project-switch-commands
                     '(lk/git-repo-home "Homepage" "b"))
  (add-to-list 'project-switch-commands
               '(magit-project-status "Magit" "m"))
  (setq magit-clone-set-remote.pushDefault t)
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1)
  (setq magit-bury-buffer-function 'magit-restore-window-configuration)

  :config (setq magit-git-executable "/usr/bin/git")
  (setq magit-completing-read-function 'ivy-completing-read)
	:bind (( "C-c m s" . magit-status)))

(defun lk/magit-clear-buffers ()
  (interactive)
  (kill-matching-buffers ".*magit.*" 't 't))



(provide 'lk/git)
;;; git.el ends here
