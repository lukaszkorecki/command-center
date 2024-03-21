;;; git.el --- git tools, Magit mostly
;;; Commentary:

;;; Code:



;; Git and git-surf helpers

(defun lk/git-grep+ (regex)
  "Like vc-git-grep but project current directory and any extension, Pass REGEX.."
  (interactive "sRegex to search for: ")
  (vc-git-grep regex "*" (projectile-acquire-root)))

(use-package git
  :bind (( "C-c g g" . lk/git-grep+)
         ( "C-c g s" . vc-git-grep)))

(defun lk/open-current-file-in-gh ()
  (interactive)
  (let* ((line-no (line-number-at-pos))
         (command
          (format "~/.emacs.d/etc/bin/git-surf -l%s -f %s"
                  line-no
                  (file-name-nondirectory (buffer-file-name)))))
    (message (format "CMD: %s" command))
    (shell-command command)))

(global-set-key (kbd "C-c g f") 'lk/open-current-file-in-gh)


(defun lk/open-current-pr-in-gh ()
  (interactive)
  (shell-command "git surf -p"))

(global-set-key (kbd "C-c g h") 'lk/open-current-pr-in-gh)

(use-package magit
  :config (setq magit-git-executable "/usr/bin/git")
  (setq magit-completing-read-function 'ivy-completing-read)
	:bind (( "C-c m s" . magit-status)))

(defun lk/magit-clear-buffers ()
  (interactive)
  (kill-matching-buffers ".*magit.*" 't 't))

(use-package difftastic
  :demand t
  :after (magit)
  :bind (:map magit-blame-read-only-mode-map
         ("D" . difftastic-magit-show)
         ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))

(use-package ibuffer-vc)

(use-package ibuffer-git
  :init (setq ibuffer-formats
              '((mark modified read-only vc-status-mini " "
                      (name 18 18 :left :elide)
                      " "
                      " "
                      (mode 16 16 :left :elide)
                      " "
                      (git-status-mini)
                      " "
                      (git-status 8 8 :right)
                      " "
                      filename-and-process))))

(provide 'lk/git)
;;; git.el ends here
