;;; git.el --- git tools, Magit mostly
;;; Commentary:

;;; Code:



;; Git and git-surf helpers
(use-package git)

(when (string-equal system-type "Darwin"))

(defun lk/open-current-file-in-gh ()
  (interactive)
  (let* ((line-no (line-number-at-pos))
         (command
          (format "~/.emacs.d/etc/bin/git-surf -r%s,%s %s"
                  line-no line-no
                  (file-name-nondirectory (buffer-file-name)))))
    (message command)
    (shell-command command)))

(global-set-key (kbd "C-c g f") 'lk/open-current-file-in-gh)


(defun lk/open-current-pr-in-gh ()
  (interactive)
  (shell-command "~/.emacs.d/etc/bin/git-surf -p"))

(global-set-key (kbd "C-c g h") 'lk/open-current-pr-in-gh)

(use-package git

	:bind (("C-x C-g" . vc-git-grep)))

(use-package magit

  :config (setq magit-git-executable "/usr/bin/git")
  (setq magit-completing-read-function 'ivy-completing-read)
	:bind (( "C-c m s" . magit-status)))

(defun lk/magit-clear-buffers ()
  (interactive)
  (kill-matching-buffers ".*magit.*"))

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


(use-package keychain-environment

  :init (keychain-refresh-environment))

(provide 'lk/git)
;;; git.el ends here
