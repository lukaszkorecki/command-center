;; Git and git-surf helpers

(when (string-equal system-type "Darwin")
)

(defun lk/open-current-file-in-gh ()
  (interactive)
  (let* ((line-no (line-number-at-pos))
         (command (format "~/.emacs.d/etc/bin/git-surf -r%s,%s %s"
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
  :ensure t
	:bind (("C-x C-g" . vc-git-grep)))

(use-package magit
  :ensure t
  :config
  (setq magit-git-executable "/usr/bin/git")
  (setq magit-completing-read-function 'ivy-completing-read)
	:bind
  (( "C-c m s" . magit-status)))

(defun lk/magit-clear-buffers ()
  (interactive)
  (kill-matching-buffers ".*magit.*"))

(use-package ibuffer-vc
  :ensure t)

(use-package ibuffer-git
  :ensure t
  :init
  (setq ibuffer-formats
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
  :ensure t
  :init (keychain-refresh-environment))

(provide 'lk/git)
