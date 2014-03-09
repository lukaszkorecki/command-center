;;; install.el --- install packages listed in evil.el and my.el
;;; Commentary:
;;; Poor man's cask emulator.
;;; kinda.

;;; Code:
(load-file "package/init.el")
(package-list-packages)

(load-file "package/evil.el")
(load-file "package/my.el")

(defun install-from-list (list)
  "Install all packages from given LIST, only if not installed."
  (mapc (lambda (name)
	  (message "Installing %s" name)
	  (if (package-installed-p name)
	      (message "  Package %s is already installed" name)
	    (package-install name))) list))


(install-from-list my-packages)
(install-from-list evil-packages)

(provide 'install)
;;; install.el ends here
