(load-file "package/init.el")
(package-list-packages)

(load-file "package/evil.el")
(load-file "package/my.el")

(defun install-from-list (list)
  (mapc (lambda (name)
	  (message "Installing %s" name)
	  (if (package-installed-p name)
	      (message "  Package %s is already installed" name)
	    (package-install name))) list))


(install-from-list my-packages)
(install-from-list evil-packages)
