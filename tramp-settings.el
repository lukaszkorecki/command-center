; tramp configuration
(require 'tramp)
(setq tramp-default-method "ssh")

; functions for quickly browsing Vagrant vms
(defun nav-dev-machine ()
  "Navigate to vagrant VM"
  (interactive)
  (find-file-other-window "/vagrant@192.168.33.11:/home/vagrant"))
