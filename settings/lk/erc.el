;;; erc.el --- erc settings, overrides etc

;; ERC
(require 'erc-track)
(erc-track-mode 1)
(setq erc-track-position-in-mode-line 'after-modes)
(require 'erc-terminal-notifier)
(erc-notify-mode 1)

(require 'erc-hl-nicks)
(erc-hl-nicks-enable)


(defun lk/erc-connect ()
  (interactive)
  (erc-tls :server (getenv "CS_IRC_SERVER")
           :port 11224
           :password (getenv "CS_NN_PASS")
           :nick "lukasz"))

(provide 'lk/erc)
