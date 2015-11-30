;;; weechat --- Weechat settings file
;;;; Commentary:
; Auto accepts self-signed cert for the weechat relay
(require 'weechat)
(require 'gnutls)
(require 'sauron)
(add-to-list 'gnutls-trustfiles (expand-file-name "~/.private/weechat/ssl/relay.pem"))

(setq weechat-color-list '(unspecified "black" "dim gray" "dark red" "red"
                                       "dark green" "green" "brown"
                                       "orange" "dark blue" "blue"
                                       "dark magenta" "magenta" "dark cyan"
                                       "royal blue" "dark gray" "gray"))

(load-library "weechat-sauron")
(sauron-start)
(provide 'lk/weechat)

;;; weechat.el ends here
