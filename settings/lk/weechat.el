;;; weechat --- Weechat settings file
;;;; Commentary:
; Auto accepts self-signed cert for the weechat relay
(require 'weechat)
(require 'gnutls)
(add-to-list 'gnutls-trustfiles (expand-file-name "~/.private/weechat/ssl/relay.pem"))


(provide 'weechat)
