;;; erc.el --- ERC customizations and such

(require 'erc-colorize)
(erc-colorize-mode 1)

(require 'erc-image)
(add-to-list 'erc-modules 'image)
(erc-update-modules)

(require 'erc-hl-nicks)

(provide 'lk/erc)
