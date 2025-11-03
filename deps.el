;;; deps.el --- deps.el initialize package management
;;; Commentary:

;;; Code:
(add-to-list 'load-path "~/.emacs.d/lib")
(add-to-list 'load-path "~/.emacs.d/lib/lk")

(setq package-enable-at-startup t)
(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(setq package-archive-priorities
      '(("gnu" . 2)
        ("nongnu" . 5)
        ("melpa-stable" . 8)
        ("melpa" . 10)))

(package-initialize)

(provide 'deps)
;;; deps.el ends here
