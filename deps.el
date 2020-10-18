(progn
  (require 'package)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (setq package-check-signature nil)
  (when (string-match-p "26" (emacs-version))
    (package-initialize))
  (dolist (package '(use-package))
    (unless (package-installed-p package)
      (package-refresh-contents)
      (package-install package)))

  (eval-when-compile
    (require 'use-package))
  (setq use-package-always-ensure t)

  (add-to-list 'load-path "~/.emacs.d/settings")
  (add-to-list 'load-path "~/.emacs.d/site-lisp")
  (add-to-list 'load-path "~/.emacs.d/elpa")

  ;; fix not finding things in the load path... No idea why this breaks for
  ;; some packages, but works for others...
  (mapc (lambda (d)
          (when
              (not
               (or (string-equal d ".") (string-equal d "..")))
            (add-to-list 'load-path (format "~/.emacs.d/elpa/%s" d))))
        (directory-files "~/.emacs.d/elpa/")))
