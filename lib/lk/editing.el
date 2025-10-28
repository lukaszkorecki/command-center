;;; editing.el --- ...
;;; Commentary:

;;; Code:

;; strip whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace )

;; add final newline automaticaly
(setq require-final-newline t)

;; indenting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default standard-indent 2)

(define-key global-map (kbd "RET") 'newline-and-indent)

(define-key global-map (kbd "C-c R") 'revert-buffer)

(defun lk/select-line ()
  "Select current line"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun lk/join-lines ()
  (interactive)
  (next-line)
  (end-of-line)
  (join-line))

;; Disable certain commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'magit-clean 'disabled nil)

;; disable c-z which maps to minimize
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x z"))
(global-unset-key (kbd "C-x C-z"))

(global-set-key (kbd "C-x =") 'indent-according-to-mode)

(global-set-key (kbd "C-x l") 'lk/select-line)
(global-set-key (kbd "C-x j") 'lk/join-lines)

(global-set-key (kbd "C-c n k") 'lk/show-kill-ring)

;; Editing and general syntax highlighting

;; bind awkard M-[ & M-] to something better
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; override C-x C-o with a variant which:
;; deletes all blank lines and inserts a new one
(defun lk/reduce-blank-lines ()
  (interactive)
  (delete-blank-lines)
  (end-of-line)
  (insert-char "\n" 1))

(use-package visual-regexp
  :ensure t
  :init (global-set-key (kbd "C-x R") 'vr/replace)
  :bind (( "C-x R" . vr/replace)))

;; no backup files
(setq-default make-backup-files nil)
;; no lockfiles
(setq create-lockfiles nil)
;; autosaves
(make-directory "~/.emacs_autosave/" t)
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs_autosave/" t)))

(use-package string-inflection
  :ensure t
  :after transient
  :config (progn
            (transient-define-prefix lk/string-inflection
              ()
              "Inflect all the things"
              [["camelCase"
                ("l" "lowerCamelCase"  string-inflection-lower-camelcase )
                ("u" "UpperCamelCase"  string-inflection-camelcase )
                ]
               ["snake_case/underscore"
                ("s" "snake_case"  string-inflection-underscore )
                ("k" "SNAKE_CASE"  string-inflection-upcase-underscore )
                ]
               ["kebab-case" ("d" "kebab-case"  string-inflection-kebab-case )]])
            (global-set-key (kbd "C-x c l") 'lk/string-inflection)))

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode nil)
  :config (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-auto-save-history nil)
  :bind (("C-c u" . undo-tree-visualize)))

(put 'narrow-to-region 'disabled nil)

(use-package mutliple-cursors
  :ensure t
  :bind (("C-c a" . mc/mark-all-like-this)))

(provide 'lk/editing)
;;; editing.el ends here
