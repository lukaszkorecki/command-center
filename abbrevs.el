;;; abbreviations --- useful abbreviations (snippets) ported from my vimrc
;;; Commentary:
;;; abbrevs work slightly different from vim's since they can be only composed
;;; of letters and numbers (so ci- will not work...)
;;; Code:

(defun abbrevs-jump-to-placeholder ()
  "Find placeholder string, nuke it and leave cursor there."
  (search-backward "XXX")
  (delete-char 3))

(setq abbrev-file-name "~/.emacs.d/abbrev_defs.el")
(read-abbrev-file abbrev-file-name t)
(setq dabbrev-case-replace nil)  ; Preserve case when expanding
(setq abbrev-mode t)
(setq abbrev-mode t)

(provide 'abbrevs)
;;; abbrevs.el ends here
