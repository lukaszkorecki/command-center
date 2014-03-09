;;; abbreviations --- useful abbreviations (snippets) ported from my vimrc
;;; Commentary:
;;; abbrevs work slightly different from vim's since they can be only composed
;;; of letters and numbers (so ci- will not work...)
;;; Code:

(defun abbrevs-jump-to-placeholder ()
  "Find placeholder string, nuke it and leave cursor there."
  (search-backward "XXX")
  (delete-char 3))

(define-abbrev-table 'js2-mode-abbrev-table
  '(
    ("cni" "console.info(XXX);" abbrevs-jump-to-placeholder)
    ("cnl" "console.log(XXX);" abbrevs-jump-to-placeholder)
    ("cne" "console.error(XXX);" abbrevs-jump-to-placeholder)
    ("fna" "function (XXX) { }" abbrevs-jump-to-placeholder)
    ("fn" "function XXX() {\n}" abbrevs-jump-to-placeholder)
    ("dbg" "debugger;" )))

(define-abbrev-table 'ruby-mode-abbrev-table
  '(
    ; df -> method definition
    ("df" "defXXX\nend" abbrevs-jump-to-placeholder)
    ; cls -> class definition
    ("cls" "classXXX\nend" abbrevs-jump-to-placeholder)
    ; mod -> module
    ("mod" "moduleXXX\nend" abbrevs-jump-to-placeholder)
    ; bl -> do...end block with args
    ("bl"  "do |XXX|\nend" abbrevs-jump-to-placeholder)
    ; ble -> do...end block with no args
    ("bl"  "do\nXXX\nend" abbrevs-jump-to-placeholder)
    ; bli -> { |args| } block
    ("bli" "{ |XXX| }" abbrevs-jump-to-placeholder)
    ("dbg" "require 'pry' ; binding.pry")))

(setq abbrev-mode t)

(provide 'abbrevs)
;;; abbrevs.el ends here
