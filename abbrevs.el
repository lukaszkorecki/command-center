(define-abbrev-table 'global-abbrev-table '(
    ("pry_" "require 'pry' ; binding.pry" )
    ("deb_" "debugger;" )
    ("ci-" "console.info(" )))

(setq save-abbrevs nil)
(setq-default abbrev-mode t)
(setq dabbrev-case-replace nil)
