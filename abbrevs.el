(define-abbrev-table 'js2-mode-abbrev-table
  '(
    ("ci-" "console.info(" nil 0)
    ("deb_" "debugger;" nil 0)))

(define-abbrev-table 'ruby-mode-abbrev-table
  '(
    ("deb_" "require 'pry' ; binding.pry" nil 0)
    ("df=" "def
end" 0 0)))

(setq save-abbrevs nil)
(setq-default abbrev-mode t)
(setq dabbrev-case-replace nil)
