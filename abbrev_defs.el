;;-*-coding: utf-8;-*-
(define-abbrev-table 'Buffer-menu-mode-abbrev-table '())

(define-abbrev-table 'js2-mode-abbrev-table
  '(
    ("ci-" "console.info(" nil 0)
    ("cne" "console.error(XXX);" abbrevs-jump-to-placeholder 0)
    ("cni" "console.info(XXX);" abbrevs-jump-to-placeholder 0)
    ("cnl" "console.log(XXX);" abbrevs-jump-to-placeholder 0)
    ("dbg" "debugger;" nil 0)
    ("deb_" "debugger;" nil 0)
    ("fn" "function XXX() {
}" abbrevs-jump-to-placeholder 0)
    ("fna" "function (XXX) { }" abbrevs-jump-to-placeholder 0)
   ))

(define-abbrev-table 'ruby-mode-abbrev-table
  '(
    ("bl" "do
XXX
end" abbrevs-jump-to-placeholder 2)
    ("bla" "do |XXX|
end" abbrevs-jump-to-placeholder 1)
    ("bli" "{ |XXX| }" abbrevs-jump-to-placeholder 0)
    ("ci-" "console.info(" nil 0)
    ("cls" "classXXX
end" abbrevs-jump-to-placeholder 0)
    ("dbg" "require 'pry' ; binding.pry" nil 1)
    ("deb_" "require 'pry' ; binding.pry" nil 0)
    ("df" "defXXX
end" abbrevs-jump-to-placeholder 1)
    ("df=" "def
end" 0 0)
    ("mod" "moduleXXX
end" abbrevs-jump-to-placeholder 0)
    ("pry_" "require 'pry' ; binding.pry" 0 0)
   ))

(define-abbrev-table 'shell-mode-abbrev-table '())
