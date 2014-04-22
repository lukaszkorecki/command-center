;;-*-coding: utf-8;-*-
(define-abbrev-table 'Buffer-menu-mode-abbrev-table '())

(define-abbrev-table 'awk-mode-abbrev-table '())

(define-abbrev-table 'c++-mode-abbrev-table '())

(define-abbrev-table 'c-mode-abbrev-table '())

(define-abbrev-table 'comint-mode-abbrev-table '())

(define-abbrev-table 'completion-list-mode-abbrev-table '())

(define-abbrev-table 'emacs-lisp-byte-code-mode-abbrev-table '())

(define-abbrev-table 'emacs-lisp-mode-abbrev-table '())

(define-abbrev-table 'flycheck-error-list-mode-abbrev-table '())

(define-abbrev-table 'fundamental-mode-abbrev-table '())

(define-abbrev-table 'gfm-mode-abbrev-table '())

(define-abbrev-table 'global-abbrev-table '())

(define-abbrev-table 'grep-mode-abbrev-table '())

(define-abbrev-table 'help-mode-abbrev-table '())

(define-abbrev-table 'idl-mode-abbrev-table '())

(define-abbrev-table 'java-mode-abbrev-table '())

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

(define-abbrev-table 'lisp-mode-abbrev-table '())

(define-abbrev-table 'markdown-mode-abbrev-table '())

(define-abbrev-table 'objc-mode-abbrev-table '())

(define-abbrev-table 'occur-edit-mode-abbrev-table '())

(define-abbrev-table 'occur-mode-abbrev-table '())

(define-abbrev-table 'outline-mode-abbrev-table '())

(define-abbrev-table 'package-menu-mode-abbrev-table '())

(define-abbrev-table 'pike-mode-abbrev-table '())

(define-abbrev-table 'process-menu-mode-abbrev-table '())

(define-abbrev-table 'prog-mode-abbrev-table '())

(define-abbrev-table 'rspec-compilation-mode-abbrev-table '())

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

(define-abbrev-table 'special-mode-abbrev-table '())

(define-abbrev-table 'tabulated-list-mode-abbrev-table '())

(define-abbrev-table 'text-mode-abbrev-table '())

(define-abbrev-table 'vc-git-log-edit-mode-abbrev-table '())

(define-abbrev-table 'vc-git-log-view-mode-abbrev-table '())

(define-abbrev-table 'web-mode-abbrev-table '())

(define-abbrev-table 'yaml-mode-abbrev-table '())

