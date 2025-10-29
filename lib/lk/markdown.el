(use-package markdown-ts-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-ts-mode)
  :defer 't
  :config (add-to-list 'treesit-language-source-alist
                       '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
  (add-to-list 'treesit-language-source-alist
               '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src"))

  :bind (:map markdown-ts-mode-map
              ("C-c C-e" . lk/markdown-edit-code-block)))

(use-package edit-indirect :ensure t)

(require 'edit-indirect)

(defun lk/markdown-edit-code-block ()
  "Edit the code block at point in a separate buffer with appropriate major mode."
  (interactive)
  (unless (treesit-available-p)
    (user-error "Tree-sitter is not available"))

  (let* ((node (treesit-node-at (point)))
         ;; Find the fenced_code_block parent
         (code-block
          (treesit-parent-until
           node
           (lambda (n)
             (equal (treesit-node-type n) "fenced_code_block")))))

    (unless code-block (user-error "Not in a code block"))

    ;; Extract language and content from children
    (let* ((children (treesit-node-children code-block))
           (info-string nil)
           (content-node nil)
           (language nil))

      ;; Find the info_string and code_fence_content nodes
      (dolist (child children)
        (pcase (treesit-node-type child)
          ("info_string" (setq info-string child))
          ("code_fence_content" (setq content-node child))))

      (unless content-node
        (user-error "Could not find code content in block"))

      (setq language
            (when info-string
              (string-trim (treesit-node-text info-string t))))

      (let* ((beg (treesit-node-start content-node))
             (end (treesit-node-end content-node))
             (edit-indirect-guess-mode-function
              (lambda (_parent-buffer _beg _end)
                (lk/set-mode-from-language language))))
        (edit-indirect-region beg end t)))))

(defun lk/set-mode-from-language (lang)
  "Set major mode based on LANG string from code fence."
  (let* ((lang (or lang ""))
         (mode
          (cond
           ;; Clojure variants
           ((string-match-p
             "^\\(clj\\|clojure\\|cljs\\|clojurescript\\|cljc\\)$" lang)
            'clojure-mode)
           ;; Lisp family
           ((string-match-p "^\\(elisp\\|emacs-lisp\\)$" lang)
            'emacs-lisp-mode)
           ((string-match-p "^lisp$" lang)
            'lisp-mode)
           ((string-match-p "^scheme$" lang)
            'scheme-mode)
           ;; JavaScript/TypeScript
           ((string-match-p "^\\(js\\|javascript\\)$" lang)
            'js-mode)
           ((string-match-p "^\\(ts\\|typescript\\)$" lang)
            'typescript-mode)
           ;; Python
           ((string-match-p "^\\(py\\|python\\)$" lang)
            'python-mode)
           ;; Shell
           ((string-match-p "^\\(sh\\|bash\\|shell\\)$" lang)
            'sh-mode)
           ;; Ruby
           ((string-match-p "^\\(rb\\|ruby\\)$" lang)
            'ruby-mode)
           ;; Java
           ((string-match-p "^java$" lang)
            'java-mode)
           ;; C/C++
           ((string-match-p "^c$" lang)
            'c-mode)
           ((string-match-p "^\\(cpp\\|c\\+\\+\\)$" lang)
            'c++-mode)
           ;; Rust
           ((string-match-p "^rust$" lang)
            'rust-mode)
           ;; Go
           ((string-match-p "^go$" lang)
            'go-mode)
           ;; Data formats
           ((string-match-p "^json$" lang)
            'json-mode)
           ((string-match-p "^\\(yaml\\|yml\\)$" lang)
            'yaml-mode)
           ((string-match-p "^\\(xml\\|html\\)$" lang)
            'html-mode)
           ((string-match-p "^css$" lang)
            'css-mode)
           ;; SQL
           ((string-match-p "^sql$" lang)
            'sql-mode)
           ;; Fallback
           (t 'fundamental-mode))))
    (funcall mode)))



;; helpers for markdown and writing in general
(defun lk/insert-current-date ()
  "Insert current date at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun lk/insert-current-date-time ()
  "Insert current date at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M")))

(defun lk/insert-random-uuid ()
  "Insert a random UUID at point."
  (interactive)
  (insert (uuid-string)))

(defun lk/insert-md-callout (callout-type)
  "Insert a markdown callout of type CALLOUT-TYPE at point."
  (interactive "sCallout type: ")
  (insert (format "> [!%s]\n" callout-type)))

(defun lk/gh-preview-markdown ()
  "Preview markdown file in browser by rendeding it to html using `ghmd-preview` script
  which returns the local file path of the rendered html."
  (interactive)
  (let* ((file-name (buffer-file-name))
         ;;         (tmp-file-name (format "%s.html" (make-temp-file "ghmd-preview")))
         ;; create preview next to the original file but with .html extension added)
         (preview-file-name (concat file-name ".html"))
         (html-file
          (shell-command-to-string
           (format "ghmd-preview -f %s -o %s" file-name preview-file-name)))
         (browseable-file-path (format "file://%s" html-file)))
    (message "Previewing %s" browseable-file-path)
    ;; (xwidget-webkit-browse-url browseable-file-path)
    (browse-url browseable-file-path)))

(provide 'lk/markdown)
