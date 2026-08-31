;;; -*- lexical-binding: t; -*-
(use-package markdown-ts-mode
  :ensure nil
  :mode ("\\.md\\'" "\\.mdx\\'" "\\.markdown\\'")
  :config
  (require 'markdown-ts-mode-x))

(defun lk/preview-markdown ()

  "Render markdown using ~/.emacs.d/etc/bin/markdown server and open in eww."
  (interactive)

  ;; steps
  ;; markdown <path to md file>
  ;; output is HTML
  ;; save in tmp file
  ;; open it using eeww

  (let* ((file-name (buffer-file-name))
         (tmp-file-name (make-temp-file "lk-md-preview" nil ".html"))
         (command
          (format "~/.emacs.d/etc/bin/markdown %s > %s"
                  (shell-quote-argument file-name)
                  (shell-quote-argument tmp-file-name))))
    (shell-command command)
    (eww-open-file tmp-file-name)))

(provide 'lk/markdown)
