(use-package s)
(use-package window-number)
(use-package git)

;; vc mode line needs refreshing every now and then
(setq auto-revert-check-vc-info t)

(defun vc-status-mode-line ()
  "Builds a source control string or nil."
  (when vc-mode
    `(" "
      ,(s-trim (substring-no-properties vc-mode))
      " ")))

;; customize the mode-line
(setq-default
 mode-line-format
 (list
'(:eval (propertize
           (format "> W:%s " (window-number))
           'face 'font-lock-comment-face))
  '(:eval (vc-status-mode-line))
  ;; buffername
  '(:eval (propertize "%b " 'face 'font-lock-keyword-face))

  " L:%l C:%c | "

  ;; major mode
  '(:eval (propertize "%m " 'face 'font-lock-comment-face))

  ;; modified * / RO % / no changes -
  '(:eval (propertize " %*" 'face 'font-lock-warning-face))

  '(global-mode-string global-mode-string)))


(provide 'lk/modeline)
