;;; modeline.el --- modeline, you know it
;;; Commentary:

;;; Code:



(use-package s)

(use-package window-number)

;; vc mode line needs refreshing every now and then
(setq auto-revert-check-vc-info t)

(defun vc-status-mode-line ()
  "Builds a source control string or nil."
  (when vc-mode
    `(" " ,(s-trim (substring-no-properties vc-mode)) " ")))

;; customize the mode-line
(setq-default
 mode-line-format
 (list
  '(:eval (vc-status-mode-line))

  ;; buffername, line, column
  "| %b | L:%l C:%c | "
  ;; major mode
  '(:eval
    (propertize "%m " 'face 'font-lock-constant-face)
    ;; modified * / RO % / no changes -
    '(:eval (propertize " %*" 'face 'font-lock-warning-face))

    '(global-mode-string global-mode-string))))


(provide 'lk/modeline)
;;; modeline.el ends here
