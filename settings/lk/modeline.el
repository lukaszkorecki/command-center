;;; modeline.el --- modeline, you know it
;;; Commentary:

;;; Code:
(use-package s)

;; vc mode line needs refreshing every now and then
(setq auto-revert-check-vc-info t)

(defun vc-status-mode-line ()
  "Builds a source control string or nil."
  (when vc-mode
    `(" / " ,(s-trim (substring-no-properties vc-mode)) " / ")))

(use-package nyan-mode :ensure t)


;; track the selected window and use that control what the mode-line shows
;; stolen from https://emacs.stackexchange.com/a/26345/13060
(defvar lk/selected-window nil)

(defun lk/capture-selected-window ()
  "Capture the selected window."
  (setq lk/selected-window (selected-window)))

(defun lk/force-update-mode-line ()
  "Force update the mode line."
  (force-mode-line-update t))

(add-hook 'post-command-hook #'lk/capture-selected-window)
(add-hook 'buffer-list-update-hook #'lk/force-update-mode-line)

(setq-default mode-line-format
              (list
               " "
               '(:eval
                 (window-parameter
                  (selected-window)
                  'ace-window-path))

               '(:eval (vc-status-mode-line))
               ;; buffername, line, column
               "/ %b / L:%l C:%c / "
               ;; major-mode--suspendedr mode
               '(:eval
                 (propertize "%m " 'face 'font-lock-constant-face)
                 ;; modified * / RO % / no changes -
                 '(:eval
                   (propertize " %*" 'face 'font-lock-warning-face))
                 '(global-mode-string global-mode-string))
               " / "
               '(:eval
                 (when (eq lk/selected-window (selected-window))
                   (list (nyan-create))))))


(provide 'lk/modeline)
;;; modeline.el ends here
