;;; modeline.el --- modeline, you know it
;;; Commentary:

;;; Code:
(use-package s :ensure t)

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
               ;; buffername, line, column, mode
               " / %b / L:%l C:%c / %m / "))

(provide 'lk/modeline)
;;; modeline.el ends here
