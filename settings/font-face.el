;;; font-face.el --- set fonts and such
;;;; Commentary:
;;; Sets default font and provides functions for switching between sizes
;;; Code:

(defvar lk/font "Menlo"
  "Default font name for GUI Emacs.")

(defun lk/set-font-size (my-size)
  "Set Monaco as Gui font with specified MY-SIZE."
  (interactive )
  (custom-set-faces
   `(default
      ((t (:weight normal :height ,my-size :width normal :family ,lk/font))))
   `(fixed-pitch ((t (:family ,lk/font))))))


(defun lk/normal-font ()
  "Set font to normal work size."
  (interactive)
  (lk/set-font-size 110))


(defun lk/medium-font ()
  "Set font to normal work size."
  (interactive)
  (lk/set-font-size 130))

(defun lk/presentation-mode()
  "Set font to presentation/teaching size."
  (interactive)
  (lk/set-font-size 185)
  (load-theme 'sanityinc-tomorrow-day t))

(provide 'font-face)
;;; font-face.el ends here
