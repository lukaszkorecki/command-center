;;; -*- lexical-binding: t; -*-

(use-package org-appear :defer nil :ensure t :hook org-mode)

(use-package org-modern
  :ensure t
  :init
  (with-eval-after-load 'org (global-org-modern-mode)))

(defvar lk/org-inbox-file (expand-file-name "~/Files/org/inbox.org"))
(defvar lk/org-main-file (expand-file-name "~/Files/org/main.org"))
(defvar lk/org-calendar-file (expand-file-name "~/Files/org/calendar.org"))
(defvar lk/org-notes-file (expand-file-name "~/Files/org/notes.org"))
(defvar lk/org-done-file (expand-file-name "~/Files/org/done.org"))
(defvar lk/org-weekly-dir (expand-file-name "~/Files/org/notes/weekly-checkins/"))
(defvar lk/org-weekly-template (expand-file-name "~/Files/org/templates/ref.org"))

(defun lk/add-task ()
  "Adds task to ~/Files/org/main.org"
  (interactive)
  (org-capture nil "t"))

(defun lk/add-note ()
  "Adds note to ~/Files/org/notes.org"
  (interactive)
  (org-capture nil "n"))

(defun lk/archive-done ()
  "Move all done tasks from ~/Files/org/main.org to ~/Files/org/done.org"
  (interactive)
  (require 'org-archive)
  (with-current-buffer (find-file-noselect lk/org-main-file)
    (let ((org-archive-location (concat lk/org-done-file "::"))
          (org-archive-save-context-info '(time))
          (count 0))
      (org-map-entries
       (lambda ()
         (org-archive-subtree)
         (setq count (1+ count))
         (setq org-map-continue-from
               (org-element-begin (org-element-at-point))))
       "/DONE" 'file)
      (save-buffer)
      (when-let* ((buf (find-buffer-visiting lk/org-done-file)))
        (with-current-buffer buf (save-buffer)))
      (message "Archived %d task(s) to %s" count lk/org-done-file))))

(defun lk/resort-file-by-time ()
  "Resort current file by timestamps"
  (interactive)
  (message "booo"))

(defun lk/open-notes ()
  "Open ~/Files/org/notes.org in another window."
  (interactive)
  (find-file-other-window lk/org-notes-file))

(defun lk/open-main ()
  "Open ~/Files/org/main.org in another window."
  (interactive)
  (find-file-other-window lk/org-main-file))

(defun lk/open-inbox ()
  "Open ~/Files/org/inbox.org in another window."
  (interactive)
  (find-file-other-window lk/org-inbox-file))

(defun lk/org-weekly-file ()
  "Return the path of today's weekly check-in file.
Creates `lk/org-weekly-dir' if it does not exist, since `org-capture'
will not create missing directories itself.  Used as the capture target
for the weekly check-in template."
  (make-directory lk/org-weekly-dir t)
  (expand-file-name (format-time-string "%Y-%m-%d.org") lk/org-weekly-dir))

(defun lk/align ()
  "Align the table at point, or all tags in the buffer."
  (interactive)
  (if (org-at-table-p)
      (org-table-align)
    (org-align-tags t)))

(defun lk/org-today-agenda ()
  "Open the agenda, optionally filtered to TAG."
  (interactive)
  (org-agenda-list nil "d"))

(defun lk/org-week-agenda ()
  "Open the agenda, optionally filtered to TAG."
  (interactive)
  (org-agenda-list nil "w"))

(defvar lk/org-image-max-height 200
  "Maximum display height, in pixels, for inline images in Org buffers.")

(defun lk/org-image-clamp-height (image)
  "Constrain IMAGE to `lk/org-image-max-height' pixels tall.
Org only supports width limits, so this is applied as `:filter-return'
advice on `org--create-inline-image'.  Emacs rescales the width to match,
preserving the aspect ratio: `:width' overrides `:max-width' but never
`:max-height'."
  (when (and image lk/org-image-max-height)
    (setf (image-property image :max-height) lk/org-image-max-height))
  image)

(advice-add 'org--create-inline-image :filter-return #'lk/org-image-clamp-height)

(use-package transient
  :ensure nil
  :demand t
  :config ;;
  (transient-define-prefix lk/org
    ()
    "Org actions"
    [["Capture"
      ("t" "Add task" lk/add-task)
      ("n" "Add note" lk/add-note)
      ]
     ["Visit"
      ("a" "Today's agenda" lk/org-today-agenda)
      ("w" "This week agenda" lk/org-week-agenda)
      ("m" "Tasks" lk/open-main)
      ("i" "Inbox" lk/open-inbox)
      ("N" "Notes (notes.org)" lk/open-notes)
      ]
     ]

    [:if-mode org-mode
     ["Editing"
      ("r" "Refile" org-refile)
      ("S" "Sort by time" lk/resort-file-by-time)
      ("=" "Align" lk/align)
      ]

     ["Maintain" ("A" "Archive done" lk/archive-done)]
     ["View" ("#" "Toggle modern look" org-modern-mode)]
     ]))

(use-package org
  :ensure nil
  :defer nil
  :bind ("C-c o" . lk/org)
  :init
  (require 'org-agenda)
  :config ;
  (setq org-return-follows-link t)
  (setq org-startup-folded nil)
  (setq org-hide-emphasis-markers t)

  ;; inline images: preview every link on open.  Natural size, honouring
  ;; #+ATTR_ORG :width when present, clamped by max-width and by
  ;; `lk/org-image-max-height' above.
  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width nil)
  (setq org-image-max-width 'fill-column)

  (setq org-agenda-files (list lk/org-main-file lk/org-calendar-file))
  (setq org-log-done 'time)

  (setq org-refile-targets '((lk/org-main-file :level . 1)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)

  (setq org-agenda-custom-commands
        '(("d" "Today"
           ((agenda ""
                    ((org-agenda-span 'day)
                     (org-deadline-warning-days 0)))))
          ("w" "Week"
           ((agenda ""
                    ((org-agenda-span 'week)
                     (org-agenda-start-on-weekday nil)
                     (org-deadline-warning-days 0)))))))
  (setq org-capture-templates
        `(("t" "Task" entry
           (file lk/org-inbox-file)
           "* TODO %?  %^G\nSCHEDULED: %^t"
           :empty-lines 1)
          ("n" "Note" entry
           (file lk/org-notes-file)
           "* %?\n%U"
           :empty-lines 1)
          ("w" "Weekly check-in" entry
           (file lk/org-weekly-file)
           (file ,lk/org-weekly-template)
           :empty-lines 1))))

(provide 'lk/orgmode)
