;;; -*- lexical-binding: t; -*-

(defvar lk/org-main-file (expand-file-name "~/Files/org/main.org"))
(defvar lk/org-notes-file (expand-file-name "~/Files/org/notes.org"))
(defvar lk/org-done-file (expand-file-name "~/Files/org/done.org"))

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
         (setq org-map-continue-from (org-element-begin (org-element-at-point))))
       "/DONE" 'file)
      (save-buffer)
      (when-let* ((buf (find-buffer-visiting lk/org-done-file)))
        (with-current-buffer buf (save-buffer)))
      (message "Archived %d task(s) to %s" count lk/org-done-file))))

(defun lk/open-notes ()
  "Open ~/Files/org/notes.org in another window."
  (interactive)
  (find-file-other-window lk/org-notes-file))

(defun lk/open-main ()
  "Open ~/Files/org/main.org in another window."
  (interactive)
  (find-file-other-window lk/org-main-file))

(defun lk/align ()
  "Align the table at point, or all tags in the buffer."
  (interactive)
  (if (org-at-table-p)
      (org-table-align)
    (org-align-tags t)))

(defun lk/org-agenda (&optional tag)
  "Open the agenda, optionally filtered to TAG."
  (interactive)
  (require 'org-agenda)
  (let ((org-agenda-tag-filter-preset (and tag (list (concat "+" tag)))))
    (org-agenda nil "a")))

(defun lk/org-tags ()
  "All tags used in `lk/org-main-file', sorted."
  (with-current-buffer (find-file-noselect lk/org-main-file)
    (sort (delete-dups (apply #'append (org-map-entries #'org-get-tags nil 'file)))
          #'string<)))

(defun lk/org-tag-keys (tags)
  "Alist of (KEY . TAG), assigning each of TAGS a unique transient key."
  (let ((used (list ?a)))
    (delq nil
          (mapcar (lambda (tag)
                    (when-let* ((key (seq-find (lambda (c) (not (memq c used)))
                                               (append tag (number-sequence ?0 ?9)))))
                      (push key used)
                      (cons key tag)))
                  tags))))

(defun lk/org-tag-suffixes (_children)
  (transient-parse-suffixes
   'lk/show-agenda
   (mapcar (lambda (cell)
             (let ((tag (cdr cell)))
               (list (char-to-string (car cell)) tag
                     (lambda () (interactive) (lk/org-agenda tag)))))
           (lk/org-tag-keys (lk/org-tags)))))

(use-package transient
  :ensure nil
  :demand t
  :config
  (transient-define-prefix lk/show-agenda
    ()
    "Agenda"
    [["All"
      ("a" "Any tag" lk/org-agenda)]
     ["By tag"
      :class transient-column
      :setup-children lk/org-tag-suffixes]])

  (transient-define-prefix lk/org
    ()
    "Org actions"
    [["Capture"
      ("t" "Add task" lk/add-task)
      ("n" "Add note" lk/add-note)]
     ["Visit"
      ("a" "Agenda" lk/show-agenda)
      ("m" "Tasks (main.org)" lk/open-main)
      ("N" "Notes (notes.org)" lk/open-notes)]
     ["Maintain"
      ("A" "Archive done" lk/archive-done)]]

    [:if-mode org-mode
     ["Timestamp"
      ("C" "Open calendar, use C-c < to fill from there" calendar)
      (">" "Up date field" org-timestamp-up-day)
      ("+" "Up time field" org-timestamp-up)
      ("<" "Down date field" org-timestamp-down-day)
      ("-" "Down time field" org-timestamp-down)]
     ["Editing"
      ("=" "Align" lk/align)]]))

(use-package org
  :ensure nil
  :defer nil
  :bind ("C-c o" . lk/org)
  :config
  (setq org-return-follows-link t)
  (setq org-startup-folded nil)
  (setq org-hide-emphasis-markers t)
  (setq org-agenda-files (list lk/org-main-file))

  (setq org-capture-templates
        '(("t" "Task" entry (file lk/org-main-file)
           "* TODO %?  %^G\nSCHEDULED: %^t"
           :empty-lines 1)
          ("n" "Note" entry (file lk/org-notes-file)
           "* %?\n%U"
           :empty-lines 1))))

(use-package org-appear
  :ensure t
  :hook org-mode)


(provide 'lk/orgmode)
