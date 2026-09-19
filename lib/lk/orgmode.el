;;; -*- lexical-binding: t; -*-

(use-package org
  :ensure nil
  :defer nil
  :config
  (setq org-return-follows-link t)
  (setq org-startup-folded 'fold)
  (setq org-hide-emphasis-markers t)
  (setq org-agenda-files (list (expand-file-name "~/Files/org/main.org"))))

(use-package org-appear
  :ensure t
  :hook org-mode)


(provide 'lk/orgmode)
