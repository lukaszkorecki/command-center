;;; -*- lexical-binding: t; -*-
;;; completion.el - built-in completion (Emacs 31+) plus orderless

(defun lk/urls-in-buffer->select->browse ()
  "Find URLs in the current buffer and open the selected one in a browser."
  (interactive)

  (let ((urls nil)
        (beg (point-min))
        (end (point-max)))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward
              "\\(https?://\\|ftp://\\|file:///\\)[-A-Za-z0-9+&@#/%?=~_|!:,.;]*[-A-Za-z0-9+&@#/%=~_|]" nil t)
        (push (match-string 0) urls)))
    (when urls
      (message "URLS: %s" urls)
      (let ((selected-url (completing-read "Select URL: " urls nil t)))
        (browse-url selected-url)))))

(use-package emacs
  :custom
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (enable-recursive-minibuffers t)
  (history-delete-duplicates t)
  ;; in-buffer completion
  (tab-always-indent 'complete)
  (completion-cycle-threshold 3)
  :bind (("M-y" . yank-from-kill-ring)
         ("C-c s" . occur)
         ("C-c n i" . imenu)
         ("C-x b" . switch-to-buffer)
         ("M-g M-g" . goto-line)
         ("C-x c u" . lk/urls-in-buffer->select->browse)))

(use-package minibuffer
  :custom
  ;; *Completions* opens eagerly and refilters as you type
  (completion-eager-display t)
  (completion-eager-update t)
  (completions-format 'one-column)
  (completions-max-height 15)
  (completions-detailed t)
  (completions-sort 'historical)
  (completions-header-format nil)
  (completion-auto-help 'visible)
  (completion-auto-select nil)
  (minibuffer-visible-completions 'up-down)
  :config
  ;; C-n/C-p alongside the arrows. `minibuffer-visible-completions--bind' wraps
  ;; the command in a :filter so it only shadows next-line/previous-line while
  ;; *Completions* is actually on screen.
  (keymap-set minibuffer-visible-completions-up-down-map "C-n"
              (minibuffer-visible-completions--bind #'minibuffer-next-completion))
  (keymap-set minibuffer-visible-completions-up-down-map "C-p"
              (minibuffer-visible-completions--bind #'minibuffer-previous-completion)))

;; Single source of truth for `completion-styles'. Styles are tried in order and
;; the first one that matches wins, so: orderless for space-separated
;; out-of-order input ("status magit"), then flex as the trailing fallback for
;; single-token subsequences ("mgs" -> magit-status), which orderless does not
;; do by default.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic flex))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

(use-package savehist
  :init (savehist-mode 1))

(use-package recentf
  :init (recentf-mode 1))

(use-package mb-depth
  :init (minibuffer-depth-indicate-mode 1))

(use-package completion-preview
  :demand t
  ;; Cycle inline preview candidates with the same keys the minibuffer and the
  ;; in-buffer *Completions* already use. Not M-n/M-p: those are
  ;; forward/backward-paragraph here and history navigation in the minibuffer.
  :bind ( :map completion-preview-active-mode-map
          ("M-<down>" . completion-preview-next-candidate)
          ("M-<up>" . completion-preview-prev-candidate))
  :config (global-completion-preview-mode 1))

(provide 'lk/completion)
