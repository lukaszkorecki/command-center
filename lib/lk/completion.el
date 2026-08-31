;;; -*- lexical-binding: t; -*-
;;; completion.el - built-in completion only (Emacs 31+)

(setq completion-styles '(basic partial-completion flex)
      completion-pcm-leading-wildcard t
      completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

;; *Completions* opens eagerly and refilters as you type
(setq completion-eager-display t
      completion-eager-update t
      completions-format 'one-column
      completions-max-height 15
      completions-detailed t
      completions-sort 'historical
      completions-header-format nil
      completion-auto-help 'visible
      completion-auto-select nil
      minibuffer-visible-completions 'up-down
      enable-recursive-minibuffers t
      history-delete-duplicates t)

;; C-n/C-p alongside the arrows. `minibuffer-visible-completions--bind' wraps
;; the command in a :filter so it only shadows next-line/previous-line while
;; *Completions* is actually on screen.
(keymap-set minibuffer-visible-completions-up-down-map "C-n"
            (minibuffer-visible-completions--bind #'minibuffer-next-completion))
(keymap-set minibuffer-visible-completions-up-down-map "C-p"
            (minibuffer-visible-completions--bind #'minibuffer-previous-completion))

(savehist-mode 1)
(recentf-mode 1)
(minibuffer-depth-indicate-mode 1)

;; in-buffer completion
(setq tab-always-indent 'complete
      completion-cycle-threshold 3)
(global-completion-preview-mode 1)

;; Cycle inline preview candidates with the same keys the minibuffer and the
;; in-buffer *Completions* already use. Not M-n/M-p: those are
;; forward/backward-paragraph here and history navigation in the minibuffer.
(with-eval-after-load 'completion-preview
  (keymap-set completion-preview-active-mode-map
              "M-<down>" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map
              "M-<up>" #'completion-preview-prev-candidate))

(keymap-global-set "M-y" #'yank-from-kill-ring)
(keymap-global-set "C-c s" #'occur)
(keymap-global-set "C-c n i" #'imenu)
(keymap-global-set "C-x b" #'switch-to-buffer)
(keymap-global-set "M-g M-g" #'goto-line)

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

(global-set-key
 (kbd "C-x c u")
 'lk/urls-in-buffer->select->browse)

(provide 'lk/completion)
