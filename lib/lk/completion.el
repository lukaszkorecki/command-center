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

(use-package corfu
  :custom ;
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match 'insert) ;; Configure handling of exact matches
  :init (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package emacs
  :after (corfu)
  :custom ;
  (read-extended-command-predicate #'command-completion-default-include-p)
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (enable-recursive-minibuffers t)
  (history-delete-duplicates t)
  ;; in-buffer completion
  (tab-always-indent 'complete)
  (completion-cycle-threshold 3)
  (text-mode-ispell-word-completion nil)
  ;; No bindings for buffer/imenu/goto-line/kill-ring here: the consult
  ;; equivalents live in `lk/consult' (C-c d), and the keys they used to shadow
  ;; are the stock Emacs ones anyway.
  :bind (("C-c s" . occur)
         ("C-x c u" . lk/urls-in-buffer->select->browse)))

(use-package minibuffer
  :custom ;;

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
  :config ;;
  ;; make C-n/C-p work as nav in minibuffer
  (keymap-set minibuffer-visible-completions-up-down-map "C-n"
              (minibuffer-visible-completions--bind #'minibuffer-next-completion))
  (keymap-set minibuffer-visible-completions-up-down-map "C-p"
              (minibuffer-visible-completions--bind #'minibuffer-previous-completion))
  ;; hide help
  (setq completion-show-help nil)
  (setq completion-show-inline-help nil)
  (setq completions-detailed t)
  (setq completions-sort 'historical)
  (setq completion-auto-help t)
  (setq completion-eager-display t)
  (setq completion-eager-update t))

;; Single source of truth for `completion-styles'. Styles are tried in order and
;; the first one that matches wins, so: orderless for space-separated
;; out-of-order input ("status magit"), then flex as the trailing fallback for
;; single-token subsequences ("mgs" -> magit-status), which orderless does not
;; do by default.
(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless basic flex))
  (completion-category-overrides
   '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

(use-package savehist :init (savehist-mode 1))

(use-package recentf :init (recentf-mode 1))

(use-package mb-depth :init (minibuffer-depth-indicate-mode 1))

(use-package completion-preview
  :demand t
  ;; Cycle inline preview candidates with the same keys the minibuffer and the
  ;; in-buffer *Completions* already use. Not M-n/M-p: those are
  ;; forward/backward-paragraph here and history navigation in the minibuffer.
  :bind ( :map completion-preview-active-mode-map
          ("M-<down>" . completion-preview-next-candidate)
          ("M-<up>" . completion-preview-prev-candidate))
  :config (global-completion-preview-mode 1))

(use-package consult
  :defer t
  ;; Everything else lives in `lk/consult' (C-c d). Only the map-local entries
  ;; stay as key bindings: isearch needs them to detect that it is running, and
  ;; the minibuffer ones replace the built-in history matching commands.
  :bind ( :map isearch-mode-map
          ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
          ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
          ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
          ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
          :map minibuffer-local-map
          ("M-s" . consult-history)                 ;; orig. next-matching-history-element
          ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; The :init configuration is always executed (Not lazy)
  :init (require 'transient)
  (transient-define-prefix lk/consult
    ()
    "Consult"
    [["Buffers"
      ("b" "Buffer"         consult-buffer)
      ("B" "Other frame"    consult-buffer-other-frame)
      ("p" "Project buffer" consult-project-buffer)
      ]
     ["Search"
      ("l" "Line"        consult-line)
      ("L" "Line (multi)" consult-line-multi)
      ("r" "Ripgrep"     consult-ripgrep)
      ("g" "Grep"        consult-grep)
      ("G" "Git grep"    consult-git-grep)
      ("f" "Find file"   consult-find)
      ]
     ["Goto"
      ("i" "Imenu"         consult-imenu)
      ("I" "Imenu (multi)" consult-imenu-multi)
      ("n" "Line number"   consult-goto-line)
      ("e" "Compile error" consult-compile-error)
      ("d" "Diagnostic"    consult-flymake)
      ("m" "Grep match"    consult-grep-match)
      ]
     ["Misc"
      ("y" "Yank pop"        consult-yank-pop)
      ("h" "History"         consult-history)
      ("s" "Isearch history" consult-isearch-history)
      ("k" "Keep lines"      consult-keep-lines)
      ("u" "Focus lines"     consult-focus-lines)
      ("R" "Register"        consult-register)
      ("x" "Mode command"    consult-mode-command)
      (":" "Complex command" consult-complex-command)
      ]])

  (global-set-key (kbd "C-c d") 'lk/consult)

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config ;;
  (consult-customize
   consult-theme :preview-key
   '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any)))

(provide 'lk/completion)
