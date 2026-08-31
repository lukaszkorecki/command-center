;;; -*- lexical-binding: t; -*-

(defun lk/read-file (file &optional oneline)
  (with-temp-buffer
  	(insert-file-contents file)
	  (goto-char (point-min))
	  (buffer-substring
     (point)
		 (if oneline
         ;; read only 1st line
         (line-end-position)
       ;; read whole file
       (point-max)))))


(defun lk/invoke-cli (bufname cmd)
  (let* ((return (make-hash-table)))
    (when-let* ((b (get-buffer bufname)))
      (kill-buffer b))
    (with-current-buffer (get-buffer-create bufname)
      (erase-buffer)
      (let ((exit-code (call-process-shell-command cmd nil t)))
        (puthash :status exit-code return)
        (puthash :output (buffer-string) return))
      (kill-buffer))
    return))


(defun hget (alist k)
  "Get element from a hash table but in a sane way"
  (gethash k alist))

(defun hput (hm k v)
  "Modifies hast table in place, but also returns it so its easier to thread"
  (puthash  k v hm)
  hm)


(defun lk/fix-utf ()
  (interactive)
  ;; unicode rules everything around me
  (setq locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (define-coding-system-alias 'UTF-8 'utf-8))


;; just exit if terminated or C-x C-c is invoked
(setq confirm-kill-processes nil)

;;(when (display-graphic-p)
;;  (use-package auto-dark :config (auto-dark-mode t)))


(defun lk/kill-dired-buffers ()
  (interactive)
  (mapc
   (lambda (buffer)
     (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
       (kill-buffer buffer)))
   (buffer-list)))

(defun lk/kill-buffers-by-major-mode (mode)
  (interactive "sMajor mode: ")
  "Kill all buffers in the supplied list."
  (mapcar 'kill-buffer
          (seq-filter
           (lambda (buffer)
             (eq (buffer-local-value 'major-mode buffer) mode))
           (buffer-list))))

(defun lk/clean-up-buffers ()
  (interactive)
  (kill-matching-buffers ".*magit.*" 't 't)
  (kill-matching-buffers ".*grep.*" 't 't)
  (kill-matching-buffers ".*XREF.*" 't 't)
  (lk/kill-buffers-by-major-mode 'dired-mode)
  (kill-matching-buffers ".*occur.*" 't 't)
  (kill-matching-buffers ".*Flymake.*" 't 't)
  (kill-matching-buffers ".*<eca-chat:.*" 't 't)
  (kill-matching-buffers ".*<eca:stderr.*" 't 't))

(defvar lk/home-full-path (file-truename (getenv "HOME")))

(defvar lk/project-root-files
  '( "package.json" "deps.edn" "project.clj" "main.tf" "go.mod" "mise.toml"))

(defun lk/project-find-root (path)
  "Searches for project root starting from PATH. Picks the root directory based on:

  1. Version control system root (if any)
  2. Presence of known project root files (e.g., .git, package.json, etc.
  3. fails if the detected root is equal to user's home directory
  Shortest path wins. Returns the project root path or nil if not found."
  (let* ((vc-root (vc-root-dir))          ;; as fallback
         ;; iterate over project files and find their locations:
         (paths (mapcar ; nofmt
                 (lambda (file)
                   (when-let* ((path (locate-dominating-file path file)))
                     (expand-file-name file)))
                 lk/project-root-files))
         ;; now filter out nils and home directory
         (filterd-paths (seq-filter ; nofmt
                         (lambda (p)
                           (and p (not (string= (file-truename p) lk/home-full-path)))) ; nofmt
                         paths)))

    ;; return shortest path if any, fallback to VC root
    (if filterd-paths (car (sort filterd-paths)) vc-root)))

(provide 'lk/utils)
