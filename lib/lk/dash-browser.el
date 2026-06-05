;;; -*- lexical-binding: t; -*-
;;; dash.el --- Browse Dash.app docsets via eww
;;; Commentary:
;;
;; Talks to Dash.app's local HTTP API (Dash must be running with the
;; API server enabled) to list docsets and search documentation,
;; rendering selected entries in eww.
;;
;; Entry point: `M-x lk/dash' opens a transient.
;;
;;; Code:

(require 'url)
(require 'url-util)
(require 'json)
(require 'eww)
(require 'transient)
(require 'subr-x)
(require 'seq)

(defgroup lk/dash nil
  "Browse Dash.app docsets from Emacs."
  :group 'tools)

(defcustom lk/dash-status-file
  (expand-file-name "Library/Application Support/Dash/.dash_api_server/status.json" "~")
  "Path to the Dash local API status file."
  :type 'file
  :group 'lk/dash)

(defcustom lk/dash-request-timeout 10
  "Seconds to wait for a Dash API response."
  :type 'integer
  :group 'lk/dash)

(defvar lk/dash--docsets nil
  "Cached docsets: list of (NAME . IDENTIFIER) pairs.")

(defvar lk/dash--filter nil
  "Active docset identifier filter, a list of identifier strings.
At least one docset is required — Dash's API errors on empty filters.")

(defun lk/dash--port ()
  "Read the Dash API port from the status file."
  (unless (file-exists-p lk/dash-status-file)
    (user-error "Dash status file not found at %s — is Dash running with the API enabled?"
                lk/dash-status-file))
  (with-temp-buffer
    (insert-file-contents lk/dash-status-file)
    (goto-char (point-min))
    (let ((data (json-parse-buffer :object-type 'alist)))
      (or (alist-get 'port data)
          (user-error "No `port' field in Dash status file")))))

(defun lk/dash--url (endpoint &optional query)
  "Build a URL for Dash API ENDPOINT with optional QUERY alist."
  (let ((base (format "http://127.0.0.1:%d%s" (lk/dash--port) endpoint)))
    (if query
        (concat base "?" (url-build-query-string query))
      base)))

(defun lk/dash--get-json (endpoint &optional query)
  "GET ENDPOINT (with optional QUERY) and return parsed JSON as alists."
  (let* ((url (lk/dash--url endpoint query))
         (url-show-status nil)
         (buf (condition-case err
                  (url-retrieve-synchronously url t t lk/dash-request-timeout)
                (error
                 (user-error "Dash request failed: %s" (error-message-string err))))))
    (unless buf
      (user-error "No response from Dash at %s" url))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (unless (re-search-forward "\n\n" nil t)
            (user-error "Malformed HTTP response from Dash"))
          (json-parse-buffer :object-type 'alist
                             :array-type 'list
                             :null-object nil))
      (kill-buffer buf))))

(defun lk/dash--fetch-docsets ()
  "Fetch the docset list from Dash."
  (let* ((resp (lk/dash--get-json "/docsets/list"))
         (docsets (alist-get 'docsets resp)))
    (mapcar (lambda (d)
              (cons (alist-get 'name d)
                    (alist-get 'identifier d)))
            docsets)))

(defun lk/dash--docsets ()
  "Return cached docsets, fetching if needed."
  (or lk/dash--docsets
      (setq lk/dash--docsets (lk/dash--fetch-docsets))))

(defun lk/dash--search (query &optional docset-ids)
  "Search Dash for QUERY, optionally restricted to DOCSET-IDS."
  (let* ((params `(("query" ,query)))
         (params (if (and docset-ids (seq-some #'identity docset-ids))
                     (append params
                             `(("docset_identifiers" ,(string-join docset-ids ","))))
                   params))
         (resp (lk/dash--get-json "/search" params)))
    (alist-get 'results resp)))

(defun lk/dash--filter-names ()
  "Display names of currently filtered docsets."
  (let ((docsets (lk/dash--docsets)))
    (mapcar (lambda (id)
              (or (car (rassoc id docsets)) id))
            lk/dash--filter)))

(defun lk/dash--filter-description ()
  "Human-readable description of the active filter."
  (if lk/dash--filter
      (format "Docsets: %s" (string-join (lk/dash--filter-names) ", "))
    "Docsets: (none — required)"))

(defun lk/dash--pick-result (results)
  "Prompt for one of RESULTS via `completing-read'."
  (when (null results)
    (user-error "No results"))
  (let ((table (make-hash-table :test 'equal))
        cands)
    (dolist (r results)
      (let* ((base (format "%s  [%s]"
                           (alist-get 'name r)
                           (alist-get 'docset r)))
             (key base)
             (n 1))
        (while (gethash key table)
          (setq n (1+ n))
          (setq key (format "%s (%d)" base n)))
        (puthash key r table)
        (push key cands)))
    (let* ((completion-extra-properties
            (list :annotation-function
                  (lambda (cand)
                    (when-let* ((r (gethash cand table))
                                (desc (alist-get 'description r)))
                      (concat "  " (propertize desc 'face 'completions-annotations))))))
           (chosen (completing-read "Result: " (nreverse cands) nil t)))
      (gethash chosen table))))

;;;###autoload
(defun lk/dash-refresh-docsets ()
  "Re-fetch the docset list from Dash."
  (interactive)
  (setq lk/dash--docsets nil)
  (message "Loaded %d Dash docsets" (length (lk/dash--docsets))))

;;;###autoload
(defun lk/dash-select-docsets ()
  "Pick one or more docsets to limit subsequent searches to."
  (interactive)
  (let* ((docsets (lk/dash--docsets))
         (chosen (completing-read-multiple
                  "Docsets (at least one): "
                  (mapcar #'car docsets) nil t)))
    (when (null chosen)
      (user-error "At least one docset is required"))
    (setq lk/dash--filter
          (delq nil
                (mapcar (lambda (name) (cdr (assoc name docsets))) chosen)))
    (message "%s" (lk/dash--filter-description))))

;;;###autoload
(defun lk/dash-clear-docsets ()
  "Clear the docset filter. The next search will re-prompt."
  (interactive)
  (setq lk/dash--filter nil)
  (message "Dash filter cleared"))

;;;###autoload
(defun lk/dash-search (query)
  "Search Dash for QUERY using the active docset filter, open in eww.
Prompts for docsets first if none are selected."
  (interactive
   (progn
     (unless lk/dash--filter (lk/dash-select-docsets))
     (list (read-string
            (format "Search Dash (%s): " (lk/dash--filter-description))))))
  (when (string-blank-p query)
    (user-error "Empty query"))
  (let* ((results (lk/dash--search query lk/dash--filter))
         (chosen (lk/dash--pick-result results)))
    (eww (alist-get 'load_url chosen))))

;;;###autoload (autoload 'lk/dash "lk/dash" nil t)
(transient-define-prefix lk/dash ()
  "Browse Dash.app documentation."
  [:description
   (lambda () (lk/dash--filter-description))
   ("s" "Search"          lk/dash-search)
   ("d" "Select docsets"  lk/dash-select-docsets :transient t)
   ("c" "Clear docsets"   lk/dash-clear-docsets  :transient t)
   ("r" "Refresh docsets" lk/dash-refresh-docsets :transient t)
   ("q" "Quit"            transient-quit-one)])

(provide 'lk/dash-browser)
;;; dash.el ends here
