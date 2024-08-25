;;; CUSTOM-KHALEL --- customising khalel  -*- lexical-binding: t; -*-
;;
;; Author: Marc Wenzlawski <marc.wenzlawski@icloud.com>
;; Copyright © 2024, Marc Wenzlawski, all rights reserved.
;; Created: 26 June 2024
;;
;;; Commentary:
;;
;;  
;;
;;; Code:

(require 'khalel)
(require 's)
(require 'cl-macs)

(eval-when-compile
  (require 'cl-lib))

(defcustom khalel-import-variables
  '(title cancelled calendar description organizer location uid start-date-long end-date-long start-time end-time url)
  "Values imported from khalel")

(defcustom khalel-import-timestamp-function 'my/khalel-import-timestamp-complex
  "Default vaule of the timestamp function.")

(setopt khalel-import-format "* ${title} ${cancelled} :${calendar}:
:PROPERTIES:
:CALENDAR: ${calendar}
:LOCATION: ${location}
:ID: ${uid}
:END:
- When: ${timestamp}
- Where: ${location}
- Description: ${description}
- URL: ${url}
- Organizer: ${organizer}

[[elisp:(khalel-edit-calendar-event)][Edit this event]]    [[elisp:(progn (khalel-run-vdirsyncer) (khalel-import-events))][Sync and update all]]")

(defun khalel-import-json-arguments ()
  "Build the import format string."
  (cl-loop for key in khalel-import-variables
	append (list "--json" (symbol-name key))))

(defun my/khalel-import-timestamp-complex (event)
  "Construct a suitable representation in org timestamp format."
  (let ((start-date (cdr (assoc 'start-date-long event)))
	(start-time (cdr (assoc 'start-time event)))
	(end-date (cdr (assoc 'end-date-long event)))
	(end-time (cdr (assoc 'end-time event))))
    (if (string= start-date end-date)
	(format "<%s %s-%s>" start-date start-time end-time)
      (format "<%s %s>--<%s %s>" start-date start-time end-date end-time))))

(defun my/khalel-import-timestamp-default (event)
  "Default timestamp format function"
  (let ((start-date (cdr (assoc 'start-date-long event)))
	(start-time (cdr (assoc 'start-time event)))
	(end-date (cdr (assoc 'end-date-long event)))
	(end-time (cdr (assoc 'end-time event))))
    (format "<%s %s>--<%s %s>" start-date start-time end-date end-time)))

(defun khalel-import-escape-newline (str)
  (replace-regexp-in-string "\n" "\\n" str nil 'literal))

(defun my/khalel-import-process-event (event)
  "Process an event into an Org subtree."
  (push `(timestamp . ,(funcall khalel-import-timestamp-function event)) event)
  (push `(title . ,(khalel-import-escape-newline (alist-get 'title event))) event)
  (push `(description . ,(khalel-import-escape-newline (alist-get 'description event))) event)
  (s-format khalel-import-format 'aget event))

(defun my/khalel-import-process-json (buf)
  "Process the JSON in buf."
  (with-current-buffer buf
    (goto-char (point-min))
    (cl-loop while (not (eq (point) (1- (point-max))))
             vconcat (json-parse-buffer :object-type 'alist))))

(defun my/khalel-import-clean-duplicates (events)
  "Delete the duplicate events."
  (cl-delete-duplicates events :test (lambda (x y) (equal x y))))


;;;###autoload
(defun my/khalel-import-events ()
  "Imports calendar entries by calling khal externally.

The time delta which determines how far into the future events
are imported is configured through `khalel-import-start-date'
and `khalel-import-end-date'.
CAUTION: The results are imported into the file
`khalel-import-org-file' which is overwritten to avoid adding
duplicate entries already imported previously. As default, the
file is configured to be read-only. This can be adjusted by
configuring `khalel-import-org-file-read-only'.

When a prefix argument is given, the import will be limited to
the calendar `khalel-default-calendar'. If this is nil then the
user is asked to specify a calendar to limit the export to
instead.

Please note that the resulting org file does not necessarily
include all information contained in the .ics files it is based
on. Khal only supports certain (basic) fields when creating lists.

Examples of missing fields are timezone information, categories,
alarms or settings for repeating events."
  (interactive)
  (let*
      ( ;; call khal directly.
       (khal-bin (or khalel-khal-command
                     (executable-find "khal")))
       (khal-cfg (when khalel-khal-config
		   `("-c" ,khalel-khal-config)))
       (khal-cal (when current-prefix-arg
                   (format "-a%s"
                           (khalel--get-calendar))))
       (khal-start (org-read-date nil nil khalel-import-start-date))
       (khal-end (org-read-date nil nil khalel-import-end-date))
       (dst (generate-new-buffer "*khal-output*"))
       (err (get-buffer-create "*khal-errors*"))
       (errfn (make-temp-file "khalel-khal-errors-"))
       ;; determine arguments for khal call
       (args
        (remq nil  ;; remove nil elements
              `(,khal-cfg "list" ,khal-cal
                          ,@(khalel-import-json-arguments)
                          "--day-format" ""
                          ,khal-start ,khal-end)))
       (exitval (apply 'call-process khal-bin nil
                       (list dst errfn) nil
                       args)))
    (save-excursion
      (with-current-buffer err
        (goto-char (point-max))
        (insert-file-contents errfn))
      (with-temp-buffer
	;; events that are scheduled as full-day and span multiple days will
	;; also appear multiple times in the output of `khal list' but will
	;; each keep the entire range in the time stamp. This results in
	;; identical entries in the output file and multiple occurances in the
	;; org-agenda. Filter these duplicates out here.
	(let ((event-strings
	       (cl-map 'vector #'my/khalel-import-process-event
		       (my/khalel-import-clean-duplicates
			(my/khalel-import-process-json dst)))))
	  
	  (khalel--insert-import-file-header khal-start khal-end)

	  (insert (string-join event-strings "\n\n"))

	  (write-file khalel-import-org-file
	              khalel-import-org-file-confirm-overwrite)
	  (message "Imported %d events from khal into %s"
	           (length (org-map-entries nil nil nil))
	           khalel-import-org-file))))
    (if (/= 0 exitval)
	(message "khal exited with non-zero exit code; see buffer `*khal-errors*' for details.")
      (kill-buffer dst)
      )
    ;; revert any buffer visisting the file
    (let ((buf (find-buffer-visiting khalel-import-org-file)))
      (when buf (with-current-buffer buf (revert-buffer :ignore-auto :noconfirm))))
    ))

(provide 'custom-khalel)
;;; custom-khalel.el ends here
