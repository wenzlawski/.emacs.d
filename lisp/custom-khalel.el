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

;; TODO: The problem here is that the range is computed again and again. It
;; would be much easier to parse this into the appropriate representation
;; upfront.

;; FIXME: How do we handle more complex events such as complex repeating stuff?

(setopt khalel-import-format
	"* {title} {cancelled} :{calendar}:\n\
:PROPERTIES:\n:CALENDAR: {calendar}\n\
:LOCATION: {location}\n\
:ID: {uid}\n\
:END:\n\
- When: <%%(my/khalel-make-diary-block \"{start-date-long}\" \"{end-date-long}\") {start-time}-{end-time}>
- Where: {location}\n\
- Description: {description}\n\
- URL: {url}\n- Organizer: {organizer}\n\n\
[[elisp:(khalel-edit-calendar-event)][Edit this event]]\
    [[elisp:(progn (khalel-run-vdirsyncer) (khalel-import-events))]\
[Sync and update all]]\n")

(defun my/khalel-make-date-list (date-string)
    "Make a time stamp for khalel."
    (let ((time (parse-time-string date-string)))
      (case calendar-date-style
	  (american (list (decoded-time-month time) (decoded-time-day time) (decoded-time-year time)))
	  (european (list (decoded-time-day time) (decoded-time-month time) (decoded-time-year time)))
	  (iso (list (decoded-time-year time) (decoded-time-month time) (decoded-time-day time))))
      ))

(defun my/khalel-make-diary-block (date-start date-end)
  "make a diary block."
  `(diary-block ,@(my/khalel-make-date-list date-start) ,@(my/khalel-make-date-list date-end)))


(provide 'custom-khalel)
;;; custom-khalel.el ends here
