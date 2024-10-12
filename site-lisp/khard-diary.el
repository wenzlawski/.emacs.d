;;; KHARD-DIARY ---   -*- lexical-binding: t; -*-
;;
;; Author: Marc Wenzlawski <marcwenzlawski@posteo.com>
;; Copyright © 2024, Marc Wenzlawski, all rights reserved.
;; Created: 12 October 2024
;;
;;; Commentary:
;;
;;  
;;
;;; Code:

(require 's)
(require 'khardel)

(eval-when-compile
  (require 'cl-lib))

(defvar my/diary-birthday-file (dir-concat user-emacs-directory "birthdays")
  "File for birthdays.")

(defun khard-diary-make-file ()
  "Make the birthday diary file."
  (interactive)
  (let ((year (decoded-time-year (decode-time (current-time))))
	dates names)
    (with-temp-buffer
      (call-process khardel-command nil t nil "birthdays" "-p")
      (goto-char (point-min))
      (while (not (eobp))
	(let* ((line (buffer-substring-no-properties
		      (line-beginning-position)
		      (line-end-position)))
	       (parts (s-split "\t" line)))
	  (push (car parts) dates)
	  (push (cadr parts) names)
	  (forward-line 1))))
    (with-temp-buffer
      (let ((formatted (s-split "\n" (shell-command-to-string (concat "strptime -i %Y.%m.%d " (s-join " " dates)))))
	    bday
	    age
	    )
	(cl-loop for date in formatted
		 for name in names
		 do
		 (progn
		   (setq bday (parse-time-string date))
		   (setq age (- year (decoded-time-year bday)))
		   (insert (format "%d/%d " (decoded-time-month bday) (decoded-time-day bday)) name "'s ")
		   (insert (int-to-string age)
			   (pcase (cl-rem age 10)
			     (1 "st")
			     (2 "nd")
			     (3 "rd")
			     (_ "th"))
			   " Birthday\n"))))
      (write-file my/diary-birthday-file nil))))


(provide 'khard-diary)
;;; khard-diary.el ends here
