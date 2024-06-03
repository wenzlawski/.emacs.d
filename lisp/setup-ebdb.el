;;; setup-ebdb.el --- Setup ebdb -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package ebdb
  :straight t)

(with-eval-after-load 'ebdb
  (defclass ebdb-field-name-complex (ebdb-field-name)
    ((surname
      :initarg :surname
      :type (or null string)
      :custom (choice (const :tag "No surname" nil)
		      (string :tag "Surname"))
      :initform nil)
     (given-names
      :initarg :given-names
      :type (list-of string)
      :custom (repeat (string :tag "Name"))
      :initform nil)
     (prefix
      :initarg :prefix
      :type (or null string)
      :custom (choice (const :tag "No prefix" nil)
		      (string :tag "Prefix"))
      :initform nil)
     (suffix
      :initarg :suffix
      :type (or null string)
      :custom (choice (const :tag "No suffix" nil)
		      (string :tag "Suffix"))
      :initform nil)
     (actions :initform '(("Lookup notes" . my/ebdb-uuid-lookup))))
    :documentation "A name class for \"complex\", ie structured,
  names."
    :human-readable "alt name")

  (defun my/ebdb-uuid-lookup (record field)
    "Open the notes section in the Contacts file for record, or create one."
    (interactive (list (ebdb-current-record)
		       (ebdb-current-field)))
    (let* ((uuid (ebdb-string (slot-value record 'uuid)))
	   (loc (org-id-find-id-in-file uuid "/Users/mw/Dropbox/denote/20240603T001002--contacts.org")))
      (if loc
	  (progn (find-file "/Users/mw/Dropbox/denote/20240603T001002--contacts.org")
		 (goto-char (cdr loc)))
	(progn (if (y-or-n-p "No note found. Create one?")
		   (progn (find-file "/Users/mw/Dropbox/denote/20240603T001002--contacts.org")
			  (goto-char (point-max))
			  (org-insert-heading nil nil 1)
			  (insert (ebdb-record-name-string record))
			  (org-entry-put (point) "ID" uuid))
		 ))))))

(provide 'setup-ebdb)
;;; setup-ebdb.el ends here
