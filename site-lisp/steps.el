;;; STEPS -- STPS -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 's)
(require 'consult)
(require 'vbnet-mode)

(defcustom steps-db-connection '((instance . "VMWSDB01")
				 (database . "Lohndialog 200")
				 (user . "sa")
				 (password . "sql_admin15"))
  "Db connection settings." :group 'steps :type '(sexp))

(defcustom steps-db-db nil
  "DB override to use." :group 'steps :type '(string))

(defcustom steps-db-dbs '("Goldstein 206" "Lohndialog 200")
  "Steps Databases" :group 'steps :type '(sexp))

(defcustom steps-asj-number nil
  "Set the ASJ" :group 'step :type '(string))

(defvar steps--sql-cmd "sqlcmd")

(defvar steps--sql-cmd-args '("-W"
			      "-h" "-1"
			      "-m" "-1"))

(defun steps-sql-command (command &optional instance database user password)
  "Build the initial query"
  (unless instance
    (setq instance (alist-get 'instance steps-db-connection)))
  (unless database
    (setq database (cond (steps-db-db)
			 ((alist-get 'database steps-db-connection)))))
  (unless user
    (setq user (alist-get 'user steps-db-connection)))
  (unless password
    (setq password (alist-get 'password steps-db-connection)))
  (format "%s %s -S '%s' -d '%s' -U '%s' -P '%s' -Q \"SET NOCOUNT ON; %s\""
	  steps--sql-cmd
	  (string-join steps--sql-cmd-args " ")
	  instance
	  database
	  user
	  password
	  command))

(defun steps-sql-command-to-file (command file)
  (format "%s -o %s -y 0" (steps-sql-command command) file))

(defun steps-sql-execute (command)
  (string-trim
   (shell-command-to-string command)))

(defun steps-asj-sql-command (code asj)
  "Get the sql string to write the ASJ code to the DB."
  (format "UPDATE TOP (1) sao.ASJOB SET LS_CODE = OPENJSON(N'%s') WHERE S_ASJOBNO = '%s' AND DT_DELETED IS NULL;" code asj)
  ;; (format "UPDATE TOP (1) sao.ASJOB SET LS_CODE = CONVERT(VARCHAR(MAX), CAST('' AS XML).value('xs:base64Binary(\\\"%s\\\")', 'VARBINARY(MAX)')) WHERE S_ASJOBNO = '%s' AND DT_DELETED IS NULL;" code asj)
  )

(defun steps-asj-file-to-asj ()
  "Save the buffer content to the ASJ."
  (interactive)
  (save-buffer)
  (let* ((bufstring (buffer-substring-no-properties (point-min) (point-max)))
	 (base bufstring) ;; FIXME Base64 does not work with multibyte. Needs a module for larges comms.
	 (asj (print (cond (steps-asj-number)
			   (car (s-split "_" (file-name-base (buffer-file-name)))))))
	 (sqlcmd (steps-asj-sql-command base asj))
	 (sqlcli (steps-sql-command sqlcmd))
	 (res (print (shell-command-to-string sqlcli))))
    (if (equal "(1 row affected)\n(1 row affected)\n" res)
	(message "Saved to DB.")
      (message (concat "Error saving. " res)))))

(defun steps-asj-get-job-names ()
  "Get a list of job name and description"
  (mapcar (lambda (s) (s-join " @" (split-string s "\t")))
	  (split-string
	   (steps-sql-execute
	    (steps-sql-command "SELECT S_DESCRIPTION + '\t' + S_ASJOBNO FROM sao.ASJOB WHERE DT_DELETED IS NULL;"))
	   "\n")))

(defun steps-asj--to-tmp-file (asj)
  (let ((tmp (make-temp-file asj)))
    (steps-sql-execute
     (steps-sql-command-to-file
      (format "SELECT TOP 1 LS_CODE FROM sao.ASJOB WHERE S_ASJOBNO = '%s' AND DT_DELETED IS NULL" asj)
      tmp))
    tmp))

(defun steps-asj-consult-read-job ()
  "Get an ASJ from the DB."
  (interactive)
  (let* ((asj (split-string
	       (consult--read (steps-asj-get-job-names)
			      :require-match t
			      :sort nil
			      :prompt "Job: ") " @" ))
	 (num (cadr asj))
	 (desc (car asj)))
    `(num ,num desc ,desc)))

(defun steps-asj-open-job-from-db ()
  (interactive)
  (let* ((db (completing-read "Database: " steps-db-dbs))
	 (steps-db-db db))
    (steps-asj-open-job)))

(defun steps-asj-open-job ()
  "Put an ASJ into a buffer."
  (interactive)
  (let* ((asj (steps-asj-consult-read-job))
	 (tmp (steps-asj--to-tmp-file (plist-get asj 'num)))
	 (folder (if (equal (alist-get 'database steps-db-connection) "Goldstein 206")
		     "go"
		   "ld"))
	 (wd (format "~/work/asj/%s" folder))
	 (bufname (format "%s_%s.vb"
			  (s-replace " " "_" (plist-get asj 'num))
			  (s-replace " " "_" (plist-get asj 'desc))))
	 (fn (dir-concat wd bufname)))
    (find-file tmp)
    (goto-char (point-min))
    (save-excursion
      (while (search-forward "" nil t)
	(replace-match "")))
    (insert "' -*- steps-asj-number: \"" (plist-get asj 'num) "\" -*-\n\n")
    (set-visited-file-name fn)
    (vbnet-mode)))

(define-minor-mode steps-asj-autosave-mode
  "Automatically saving the ASJ to the DB when editing"
  :group 'steps)

(provide 'steps)
;;; steps.el ends here
