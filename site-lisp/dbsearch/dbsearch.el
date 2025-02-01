;;; DBSEARCH ---   -*- lexical-binding: t; -*-
;;
;; Author: Marc Wenzlawski <marcwenzlawski@posteo.com>
;; Copyright © 2025, Marc Wenzlawski, all rights reserved.
;; Created: 17 January 2025
;;
;;; Commentary:
;;
;;  Searching SQL databases for all their objects.
;;  For now only for SSMS.
;;  It will only store the function name, details on the object
;;  , its type, schema, source db.
;;  The functions come from the SQL files
;;  Currently objects and tables are in there, still need things like jobs, etc.
;;  See ChatGPT on where the other object types are.
;;
;;  Features:
;;  - Consult async filtering
;;  - Static filtering a la rg, or with a tablist
;;  - configuring the search with transient a la gptel
;;    one off searches, and global settings
;;  - search profiles, saved searches
;;  - multiple buffers, multiple searches
;;
;;  MAYBE IN THE FUTURE: unified search also for STEPS docs and fields. Can also get them from the help fields.
;;  So that Auftrag > Field name  can lead directly to the table column name.
;;  And that db Table > column name to object and UI field name

;;
;;; Code:

(require 'consult)
(require 'sqlite)
(require 'transient)

(with-current-buffer (get-buffer-create "*sqlcmd*")
  (call-process "sqlcmd" nil t nil "-S" "VMWSDB01" "-U" "sa" "-P" "sql_admin15" "-d" "Goldstein 206" "-i" "query.sql")
  (goto-char (point-min))
  ;; (search-forward "[")
  ;; (json-read)
  )





(provide 'dbsearch)
;;; dbsearch.el ends here
