;;; WORK --- Work stuff  -*- lexical-binding: t; -*-
;;
;; Author: Marc Wenzlawski <marcwenzlawski@posteo.com>
;; Copyright © 2024, Marc Wenzlawski, all rights reserved.
;; Created:  5 December 2024
;;
;;; Commentary:
;;
;;  Work special config
;;
;;; Code:

(setopt sql-connection-alist
	'((VMWSDB01 (sql-product 'ms)
		    (sql-server "VMWSDB01")
		    (sql-port 1433)
		    (sql-user "sa")
		    (sql-password "sql_admin15")
		    (sql-database "db_Lohn")
		    ))
	sql-ms-program "sqlcmd"
	sql-ms-options '("-w" "300" "-q" "select '1>'"))

(add-to-list 'process-coding-system-alist '("sqlcmd" . cp850-dos))

;; (use-package ob-sql-mode
;;   :straight t)

;; (use-package vbnet-mode
;;   :straight (:host github :repo "emacsmirror/vbnet-mode")
;;   :mode "\\.vb\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs on WSL open links in Windows web browser
;; https://adam.kruszewski.name/2017/09/emacs-in-wsl-and-opening-links/
(let ((cmd-exe "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe")
      (cmd-args '("-Command")))
  (when (file-exists-p cmd-exe)
    (setq browse-url-generic-program  cmd-exe
	  browse-url-generic-args     cmd-args
	  browse-url-browser-function 'browse-url-generic
	  search-web-default-browser 'browse-url-generic)))

(defun my/embark-open-externally (file)
  "Open FILE or url using system's default application."
  (interactive "sOpen externally: ")
  (unless (string-match-p "\\`[a-z]+://" file)
    (setq file (expand-file-name file)))
  (message "Opening `%s' externally..." file)
  (eval `(call-process ,browse-url-generic-program
		       nil 0 nil ,@browse-url-generic-args ,(format "Start-Process '%s'" file))))

(advice-add 'embark-open-externally :override 'my/embark-open-externally)

(setq shell-file-name "bash"
      org-pretty-entities-include-sub-superscripts nil)

(use-package ahk-mode
  :straight t)

(load-theme 'modus-operandi t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)))

(use-package sqlup-mode
  :straight t)

;; (add-to-list 'apheleia-formatters '(sqlfluff "sqlfluff" "format" "-d" "tsql" "--stdin-filename" filepath))
;; (add-to-list 'apheleia-mode-alist '(sql-mode . sqlfluff))

;; Handle spaces correctly in ob-sql
(defun my/org-babel-sql-dbstring-mssql (host user password database)
  "Make sqlcmd command line args for database connection.
`sqlcmd' is the preferred command line tool to access Microsoft
SQL Server on Windows and Linux platform."
  (mapconcat #'identity
	     (delq nil
		   (list (when host (format "-S \"%s\"" (shell-quote-argument host)))
			 (when user (format "-U \"%s\"" (shell-quote-argument user)))
			 (when password (format "-P \"%s\"" (shell-quote-argument password)))
			 (when database (format "-d \"%s\"" database))))
	     " "))


(advice-add 'org-babel-sql-dbstring-mssql :override 'my/org-babel-sql-dbstring-mssql)

(setq org-directory "~/work/"
      org-capture-templates
      `(("i" "inbox" entry (id "0d0b8397-b242-4f88-983d-90a67fd51eb0")
	 "* %?\n%U\n" :prepend t)
	("p" "project" entry (id "073acc5a-5c96-47c6-a4fb-fc47c1f5fd14")
	 (file ,(dir-concat user-emacs-directory "capture/project.org")) :prepend t)
	("t" "Task" entry (id "073acc5a-5c96-47c6-a4fb-fc47c1f5fd14")
	 (file ,(dir-concat user-emacs-directory "capture/task.org")) :prepend t)
	("e" "email")
	("et" "task" entry (file "~/work/work.org") (file ,(dir-concat user-emacs-directory "capture/mail-task.org")) :prepend t)

	("c" "clock")
	("cn" "clock note" entry (clock) "%^{Title}\n%?")
	("ct" "clock task" entry (clock) (file ,(dir-concat user-emacs-directory "capture/task.org")) :prepend t)
	("cw" "clock web"  entry (clock) "%?%:description\nSource: %:link\n\nTitle: %:description\n\n#+begin_quote\n%i\n#+end_quote" :empty-lines 1)))

(shell-command "xset r rate 170 30")

(with-eval-after-load 'outlook
  (require 'ol-outlook))

(provide 'work)
;;; work.el ends here

