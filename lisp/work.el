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
	'(("VMWSDB01" (sql-product 'ms)
	   (sql-server "VMWSDB01")
	   (sql-port 1433)
	   (sql-user "sa")
	   (sql-password "sql_admin15")
	   (sql-database "db_Lohn")
	   ))
	sql-ms-program "sqlcmd"
	sql-ms-options '("-w" "300" "-q" "select '1>'"))

(add-to-list 'process-coding-system-alist '("sqlcmd" . cp850-dos))

;; (bind-key "M-j" #'indent-new-comment-line 'prog-mode-map)

(add-hook 'before-save-hook #'time-stamp)
(use-package time-stamp
  :custom
  (time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S %L"))

(use-package powershell
  :straight t)

(use-package vbnet-mode
  :straight (:host github :repo "emacsmirror/vbnet-mode")
  :mode "\\.vbs?\\'"
  :bind
  (:map vbnet-mode-map
	("TAB" . indent-for-tab-command))
  :custom
  (vbnet-want-flymake-fixup nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs on WSL open links in Windows web browser
;; https://adam.kruszewski.name/2017/09/emacs-in-wsl-and-opening-links/
(setq browse-url-generic-program  "wslview"
      browse-url-generic-args     '()
      browse-url-browser-function 'browse-url-generic
      search-web-default-browser 'browse-url-generic)

(defun my/embark-open-externally (file)
  "Open FILE or url using system's default application."
  (interactive "sOpen externally: ")
  (unless (string-match-p "\\`[a-z]+://" file)
    (setq file (expand-file-name file)))
  (message "Opening `%s' externally..." file)
  (eval `(call-process ,browse-url-generic-program
		       nil 0 nil ,@browse-url-generic-args ,file)))

(advice-add 'embark-open-externally :override 'my/embark-open-externally)

(setq shell-file-name "bash"
      org-pretty-entities-include-sub-superscripts nil)

(use-package ahk-mode
  :straight t
  :config
  (require 'custom-ahk-mode))

(load-theme 'modus-operandi t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)))

(use-package sqlup-mode
  :straight t
  :hook (sql-mode . sqlup-mode))

;; (add-to-list 'apheleia-formatters '(sqlfluff "sqlfluff" "format" "-d" "tsql" inplace))
(add-to-list 'apheleia-formatters '(sqlformatter "sql-formatter" "-l" "tsql"))
(add-to-list 'apheleia-mode-alist '(sqlformatter . sqlfluff))

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

(setopt org-directory "~/org/"
	org-capture-templates
	`(("i" "inbox" entry (id "0d0b8397-b242-4f88-983d-90a67fd51eb0")
	   "* %?\n%U\n" :prepend t)
	  ("p" "project" entry (id "073acc5a-5c96-47c6-a4fb-fc47c1f5fd14")
	   (file ,(dir-concat user-emacs-directory "capture/project.org")) :prepend t)
	  ("t" "Task" entry (id "073acc5a-5c96-47c6-a4fb-fc47c1f5fd14")
	   (file ,(dir-concat user-emacs-directory "capture/task.org")) :prepend t)
	  ("m" "Meeting" entry (id "0d0b8397-b242-4f88-983d-90a67fd51eb0")
	   "* %? :meeting:\nSCHEDULED: %^t\n\n")
	  ("e" "email")
	  ("et" "task" entry (file "~/work/work.org") (file ,(dir-concat user-emacs-directory "capture/mail-task.org")) :prepend t)

	  ("c" "clock")
	  ("cn" "clock note" entry (clock) "%^{Title}\n%?")
	  ("ct" "clock task" entry (clock) (file ,(dir-concat user-emacs-directory "capture/task.org")) :prepend t)
	  ("cw" "clock web"  entry (clock) "%?%:description\nSource: %:link\n\nTitle: %:description\n\n#+begin_quote\n%i\n#+end_quote" :empty-lines 1))
	khalel-import-org-file (concat org-directory "calendar.org"))

(shell-command "xset r rate 165 45")

(with-eval-after-load 'org
  (require 'ol-outlook))

(with-eval-after-load 'khalel
  (khalel-import-events))

;; TODO Make a global minor mode that I can enable when working on the console within
;; an emacsclient. It temporarily enables all the changes for the terminal, and
;; reverts when disabled
(when (eq window-system nil)
  (with-eval-after-load 'org
    (bind-key "<C-i>" #'org-cycle 'org-mode-map)
    (bind-key "C-j" #'org-insert-heading-respect-content 'org-mode-map))
  (bind-key "M-'" #'embark-act)
  (bind-key "<C-i>" #'indent-for-tab-command))

;; NOTMUCH

(with-eval-after-load 'notmuch
  (setopt notmuch-identities '("Marc Wenzlawski <m.wenzlawski@goldstein.de")
	  notmuch-fcc-dirs '(("m.wenzlawski@goldstein.de" . "go/Sent -inbox +sent -unread +go"))))

(use-package org-re-reveal
  :straight t
  :custom
  (org-reveal-root "./reveal.js/"))

(use-package org-present
  :straight t
  :config
  (progn
    (add-hook 'org-present-mode-hook
              (lambda ()
                (org-present-big)
                (org-display-inline-images)
                (org-present-hide-cursor)
                (org-present-read-only)))
    (add-hook 'org-present-mode-quit-hook
              (lambda ()
                (org-present-small)
                (org-remove-inline-images)
                (org-present-show-cursor)
                (org-present-read-write)))))

(use-package simple-httpd
  :straight t)

(require 'steps)

(provide 'work)
;;; work.el ends here

