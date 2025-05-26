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

(use-package ejc-sql
  :straight t
  :hook
  (ejc-sql-minor-mode . ejc-eldoc-setup)
  :custom
  (clomacs-httpd-default-port 8091)
  (ejc-result-table-impl 'ejc-result-mode)
  :config
  (add-hook 'ejc-sql-connected-hook
            (lambda ()
              (ejc-set-fetch-size 100)
              (ejc-set-max-rows 100)
              (ejc-set-show-too-many-rows-message t)
              (ejc-set-column-width-limit 25)
              (ejc-set-use-unicode nil))))

(with-eval-after-load 'ejc-sql
  (ejc-create-connection
   "Goldstein 206"
   :dependencies [[com.microsoft.sqlserver/mssql-jdbc "6.2.2.jre8"]]
   :connection-uri (concat "jdbc:sqlserver://VMWSDB01.gold.local:1433;"
                           "databaseName=Goldstein 206;"
                           "user=sa;"
                           "password=sql_admin15;"))

  (ejc-create-connection
   "Lohndialog 200"
   :dependencies [[com.microsoft.sqlserver/mssql-jdbc "6.2.2.jre8"]]
   :connection-uri (concat "jdbc:sqlserver://VMWSDB01.gold.local:1433;"
                           "databaseName=Lohndialog 200;"
                           "user=sa;"
                           "password=sql_admin15;"))
  )
(defun View-scroll-half-page-right (&optional columns)
  (interactive "P")
  (scroll-right (or columns (/ (window-total-width) 2))))

(defun View-scroll-half-page-left (&optional columns)
  (interactive "P")
  (scroll-left (or columns (/ (window-total-width) 2))))

(use-package view
  :bind
  (:map view-mode-map
	("f" . View-scroll-half-page-left)
	("b" . View-scroll-half-page-right)))

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

(bind-key "M-RET" #'indent-new-comment-line 'prog-mode-map)

(defun my/vterm-powershell (&optional arg)
  (interactive "P")
  (let ((vterm-shell (executable-find "powershell.exe")))
    (vterm arg)))

(bind-key "C-c T" #'my/vterm-powershell)

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
      search-web-default-browser 'browse-url-generic
      my/browse-generic-program "wslview -s"
      shell-command-guess-open "wslview"
      trash-directory "~/.trash"
      tab-bar-select-tab-modifiers '(control meta))

(bind-key "C-t" #'avy-goto-word-or-subword-1 'dired-mode-map)

(defun my/embark-open-externally (file)
  "Open FILE or url using system's default application."
  (interactive "sOpen externally: ")
  (unless (string-match-p "\\`[a-z]+://" file)
    pp    (setq file (expand-file-name file)))
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
		   (list (when host (format "-s \"%s\"" (shell-quote-argument host)))
			 (when user (format "-U \"%s\"" (shell-quote-argument user)))
			 (when password (format "-P \"%s\"" (shell-quote-argument password)))
			 (when database (format "-d \"%s\"" database))))
	     " "))


(advice-add 'org-babel-sql-dbstring-mssql :override 'my/org-babel-sql-dbstring-mssql)

(setopt org-directory "~/org/"
	org-refile-targets '((org-agenda-files :level . 8)
			     ("work.org" :level . 2))
	org-agenda-files '("work.org")
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
	  ("s" "steps")
	  ("st" "tips&tricks" entry (id "91b8fed9-4ac8-4608-8959-26d1e6ba5a74")
	   "* %?\n%U\n")

	  ("c" "clock")
	  ("cn" "clock note" entry (clock) "%^{Title}\n%?")
	  ("ct" "clock task" entry (clock) (file ,(dir-concat user-emacs-directory "capture/task.org")) :prepend t)
	  ("cw" "clock web"  entry (clock) "%?%:description\nSource: %:link\n\nTitle: %:description\n\n#+begin_quote\n%i\n#+end_quote" :empty-lines 1))
	khalel-import-org-file (concat org-directory "calendar.org"))


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

(setopt fontaine-presets
	`(
	  (input :default-family "Input Mono"
		 :line-spacing nil)
	  (jetbrains :default-family "JetBrainsMono Nerd Font"
		     :line-spacing 0.02)
	  (source-code-pro :default-family "Source Code Pro"
			   :line-spacing nil)
	  (dejavu-sans-mono :default-family "DejaVuSansM Nerd Font")
	  (fira-code :default-family "Fira Code Nerd Font"
		     :line-spacing nil)
	  (iosevka :default-family "Iosevka Nerd Font")
	  (menlo :default-family "Menlo")
	  (meslo :default-family "MesloLGM Nerd Font")
	  (unifont :default-family "Unifont"
		   :default-height 160
		   :default-weight bold)
	  (go-mono :default-family "GoMono Nerd Font")
	  (present :default-height 350)
	  (arial :default-family "Arial")
	  (libsans :default-family "LiberationSans" :default-height 180)
	  (libserif :default-family "LiberationSerif")
	  (libmono :default-family "LiberationMono")
	  (iaquattro :default-family "iAWriterQuattroS")
	  (regular)
	  (t :default-family "Iosevka Nerd Font"
	     :default-weight regular
	     :default-slant normal
	     :default-height 140
	     :line-spacing 1
	     :fixed-pitch-family nil
	     :fixed-pitch-weight nil
	     :fixed-pitch-slant nil
	     :fixed-pitch-height 1.0
	     :fixed-pitch-serif-family nil
	     :fixed-pitch-serif-weight nil
	     :fixed-pitch-serif-slant nil
	     :fixed-pitch-serif-height 1.0
	     :variable-pitch-family ,(if IS-MAC "iA Writer Quattro V" "LiberationSans")
	     :variable-pitch-weight bold
	     :variable-pitch-slant nil
	     :variable-pitch-height 1.1
	     :mode-line-active-family nil
	     :mode-line-active-weight nil
	     :mode-line-active-slant nil
	     :mode-line-active-height 1.0
	     :mode-line-inactive-family nil
	     :mode-line-inactive-weight nil
	     :mode-line-inactive-slant nil
	     :mode-line-inactive-height 1.0
	     :header-line-family nil
	     :header-line-weight nil
	     :header-line-slant nil
	     :header-line-height 1.0
	     :line-number-family nil
	     :line-number-weight normal
	     :line-number-slant nil
	     :line-number-height 1.0
	     :tab-bar-family nil
	     :tab-bar-weight nil
	     :tab-bar-slant nil
	     :tab-bar-height 1.0
	     :tab-line-family nil
	     :tab-line-weight nil
	     :tab-line-slant nil
	     :tab-line-height 1.0
	     :bold-family nil
	     :bold-slant nil
	     :bold-weight bold
	     :bold-height 1.0
	     :italic-family nil
	     :italic-weight nil
	     :italic-slant italic
	     :italic-height 1.0)
	  ))

(fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

(defun my/remove-carriage-return ()
  (interactive)
  (replace-string-in-region "" "" (point-min) (point-max)))

(bind-key "m" #'my/remove-carriage-return 'mule-keymap)

(shell-command "xset r rate 165 45")

(push (lambda (_) (shell-command "xset r rate 165 45")) after-make-frame-functions)

(require 'steps)

(provide 'work)
;;; work.el ends here

