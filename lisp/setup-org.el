;;; setup-org --- Org Configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'straight)
(require 'noflet)

;; * ORG

(defun my/org-fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(use-package org
  :straight t
  :hook
  ;;(org-mode . auto-fill-mode)
  (org-mode . visual-line-mode)
  ;;(org-mode . variable-pitch-mode)
  (org-mode . (lambda nil (setq cursor-type 'bar)))
  (org-capture-mode . (lambda nil (setq-local
				   header-line-format
				   (substitute-command-keys
				    "\\<org-capture-mode-map>Capture buffer.  Finish \
`\\[my/org-capture-finalize]', refile `\\[org-capture-refile]', \
abort `\\[org-capture-kill]'."))))
  (org-babel-after-execute . my/org-fix-inline-images)
  :bind
  ("C-c a" . org-agenda)
  ("C-c l" . org-store-link)
  ("C-x c" . org-capture)
  (:map org-mode-map
	("C-'" . nil)
	("<C-i>" . backward-kill-word)
	("M-i" . org-delete-backward-char)
	("C-c <C-i>" . org-cycle)
	("C-c C-v <C-i>" . org-toggle-inline-images)
	("C-c C-x <DEL>" . org-cut-subtree)
	("C-c C-x C-<backspace>" . org-cut-subtree)
	("C-c C-x h" . org-edit-headline)
	("C-c C-x i" . org-indent-mode)
	("C-c C-x t" . (lambda () (interactive) (setopt visual-fill-column-center-text (not visual-fill-column-center-text)) (visual-fill-column-mode 1)))
        ("C-c 4 C-o" . my/org-open-at-point-other-window)
        ("C-c 4 o" . my/org-open-at-point-other-window)
        ("C-c 5 C-o" . my/org-open-at-point-other-frame)
        ("C-c 5 o" . my/org-open-at-point-other-frame)
        ("C-c C-." . org-time-stamp-inactive)
        ("C-c e" . org-emphasize))
  :custom-face
  (org-agenda-date ((t (:height 1.2))))
  (org-agenda-structure ((t (:height 1.2))))
  (org-archived ((t (:background unspecified :foreground "grey"))))
  (org-block ((t (:inherit fixed-pitch))))
  (org-block-begin-line ((t (:inherit fixed-pitch))))
  (org-column-title ((t (:inherit fixed-pitch))))
  (org-document-info ((t (:inherit fixed-pitch))))
  (org-document-info-keyword ((t (:inherit fixed-pitch))))
  (org-document-title ((t (:height 1.7))))
  (org-drawer ((t (:inherit fixed-pitch))))
  (org-meta-line ((t (:inherit fixed-pitch))))
  (org-property-value ((t (:inherit fixed-pitch))))
  (org-special-keyword ((t (:inherit fixed-pitch))))
  (org-table ((t (:inherit fixed-pitch))))
  ;; :custom
  ;; (display-buffer-alist
  ;;  (append display-buffer-alist
  ;;       '(("^\\(CAPTURE-.+\\)$\\|\\*\\(?:Capture\\|Org Select\\)\\*"
  ;;          (display-buffer-below-selected display-buffer-at-bottom)
  ;;          (inhibit-same-window . t)
  ;;          (window-height . )))))
  :custom
  (org-directory "~/personal/org/")
  (org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE")))
  (org-hide-emphasis-markers t)
  (org-latex-compiler "xelatex")
  (org-refile-targets '((nil :level . 8)
			(org-agenda-files :level . 8)
			("personal.org" :maxlevel . 3)
			("refile.org" :level . 0)
			("resources.org" :level . 1)))
  (org-refile-use-outline-path 'file)
  (org-ellipsis " ↴")
  (org-src-preserve-indentation t)
  (org-edit-src-content-indentation 0)
  (org-pretty-entities t)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-fast-tag-selection-single-key t)
  (org-special-ctrl-a/e t)
  (org-outline-path-complete-in-steps nil)
  (org-goto-max-level 5)
  ;;(setq org-highlight-latex-and-related '(latex script entities))
  (org-highlight-latex-and-related nil)
  (org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))
  (org-src-window-setup 'current-window)
  (org-tags-column 0)
  (org-agenda-tags-column 0)
  (org-export-allow-bind-keywords t)
  (org-auto-align-tags nil)
  (org-tags-exclude-from-inheritance '("ARCHIVE"))
  (org-global-properties '(("Effort_ALL" . "0:15 0:30 1:00 2:00 4:00 8:00")))
  (org-log-into-drawer t)
  (org-habit-graph-column 55)
  (org-attach-use-inheritance t)
  (org-attach-directory ".data/")
  (org-attach-id-dir ".data/")
  (org-footnote-auto-adjust t)
  (org-clock-persist 'history)
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid '((daily today require-timed)
			  (800 1000 1200 1400 1600 1800 2000)
			  " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")
  (org-file-apps '((auto-mode . emacs)
		   (directory . emacs)
		   ("\\.mm\\'" . default)
		   ("\\.x?html?\\'" . default)
		   ("\\.pdf\\'" . emacs)))
  :config
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  (org-clock-persistence-insinuate)
  (add-to-list 'org-babel-load-languages '(shell . t))

  ;; ** org-agenda

  (setq org-agenda-files '("daily.org" "personal.org" "phone.org" "read.org" "calendar.org"))
  ;; after https://emacs.stackexchange.com/questions/75822/ignoring-non-existent-org-mode-agenda-files
  (setq org-agenda-skip-unavailable-files t)
  (setq org-agenda-custom-commands 	; a,e,t,m,s,T,M,S,C
	'(("p"  . "project+state search")
	  ("pp" tags "+project")
	  ("pa" tags "+project-maybe")
	  ("pm" tags "+project+TODO=\"MAYBE\"")
	  ("o" . "todo related")
	  ("os" tags "TODO=\"TODO\"" ((org-agenda-skip-function
				       '(org-agenda-skip-entry-if 'timestamp))))
	  ("ot" todo "TODO" nil)
	  ("op" todo "NEXT" nil)
	  ("om" todo "MAYBE" nil)
	  ("oi" todo "INTR" nil)
	  ("n" "Agenda / INTR / NEXT"
	   ((agenda "" nil)
	    (todo "INTR" nil)
	    (todo "NEXT" nil))
	   nil)
	  ("N" "INTR / PROG / NEXT"
	   ((todo "INTR" nil)
	    (todo "NEXT" nil))
	   nil)))
  (setq org-agenda-include-diary t)

  ;; ** org-capture
  (defun my/read-later-template (url)
    "capture template for read later"
    (let* ((article (my/read-it-later-attach url))
	   (name (nth 0 article))
	   (file (nth 1 article))
	   (dir (nth 2 article))
	   (url (nth 3 article))
	   (effort (org-minutes-to-clocksum-string
		    ( / (string-to-number
			 (string-trim (shell-command-to-string
				       (concat "wc -w < '" (expand-file-name file dir)"'")))) 100))))
      (concat "* TODO " name "\n:PROPERTIES:\n:URL: " url "\n:Effort: " effort "\n:END:\n%U\nAvailable at: [[attachment:" file "][" name "]]\n%?")))


  (defun my/read-later-template-from-kill ()
    (require 'org-web-tools)
    (my/read-later-template (org-web-tools--get-first-url)))

  (defun my/read-later-template-from-prompt ()
    (my/read-later-template (read-string "URL: ")))

  (defun my/url-librewolf-capture-to-org ()
    "Call `org-capture-string' on the current front most Safari window.
Use `org-mac-link-safari-get-frontmost-url' to capture url from Safari.
Triggered by a custom macOS Quick Action with a keyboard shortcut."
    (interactive)
    (org-capture-string (my/org-mac-link-librewolf-get-frontmost-url) "lz")
    (ignore-errors)
    (org-capture-finalize)
    nil)

  (setq org-capture-templates
	`(("r" "refile" entry (file "refile.org")
	   "* %^{Title}\n%U\n\n%i%?" :prepend t :empty-lines-after 1)

	  ("p" "project" entry (id "316F33BA-71DE-41B9-B21B-928D3778A097")
	   (file ,(dir-concat user-emacs-directory "capture/project.org")) :prepend t)

	  ("e" "email")
	  ("et" "task" entry (file "~/work/work.org") (file ,(dir-concat user-emacs-directory "capture/mail-task.org")) :prepend t)

	  ("j" "job")
	  ("jt" "job task" entry (file "~/work/work.org") (file ,(dir-concat user-emacs-directory "capture/task.org")) :prepend t)

	  ("c" "clock")
	  ("cn" "clock note" entry (clock) "%^{Title}\n%?")
	  ("ct" "clock task" entry (clock) (file ,(dir-concat user-emacs-directory "capture/task.org")) :prepend t)
	  ("cw" "clock web"  entry (clock) "%?%:description\nSource: %:link\n\nTitle: %:description\n\n#+begin_quote\n%i\n#+end_quote" :empty-lines 1)

	  ("t" "Task" entry (id "6FA6128F-4291-4508-8EB8-8951D736D81C")
	   (file ,(dir-concat user-emacs-directory "capture/task.org")) :prepend t)
	  ("h" "Habit" entry (id "7F689015-46F8-4BD8-9B09-164AA168A16A")
	   (file ,(dir-concat user-emacs-directory "capture/habit.org")) :prepend t)

	  ("l" "later")
	  ("lp" "Read later prompt" entry (id "F86FBB48-767F-436D-926E-D118F57AE534")
	   (function my/read-later-template-from-prompt))
	  ("lk" "Read later kill" entry (id "F86FBB48-767F-436D-926E-D118F57AE534")
	   (function my/read-later-template-from-kill))
	  ("ll" "Read later librewolf" entry (file "refile.org")
	   "* %(my/org-mac-link-librewolf-get-frontmost-url) :link:\n%U" :immediate-finish t :prepend t)
	  ("lL" "Read later librewolf edit" entry (file "refile.org")
	   "* %(my/org-mac-link-librewolf-get-frontmost-url) :link:\n%U\n%?" :prepend t)
	  ("lb" "Bookmark" entry (id "23CDFA52-0EE4-4DAA-8B8B-E2D105E6293E") "*** [[%:link][%:description]] %^g\nSCHEDULED: %T%?")

	  ("w" "Web template" entry (file "refile.org")
           "* %?%:description\nSource: %:link\n\nTitle: %:description\n\n#+begin_quote\n%i\n#+end_quote" :empty-lines 1 :prepend t)

	  ("a" "Application" entry (file "~/personal/job-apps/applications.org::* Applications")
	   (file ,(dir-concat user-emacs-directory "capture/application.org")) :prepend t)
	  ))

  ;; ** org-capture frame

  (defun my/make-capture-frame ()
    "Create a new frame and run `org-capture'."
    (interactive)
    (make-frame '((name . "capture")
                  (top . 300)
                  (left . 700)
                  (width . 80)
                  (height . 25)))
    (select-frame-by-name "capture")
    (my/frame-recenter)
    (delete-other-windows)
    (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
      (condition-case ex
	  (org-capture)
	('error
	 ;;(message "org-capture: %s" (error-message-string ex))
	 (delete-frame)))))

  (defun my/close-if-capture (&optional a)
    (if (equal "capture" (frame-parameter nil 'name))
	(progn
	  (delete-frame)
	  ;; (if (eq 'darwin system-type)
	  ;; 	      (ns-do-applescript "tell application \"System Events\"
	  ;;     tell process \"finder\"
	  ;;         activate
	  ;;         keystroke tab using {command down}
	  ;;     end tell
	  ;; end tell"))
	  )))

  (with-eval-after-load 'org-capture
    (defun my/org-capture-finalize (&optional stay-with-capture)
      (interactive "P")
      (org-capture-finalize stay-with-capture))

    (bind-key "C-c C-c" #'my/org-capture-finalize 'org-capture-mode-map))

  (advice-add #'my/org-capture-finalize :after #'my/close-if-capture)

  ;;(advice-add #'org-capture-finalize :after #'my/close-if-capture)
  (advice-add #'org-capture-refile :after #'my/close-if-capture)
  (advice-add #'org-capture-kill :after #'my/close-if-capture)
  (advice-add #'my/read-later-template-from-prompt :after #'my/close-if-capture)
  (advice-add #'org-protocol-capture :before
	      (lambda (_) (progn
			    (make-frame '((name . "capture")
					  (top . 300)
					  (left . 700)
					  (width . 80)
					  (height . 25)))
			    (select-frame-by-name "capture")
			    (my/frame-recenter))))
  (advice-add #'org-protocol-capture :after (lambda (_) (delete-other-windows)))
  (advice-add  #'org-capture-place-template :after (lambda (_) (if (equal "capture" (frame-parameter nil 'name))
								   (delete-other-windows))))

  ;; ** org align tags

  (defun my-org--align-tags-here (to-col)
    "Align tags on the current headline to TO-COL.
Since TO-COL is derived from `org-tags-column', a negative value is
interpreted as alignment flush-right, a positive value as flush-left,
and 0 means insert a single space in between the headline and the tags."
    ;; source: https://list.orgmode.org/[email protected]/
    (save-excursion
      (when (org-match-line org-tag-line-re)
	(let* ((tags-start (match-beginning 1))
	       (tags-end (match-end 1))
	       (tags-pixel-width
		(car (window-text-pixel-size (selected-window)
					     tags-start tags-end)))
	       (blank-start (progn
			      (goto-char tags-start)
			      (skip-chars-backward " \t")
			      (point)))
	       ;; use this to avoid a 0-width space before tags on long lines:
	       (blank-start-col (progn
				  (goto-char blank-start)
				  (current-column)))
	       ;; this is to makes it work with org-indent-mode:
	       (lpref (if (org-fold-folded-p) 0
			(length (get-text-property (point) 'line-prefix)))))
	  ;; If there is more than one space between the headline and
	  ;; tags, delete the extra spaces.  Might be better to make the
	  ;; delete region one space smaller rather than inserting a new
	  ;; space?
	  (when (> tags-start (1+  blank-start))
	    (delete-region blank-start tags-start)
	    (goto-char blank-start)
	    (insert " "))
	  (if (or (= to-col 0) (< (abs to-col) (1- blank-start-col)))
	      ;; Just leave one normal space width
	      (remove-text-properties blank-start (1+  blank-start)
				      '(my-display nil))
	    (message "In here: %s" lpref)
	    (let ((align-expr
		   (if (> to-col 0)
		       ;; Left-align positive values
		       (+ to-col lpref)
		     ;; Right-align negative values by subtracting the
		     ;; width of the tags.  Conveniently, the pixel
		     ;; specification allows us to mix units,
		     ;; subtracting a pixel width from a column number.
		     `(-  ,(- lpref to-col) (,tags-pixel-width)))))
	      (put-text-property blank-start (1+  blank-start)
				 'my-display
				 `(space . (:align-to ,align-expr)))))))))

  (defun my-fix-tag-alignment ()
    (setq org-tags-column 70) ;; adjust this
    (advice-add 'org--align-tags-here :override #'my-org--align-tags-here)
    ;; this is needed to make it work with https://github.com/minad/org-modern:
    (add-to-list 'char-property-alias-alist '(display my-display))
    ;; this is needed to align tags upon opening an org file:
    (org-align-tags t))

  ;;(add-hook 'org-mode-hook #'my-fix-tag-alignment)


  ;; ** org read it later

  (defun my/org-web-tools--url-as-readable-org-file (&optional url)
    "Return string containing Org entry of URL's web page content.
Content is processed with `eww-readable' and Pandoc.  Entry will
be a top-level heading, with article contents below a
second-level \"Article\" heading, and a timestamp in the
first-level entry for writing comments."
    ;; By taking an optional URL, and getting it from the clipboard if
    ;; none is given, this becomes suitable for use in an org-capture
    ;; template, like:

    ;; ("wr" "Capture Web site with eww-readable" entry
    ;;  (file "~/org/articles.org")
    ;;  "%(org-web-tools--url-as-readable-org)")
    (-let* ((url (or url (org-web-tools--get-first-url)))
            (dom (plz 'get url :as #'org-web-tools--sanitized-dom))
            ((title . readable) (org-web-tools--eww-readable dom))
            (title (org-web-tools--cleanup-title (or title "")))
            (converted (org-web-tools--html-to-org-with-pandoc readable))
            (link (org-link-make-string url title))
            (timestamp (format-time-string (org-time-stamp-format 'with-time 'inactive))))
      (with-temp-buffer
	(org-mode)
	;; Insert article text
	(insert converted)
	;; Demote in-article headings
	;; MAYBE: Use `org-paste-subtree' instead of demoting headings ourselves.
	;;(org-web-tools--demote-headings-below 1)
	;; Insert headings at top
	(goto-char (point-min))
	(insert "#+title: " title "\n"
		"#+date: " timestamp "\n\n"
		link "\n\n")
	(buffer-string))))

  (defun my/read-it-later-attach (url)
    "Attach the URL as readable Org to the entry at point."
    (interactive "sURL: ")
    (save-window-excursion
      (org-id-open "F86FBB48-767F-436D-926E-D118F57AE534" nil)
      (let ((attach-dir (org-attach-dir)))
	(with-temp-buffer
	  (message "Getting Article...")
	  (insert (my/org-web-tools--url-as-readable-org-file url))
	  (goto-char (point-min))
	  (let ((name (cadar (org-collect-keywords '("TITLE")))))
	    (message (org-get-title))
	    (write-file (expand-file-name
			 (concat (denote-sluggify-title name) ".org")
			 attach-dir))
	    (org-link--add-to-stored-links (concat "attachment:" (buffer-name)) name)
	    (list name (buffer-name) attach-dir url))))))


  ;; ** org-babel

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     (ocaml . nil)
     (octave . t)
     (julia . t)
     (python . t)
     (ruby . t)
     (screen . nil)
     (shell . t)
     (sql . nil)
     (sqlite . t)))
  ;; ** org-refile

  (defun my/refile (file &optional headline arg)
    "Refile HEADLINE in FILE."
    (let ((pos (save-excursion
		 (find-file file)
		 (if headline (org-find-exact-headline-in-buffer headline)))))
      (org-refile arg nil (list headline file nil pos)))
    (switch-to-buffer (current-buffer)))

  (defun my/refile-this-file ()
    (interactive)
    (my/refile buffer-file-name))

  ;; ** END

  )

;; * ORG EXPORT
;; ** publish

(with-eval-after-load 'denote
  (setopt org-publish-project-alist
	  `(("denote" . (:base-directory ,denote-directory :publishing-directory "~/personal/publish" :auto-sitemap t))))
  (setopt org-export-with-broken-links 'mark))
;; ** ox-html

(use-package ox-html
  :custom
  (org-html-html5-fancy t)
  (org-html-head "<style>
    p { font-weight: normal; color: gray; }
    h1 { color: black; }
    .title { text-align: center; }
    .todo, .timestamp-kwd { color: red; }
    .done { color: green; }
</style>"))

;; ** ox-hugo

(use-package ox-hugo
  :straight t
  :after ox)

(with-eval-after-load 'ox-hugo
  (declare-function org-remove-indentation "org-macs")
  (declare-function org-export-with-backend "ox")
  ;;;; Export Block
  (defun my/org-hugo-export-block (export-block _contents _info)
    "Transcode a EXPORT-BLOCK element from Org to Hugo-compatible Markdown.
CONTENTS is nil.  INFO is a plist holding contextual information.

Example:

  #+begin_export hugo
  foo
  #+end_export

exports verbatim to \"foo\" only when exported using `hugo'
backend.

If the backend tag is \"markdown\"/\"md\" or \"html\", exporting
of those blocks falls back to the respective exporters."
    (cond
     ((string= (org-element-property :type export-block) "HUGO")
      (org-remove-indentation (org-element-property :value export-block)))
     ((string= (org-element-property :type export-block) "LATEX")
      (org-export-with-backend 'latex export-block nil nil))
     ;; Also include Markdown and HTML export blocks.
     ;; ox-md handles HTML export blocks too.
     (t
      (org-export-with-backend 'md export-block nil nil))))

  (advice-add #'org-hugo-export-block :override #'my/org-hugo-export-block)

  (defun my/org-hugo-link (link desc info)
    "Convert LINK to Markdown format.

DESC is the link's description.
INFO is a plist used as a communication channel.

Unlike `org-md-link', this function will also copy local images
and rewrite link paths to make blogging more seamless."
    (let* ((raw-link (org-element-property :raw-link link))
           (raw-path (org-element-property :path link))
           (type (org-element-property :type link))
           (link-is-url (member type '("http" "https" "ftp" "mailto"))))
      ;; (message "[org-hugo-link DBG] raw-path 1: %s" raw-path)

      (when (and (stringp raw-path)
		 link-is-url)
	(setq raw-path (org-blackfriday--url-sanitize-maybe
			info (url-encode-url raw-path))))
      ;; (message "[org-hugo-link DBG] raw-link: %s" raw-link)
      ;; (message "[org-hugo-link DBG] raw-path 2: %s" raw-path)
      ;; (message "[org-hugo-link DBG] link: %S" link)
      ;; (message "[org-hugo-link DBG] link type: %s" type)
      (cond
       ;; Link type is handled by a special function.
       ((member type '("denote"))
	(let* ((mdl (print (org-no-properties (org-export-custom-protocol-maybe link desc 'md))))
	       (file-regex "\\[\\(.+\\)\\](\\(.+\\))")
	       (parts (s-match file-regex mdl))
	       (desc (nth 1 parts))
	       (destination (nth 2 parts))
	       (destination (if (string= ".org" (downcase (file-name-extension destination ".")))
				(concat (file-name-sans-extension destination) ".md")
                              destination)))
	  (format "[%s]({{< relref \"%s\" >}})" desc destination)))
       ((org-export-custom-protocol-maybe link desc 'md))
       ((member type '("custom-id" "id"
                       "fuzzy")) ;<<target>>, #+name, heading links
	(let ((destination (if (string= type "fuzzy")
                               (org-export-resolve-fuzzy-link link info)
                             (org-export-resolve-id-link link info))))
          ;; (message "[org-hugo-link DBG] link type: %s" type)
          ;; (message "[org-hugo-link DBG] destination: %s" destination)
          ;; (message "[org-hugo-link DBG] link: %S" link)
          ;; (message "[org-hugo-link DBG] link destination elem type: %S" (org-element-type destination))
          (pcase (org-element-type destination)
            ;; External file.
            (`plain-text
             (let ((path (progn
                           ;; Treat links to `file.org' as links to `file.md'.
                           (if (string= ".org" (downcase (file-name-extension destination ".")))
                               (concat (file-name-sans-extension destination) ".md")
                             destination))))
               ;; (message "[org-hugo-link DBG] plain-text path: %s" path)
               (if (org-id-find-id-file raw-path)
                   (let* ((anchor (org-hugo-link--heading-anchor-maybe link info))
                          (ref (if (and (org-string-nw-p anchor)
					(not (string-prefix-p "#" anchor)))
                                   ;; If the "anchor" doesn't begin with
                                   ;; "#", it's a direct reference to a
                                   ;; post subtree.
                                   anchor
				 (concat path anchor))))
                     ;; (message "[org-hugo-link DBG] plain-text org-id anchor: %S" anchor)
                     (format "[%s]({{< relref \"%s\" >}})" (or desc path) ref))
		 (if desc
                     (format "[%s](%s)" desc path)
                   (format "<%s>" path)))))
            ;; Links of type [[* Some heading]].
            (`headline
             (let ((title (org-export-data (org-element-property :title destination) info)))
               (format
		"[%s](#%s)"
		;; Description
		(cond ((org-string-nw-p desc))
                      ((org-export-numbered-headline-p destination info)
                       (mapconcat #'number-to-string
                                  (org-export-get-headline-number destination info)
                                  "."))
                      (t
                       title))
		;; Reference
		(org-hugo--get-anchor destination info))))
            ;; Links to other Org elements like source blocks, tables,
            ;; paragraphs, standalone figures, <<target>> links, etc.
            (_
             (let ((description
                    (or (org-string-nw-p desc)
			(let ((number (org-export-get-ordinal
                                       destination info
                                       nil #'org-html--has-caption-p)))
                          (when number
                            (let ((num-str (if (atom number)
                                               (number-to-string number)
                                             (mapconcat #'number-to-string number "."))))
                              ;; (message "[org-hugo-link DBG] num-str: %s" num-str)
                              (if org-hugo-link-desc-insert-type
                                  (let* ((type (org-element-type destination))
					 ;; Org doesn't have a specific
					 ;; element for figures. So if
					 ;; the element is `paragraph',
					 ;; and as this element has an
					 ;; ordinal, we will assume that
					 ;; to be a figure.
					 (type (if (equal 'paragraph type)
                                                   'figure
						 type))
					 (type-str (org-blackfriday--translate type info)))
                                    (format "%s %s" type-str num-str))
				num-str)))))))
               ;; (message "[org-hugo-link DBG] link description: %s" description)
               (when description
		 (let ((dest-link (cond
                                   ;; Ref to a source block or table.
                                   ((memq (org-element-type destination) '(src-block table))
                                    (org-blackfriday--get-reference destination))
                                   ;; Ref to a standalone figure.
                                   ((and (org-html-standalone-image-p destination info)
					 (eq (org-element-type destination) 'paragraph))
                                    (let ((figure-ref (org-blackfriday--get-reference destination)))
                                      (if (org-string-nw-p figure-ref)
                                          (replace-regexp-in-string
                                           "\\`org-paragraph--"
                                           (org-blackfriday--get-ref-prefix 'figure)
                                           figure-ref)
					(org-export-get-reference destination info))))
                                   ;; Ref to a <<target>>.
                                   ((eq (org-element-type destination) 'target)
                                    (org-blackfriday--get-target-anchor destination))
                                   ;; Ref to all other link destinations.
                                   (t
                                    (org-export-get-reference destination info)))))
                   (format "[%s](#%s)" description dest-link))))))))
       ((org-export-inline-image-p link org-html-inline-image-rules)
	;; (message "[org-hugo-link DBG] processing an image: %s" desc)
	(let* ((parent (org-export-get-parent link))
               (parent-type (org-element-type parent))
               ;; If this is a hyper-linked image, it's parent type will
               ;; be a link too. Get the parent of *that* link in that
               ;; case.
               (grand-parent (when (eq parent-type 'link)
                               (org-export-get-parent parent)))
               (useful-parent (if grand-parent
                                  grand-parent
				parent))
               (attr (org-export-read-attribute :attr_html useful-parent))
               (caption (or
			 ;; Caption set using #+caption takes higher precedence.
			 (org-string-nw-p
                          (org-export-data  ;Look for caption set using #+caption
                           (org-export-get-caption (org-export-get-parent-element link))
                           info))
			 (plist-get attr :caption)))
               (caption (when (org-string-nw-p caption)
                          (format "%s%s%s%s"
                                  "<span class=\"figure-number\">"
                                  (format (org-html--translate
                                           (concat
                                            (cdr (assoc 'figure org-blackfriday--org-element-string))
                                            " %d:")
                                           info)
                                          (org-export-get-ordinal
                                           useful-parent info
                                           nil #'org-html--has-caption-p))
                                  " </span>"
                                  caption)))
               (extension (file-name-extension raw-path))
               (inlined-svg (and (stringp extension)
				 (string= "svg" (downcase extension))
				 (plist-get attr :inlined))))
          ;; (message "[org-hugo-link DBG] Inline image: %s, extension: %s" raw-path extension)
          ;; (message "[org-hugo-link DBG] inlined svg? %S" inlined-svg)
          ;; (message "[org-hugo-link DBG] caption: %s" caption)
          (if inlined-svg
              (let* ((svg-contents (with-temp-buffer
                                     (insert-file-contents raw-path)
                                     (fill-region (point-min) (point-max)) ;Make huge one-liner SVGs sane
                                     (buffer-substring-no-properties (point-min) (point-max))))
                     (svg-contents-sanitized (replace-regexp-in-string
                                              ;; Remove the HTML comments.
                                              "<!--\\(.\\|\n\\)*?-->" ""
                                              (replace-regexp-in-string
                                               ;; Remove the xml document tag as that cannot be inlined in-between
                                               ;; a Markdown (or even an HTML) file.
                                               "<\\?xml version=\"1\\.0\" encoding=\"UTF-8\" standalone=\"no\"\\?>" ""
                                               ;; Remove !DOCTYPE tag from the inlined SVG.
                                               (replace-regexp-in-string
						"<!DOCTYPE svg[^>]+>" ""
						svg-contents))))
                     (svg-html (if caption
                                   (format "<figure>\n%s\n<figcaption>\n\n  %s\n</figcaption>\n</figure>"
                                           svg-contents-sanitized caption)
				 svg-contents-sanitized)))
		;; (message "[org-hugo-link DBG] svg contents: %s" svg-contents)
		;; (message "[org-hugo-link DBG] svg contents sanitized: %s" svg-contents-sanitized)
		svg-html)
            (let* ((path (org-hugo--attachment-rewrite-maybe raw-path info))
                   (inline-image (not (org-html-standalone-image-p useful-parent info)))
                   (source (if link-is-url
                               (concat type ":" path)
                             path))
                   (num-attr (/ (length attr) 2)) ;(:alt foo) -> num-attr = 1
                   (alt-text (plist-get attr :alt)))
              ;; (message "[org-hugo-link DBG] path: %s" path)
              ;; (message "[org-hugo-link DBG] inline image? %s" inline-image)
              ;; (message "[org-hugo-link DBG] attr: %s num of attr: %d"
              ;;          attr (length attr))
              ;; (message "[org-hugo-link DBG] parent-type: %s" parent-type)
              ;; (message "[org-hugo-link DBG] useful-parent-type: %s"
              ;;          (org-element-type useful-parent))
              (cond
               (;; Use the Markdown image syntax if the image is inline and
		;; there are no HTML attributes for the image, or just one
		;; attribute, the `alt-text'.
		(and inline-image
                     (or (= 0 num-attr)
			 (and alt-text
                              (= 1 num-attr))))
		(let ((alt-text (if alt-text
                                    alt-text
                                  "")))
                  (format "![%s](%s)" alt-text source)))
               (;; Else if the image is inline (with non-alt-text
		;; attributes), use HTML <img> tag syntax.
		inline-image
		;; The "target" and "rel" attributes would be meant for <a>
		;; tags. So do not pass them to the <img> tag.
		(plist-put attr :target nil)
		(plist-put attr :rel nil)
		(org-html--format-image source attr info))
               (t ;Else use the Hugo `figure' shortcode.
		;; Hugo `figure' shortcode named parameters.
		;; https://gohugo.io/content-management/shortcodes/#figure
		(let ((figure-params `((src . ,source)
                                       (alt . ,alt-text)
                                       (caption . ,(when (org-string-nw-p caption)
                                                     (replace-regexp-in-string "\"" "\\\\\\&" caption))) ;Escape the double-quotes, if any.
                                       (link . ,(plist-get attr :link))
                                       (title . ,(plist-get attr :title))
                                       (class . ,(plist-get attr :class))
                                       (attr . ,(plist-get attr :attr))
                                       (attrlink . ,(plist-get attr :attrlink))
                                       (width . ,(plist-get attr :width))
                                       (height . ,(plist-get attr :height))
                                       ;; While the `target' and `rel'
                                       ;; attributes are not supported by
                                       ;; the inbuilt Hugo `figure'
                                       ;; shortcode, they can be used as
                                       ;; intended if a user has a custom
                                       ;; `figure' shortcode with the
                                       ;; support added for those.
                                       (target . ,(plist-get attr :target))
                                       (rel . ,(plist-get attr :rel))))
                      (figure-param-str ""))
                  (dolist (param figure-params)
                    (let ((name (car param))
                          (val (cdr param)))
                      (when val
			(setq figure-param-str (concat figure-param-str
                                                       (format "%s=\"%s\" "
                                                               name val))))))
                  ;; (message "[org-hugo-link DBG] figure params: %s" figure-param-str)
                  (format "{{< figure %s >}}" (org-trim figure-param-str)))))))))
       ((string= type "coderef")
	(let* ((ref-label (org-element-property :path link))
               (ref-info (org-hugo-link--resolve-coderef ref-label info))
               (desc (format (org-export-get-coderef-format ref-label desc)
                             (plist-get ref-info :ref))))
          ;; (message "[org-hugo-link DBG] coderef ref label: %s" ref-label)
          ;; (message "[org-hugo-link DBG] coderef ref str: %s" (plist-get ref-info :ref))
          ;; (message "[org-hugo-link DBG] coderef anchor prefix: %s" (plist-get ref-info :anchor-prefix))
          ;; (message "[org-hugo-link DBG] coderef line num: %s" (plist-get ref-info :line-num))
          ;; (message "[org-hugo-link DBG] coderef desc: %s" desc)
          (format "[%s](#%s-%s)"
                  desc
                  (plist-get ref-info :anchor-prefix)
                  (plist-get ref-info :line-num))))
       ((string= type "radio")
	(let ((destination (org-export-resolve-radio-link link info)))
          (format "[%s](#%s%s)"
                  desc
                  (org-blackfriday--get-ref-prefix 'radio)
                  (org-blackfriday--valid-html-anchor-name
                   (org-element-property :value destination)))))
       (t ;[[file:foo.png]], [[file:foo.org::* Heading]], [[file:foo.org::#custom-id]], link type: file
	(let* ((link-param-str "")
               (path (cond
                      (link-is-url
                       ;; Taken from ox-html.el -- Extract attributes
                       ;; from parent's paragraph.  HACK: Only do this
                       ;; for the first link in parent (inner image link
                       ;; for inline images).  This is needed as long as
                       ;; attributes cannot be set on a per link basis.
                       (let* ((attr
                               (let ((parent (org-export-get-parent-element link)))
				 (and (eq (org-element-map parent 'link #'identity info :first-match) link)
                                      (org-export-read-attribute :attr_html parent))))
                              ;; https://www.w3schools.com/tags/tag_link.asp
                              (link-params `((title . ,(plist-get attr :title))
                                             (style . ,(plist-get attr :style))
                                             (referrerpolicy . ,(plist-get attr :referrerpolicy))
                                             (media . ,(plist-get attr :media))
                                             (target . ,(plist-get attr :target))
                                             (rel . ,(plist-get attr :rel))
                                             (sizes . ,(plist-get attr :sizes))
                                             (type . ,(plist-get attr :type)))))
			 (dolist (param link-params)
                           (let ((name (car param))
				 (val (cdr param)))
                             (when val
                               (setq link-param-str (concat link-param-str
                                                            (format "%s=\"%s\" "
                                                                    name val))))))
			 ;; (message "[org-hugo-link DBG] link params: %s" link-param-str)
			 )
                       (concat type ":" raw-path))
                      (;; Remove the "file://" prefix.
                       (string= type "file")
                       ;; (message "[org-hugo-link DBG] raw-path: %s" raw-path)
                       (let* ((path1 (replace-regexp-in-string "\\`file://" "" raw-path))
                              (path-lc (downcase path1)))
			 (cond
                          (;; foo.org, foo.org::* Heading, foo.org::#custom_id
                           (string= ".org" (file-name-extension path-lc "."))
                           (let ((ref "")
				 (anchor ""))
                             (if (string-suffix-p org-hugo--preprocessed-buffer-dummy-file-suffix path-lc)
				 (progn
                                   (setq ref (string-remove-suffix
                                              org-hugo--preprocessed-buffer-dummy-file-suffix
                                              (file-name-nondirectory path1)))
                                   ;; Dummy Org file paths created in
                                   ;; `org-hugo--get-pre-processed-buffer'
                                   ;; For dummy Org file paths, we are
                                   ;; limiting to only "#" style search
                                   ;; strings.
                                   (when (string-match ".*\\.org::\\(#.*\\)" raw-link)
                                     (setq anchor (match-string-no-properties 1 raw-link))))
                               ;; Regular Org file paths.
                               (setq ref (file-name-sans-extension (file-name-nondirectory path1)))
                               (let ((link-search-str
                                      ;; If raw-link is "./foo.org::#bar",
                                      ;; set `link-search-str' to
                                      ;; "#bar".
                                      (when (string-match ".*\\.org::\\(.*\\)" raw-link)
					(match-string-no-properties 1 raw-link))))
				 ;; (message "[org-hugo-link DBG] link-search-str: %s" link-search-str)
				 (when link-search-str
                                   (setq anchor (org-hugo--search-and-get-anchor raw-path link-search-str info)))))
                             ;; (message "[org-hugo-link file.org::*Heading DBG] ref    = %s" ref)
                             ;; (message "[org-hugo-link file.org::*Heading DBG] anchor = %s" anchor)
                             (cond
                              ;; Link to a post subtree.  In this case,
                              ;; the "anchor" is actually the post's
                              ;; slug.
                              ((and (org-string-nw-p anchor) (not (string-prefix-p "#" anchor)))
                               (format "{{< relref \"%s\" >}}" anchor))
                              ;; Link to a non-post subtree, like a subheading in a post.
                              ((or (org-string-nw-p ref) (org-string-nw-p anchor))
                               (format "{{< relref \"%s%s\" >}}" ref anchor))
                              (t
                               ""))))
                          (t ;; attachments like foo.png
                           (org-hugo--attachment-rewrite-maybe path1 info)))))
                      (t
                       raw-path)))
               (link-param-str (org-string-nw-p (org-trim link-param-str))))
          ;; (message "[org-hugo-link DBG] desc=%s path=%s" desc path)
          ;; (message "[org-hugo-link DBG] link-param-str=%s" link-param-str)
          (cond
           ;; Link description is a `figure' shortcode but does not
           ;; already have the `link' parameter set.
           ((and desc
		 (string-match-p "\\`{{<\\s-*figure\\s-+" desc)
		 (not (string-match-p "\\`{{<\\s-*figure\\s-+.*link=" desc)))
            (replace-regexp-in-string "\\s-*>}}\\'"
                                      (format " link=\"%s\"\\&" path)
                                      desc))
           ;; Both link description and link attributes are present.
           ((and desc
		 link-param-str)
            (format "<a href=\"%s\" %s>%s</a>"
                    (org-html-encode-plain-text path)
                    link-param-str
                    (org-link-unescape desc)))
           ;; Only link description, but no link attributes.
           (desc
            (let* ((path-has-space (and
                                    (not (string-prefix-p "{{< relref " path))
                                    (string-match-p "\\s-" path)))
                   (path (if path-has-space
                             ;; https://github.com/kaushalmodi/ox-hugo/issues/376
                             ;; https://github.com/gohugoio/hugo/issues/6742#issuecomment-573924706
                             (format "<%s>" path)
                           path)))
              (format "[%s](%s)" desc path)))
           ;; Only link attributes, but no link description.
           (link-param-str
            (let ((path (org-html-encode-plain-text path)))
              (format "<a href=\"%s\" %s>%s</a>"
                      path
                      link-param-str
                      ;; Below trick is to prevent Hugo from
                      ;; auto-hyperlinking the link in the
                      ;; description. Idea from
                      ;; https://stackoverflow.com/q/25706012/1219634.
                      (replace-regexp-in-string ":" "&colon;" (org-link-unescape path)))))
           ;; Neither link description, nor link attributes.
           ((string-prefix-p "{{< relref " path)
            (format "[%s](%s)" path path))
           ((org-string-nw-p path)
            (format "<%s>" path))
           (t
            "")))))))

  (advice-add #'org-hugo-link :override #'my/org-hugo-link))

;; ** ox-pandoc

(use-package ox-pandoc
  :straight t
  :after ox
  :custom
  (org-pandoc-menu-entry
   '(
     ;;(?0 "to jats." org-pandoc-export-to-jats)
     ;;(?0 "to jats and open." org-pandoc-export-to-jats-and-open)
     ;;(?  "as jats." org-pandoc-export-as-jats)
     ;;(?1 "to epub2 and open." org-pandoc-export-to-epub2-and-open)
     ;;(?! "to epub2." org-pandoc-export-to-epub2)
     ;;(?2 "to tei." org-pandoc-export-to-tei)
     ;;(?2 "to tei and open." org-pandoc-export-to-tei-and-open)
     ;;(?" "as tei." org-pandoc-export-as-tei)
     ;;(?3 "to markdown_mmd." org-pandoc-export-to-markdown_mmd)
     ;;(?3 "to markdown_mmd and open." org-pandoc-export-to-markdown_mmd-and-open)
     ;;(?# "as markdown_mmd." org-pandoc-export-as-markdown_mmd)
     ;;(?4 "to html5." org-pandoc-export-to-html5)
     (?4 "to html5 and open." org-pandoc-export-to-html5-and-open)
     (?$ "as html5." org-pandoc-export-as-html5)
     (?5 "to html5-pdf and open." org-pandoc-export-to-html5-pdf-and-open)
     (?% "to html5-pdf." org-pandoc-export-to-html5-pdf)
     ;;(?6 "to markdown_phpextra." org-pandoc-export-to-markdown_phpextra)
     ;;(?6 "to markdown_phpextra and open." org-pandoc-export-to-markdown_phpextra-and-open)
     ;;(?& "as markdown_phpextra." org-pandoc-export-as-markdown_phpextra)
     ;;(?7 "to markdown_strict." org-pandoc-export-to-markdown_strict)
     ;;(?7 "to markdown_strict and open." org-pandoc-export-to-markdown_strict-and-open)
     ;;(?' "as markdown_strict." org-pandoc-export-as-markdown_strict)
     ;;(?8 "to opendocument." org-pandoc-export-to-opendocument)
     ;;(?8 "to opendocument and open." org-pandoc-export-to-opendocument-and-open)
     ;;(?( "as opendocument." org-pandoc-export-as-opendocument)
     ;;(?9 "to opml." org-pandoc-export-to-opml)
     ;;(?9 "to opml and open." org-pandoc-export-to-opml-and-open)
     ;;(?) "as opml." org-pandoc-export-as-opml)
     ;;(?: "to rst." org-pandoc-export-to-rst)
     ;;(?: "to rst and open." org-pandoc-export-to-rst-and-open)
     ;;(?* "as rst." org-pandoc-export-as-rst)
     ;;(?< "to slideous." org-pandoc-export-to-slideous)
     ;; (?\[ "to jira." org-pandoc-export-to-jira)
     ;; (?\[ "as jira." org-pandoc-export-as-jira)
     ;; (?< "to slideous and open." org-pandoc-export-to-slideous-and-open)
     ;; (?, "as slideous." org-pandoc-export-as-slideous)
     (?= "to ms-pdf and open." org-pandoc-export-to-ms-pdf-and-open)
     (?- "to ms-pdf." org-pandoc-export-to-ms-pdf)
     ;;(?> "to textile." org-pandoc-export-to-textile)
     ;;(?> "to textile and open." org-pandoc-export-to-textile-and-open)
     ;;(?. "as textile." org-pandoc-export-as-textile)
     ;;(?a "to asciidoc." org-pandoc-export-to-asciidoc)
     ;;(?a "to asciidoc and open." org-pandoc-export-to-asciidoc-and-open)
     ;;(?A "as asciidoc." org-pandoc-export-as-asciidoc)
     (?b "to beamer-pdf and open." org-pandoc-export-to-beamer-pdf-and-open)
     (?B "to beamer-pdf." org-pandoc-export-to-beamer-pdf)
     ;; (?c "to context-pdf and open." org-pandoc-export-to-context-pdf-and-open)
     ;; (?C "to context-pdf." org-pandoc-export-to-context-pdf)
     ;;(?d "to docbook5." org-pandoc-export-to-docbook5)
     (?d "to docbook5 and open." org-pandoc-export-to-docbook5-and-open)
     (?D "as docbook5." org-pandoc-export-as-docbook5)
     ;; (?e "to epub3 and open." org-pandoc-export-to-epub3-and-open)
     ;; (?E "to epub3." org-pandoc-export-to-epub3)
     ;;(?f "to fb2." org-pandoc-export-to-fb2)
     ;;(?f "to fb2 and open." org-pandoc-export-to-fb2-and-open)
     ;;(?F "as fb2." org-pandoc-export-as-fb2)
     ;;(?g "to gfm." org-pandoc-export-to-gfm)
     (?g "to gfm and open." org-pandoc-export-to-gfm-and-open)
     (?G "as gfm." org-pandoc-export-as-gfm)
     ;;(?h "to html4." org-pandoc-export-to-html4)
     (?h "to html4 and open." org-pandoc-export-to-html4-and-open)
     (?H "as html4." org-pandoc-export-as-html4)
     ;;(?i "to icml." org-pandoc-export-to-icml)
     ;; (?i "to icml and open." org-pandoc-export-to-icml-and-open)
     ;; (?I "as icml." org-pandoc-export-as-icml)
     ;;(?j "to json." org-pandoc-export-to-json)
     (?j "to json and open." org-pandoc-export-to-json-and-open)
     (?J "as json." org-pandoc-export-as-json)
     ;;(?k "to markdown." org-pandoc-export-to-markdown)
     ;;(?k "to markdown and open." org-pandoc-export-to-markdown-and-open)
     ;;(?K "as markdown." org-pandoc-export-as-markdown)
     (?l "to latex-pdf and open." org-pandoc-export-to-latex-pdf-and-open)
     (?L "to latex-pdf." org-pandoc-export-to-latex-pdf)
     ;;(?m "to man." org-pandoc-export-to-man)
     (?m "to man and open." org-pandoc-export-to-man-and-open)
     (?M "as man." org-pandoc-export-as-man)
     ;;(?n "to native." org-pandoc-export-to-native)
     (?n "to native and open." org-pandoc-export-to-native-and-open)
     (?N "as native." org-pandoc-export-as-native)
     (?o "to odt and open." org-pandoc-export-to-odt-and-open)
     (?O "to odt." org-pandoc-export-to-odt)
     (?p "to pptx and open." org-pandoc-export-to-pptx-and-open)
     (?P "to pptx." org-pandoc-export-to-pptx)
     ;;(?q "to commonmark." org-pandoc-export-to-commonmark)
     ;;(?q "to commonmark and open." org-pandoc-export-to-commonmark-and-open)
     ;;(?Q "as commonmark." org-pandoc-export-as-commonmark)
     ;;(?r "to rtf." org-pandoc-export-to-rtf)
     (?r "to rtf and open." org-pandoc-export-to-rtf-and-open)
     (?R "as rtf." org-pandoc-export-as-rtf)
     ;;(?s "to s5." org-pandoc-export-to-s5)
     ;;(?s "to s5 and open." org-pandoc-export-to-s5-and-open)
     ;;(?S "as s5." org-pandoc-export-as-s5)
     ;;(?t "to texinfo." org-pandoc-export-to-texinfo)
     ;;(?t "to texinfo and open." org-pandoc-export-to-texinfo-and-open)
     ;;(?T "as texinfo." org-pandoc-export-as-texinfo)
     (?< "to typst." org-pandoc-export-to-typst)
     (?, "to typst and open." org-pandoc-export-to-typst-and-open)
     ;; (?, "as typst." org-pandoc-export-as-typst)
     (?> "to typst-pdf." org-pandoc-export-to-typst-pdf)
     (?. "to typst-pdf and open." org-pandoc-export-to-typst-pdf-and-open)
     ;;(?u "to dokuwiki." org-pandoc-export-to-dokuwiki)
     ;; (?u "to dokuwiki and open." org-pandoc-export-to-dokuwiki-and-open)
     ;; (?U "as dokuwiki." org-pandoc-export-as-dokuwiki)
     ;;(?v "to revealjs." org-pandoc-export-to-revealjs)
     ;; (?v "to revealjs and open." org-pandoc-export-to-revealjs-and-open)
     ;; (?V "as revealjs." org-pandoc-export-as-revealjs)
     ;;(?w "to mediawiki." org-pandoc-export-to-mediawiki)
     ;; (?w "to mediawiki and open." org-pandoc-export-to-mediawiki-and-open)
     ;; (?W "as mediawiki." org-pandoc-export-as-mediawiki)
     (?x "to docx and open." org-pandoc-export-to-docx-and-open)
     (?X "to docx." org-pandoc-export-to-docx)
     ;;(?y "to slidy." org-pandoc-export-to-slidy)
     ;; (?y "to slidy and open." org-pandoc-export-to-slidy-and-open)
     ;; (?Y "as slidy." org-pandoc-export-as-slidy)
     ;;(?z "to dzslides." org-pandoc-export-to-dzslides)
     ;; (?z "to dzslides and open." org-pandoc-export-to-dzslides-and-open)
     ;; (?Z "as dzslides." org-pandoc-export-as-dzslides)
     ;;(?{ "to muse." org-pandoc-export-to-muse)
     ;;(?{ "to muse and open." org-pandoc-export-to-muse-and-open)
     ;;(?[ "as muse." org-pandoc-export-as-muse)
     ;;(?} "to zimwiki." org-pandoc-export-to-zimwiki)
     ;;(?} "to zimwiki and open." org-pandoc-export-to-zimwiki-and-open)
     ;;(?] "as zimwiki." org-pandoc-export-as-zimwiki)
     ;;(?~ "to haddock." org-pandoc-export-to-haddock)
     ;;(?~ "to haddock and open." org-pandoc-export-to-haddock-and-open)
     ;;(?^ "as haddock." org-pandoc-export-as-haddock)
     )))


;; ** ox-latex

(use-package ox-latex
  :straight nil
  :custom
  (org-latex-pdf-process (list "latexmk -pdflatex=\"xelatex\" -%latex -shell-escape -bibtex -output-directory=%o -f -pdf %f")))

;; ** ox-icalendar

(use-package ox-icalendar
  :custom
  (org-icalendar-include-body 100))

;; * ORG BABEL
;; ** ob-python

(use-package ob-python
  :after org
  :config
  (setq org-babel-python-command "python3"))

;; ** ob-ipython

(use-package ob-ipython
  :straight t
  :disabled
  :after org
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((ipython . t))))

;; ** ob-julia

(use-package ob-julia
  :after org
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((julia . t))))

;; (use-package ob-julia-vterm
;;   :after org
;;   :config
;;   (add-to-list 'org-babel-load-languages '(julia-vterm . t))
;;   (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
;;   (defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm)
;;   (defalias 'org-babel-variable-assignments:julia 'org-babel-variable-assignments:julia-vterm))

(with-eval-after-load 'ob-julia-vterm
  (defun ob-julia-vterm-make-str-to-run (uuid params src-file out-file)
    "Make Julia code that execute-s the code in SRC-FILE depending on PARAMS.
The results are saved in OUT-FILE.  UUID is a unique id assigned
to the evaluation."
    (format
     (pcase (cdr (assq :result-type params))
       ('output "\
#OB-JULIA-VTERM_BEGIN %s
import Logging; let
    out_file = \"%s\"
    open(out_file, \"w\") do io
        logger = Logging.ConsoleLogger(io)
        redirect_stdout(io) do
            try
                include(\"%s\")
                # %s %s
            catch e
                showerror(logger.stream, e, %s)
            end
        end
    end
    result = open(io -> println(read(io, String)), out_file)
    if result == nothing
        open(out_file, \"a\") do io
            print(io, \"\n\")
        end
    else
        result
    end
end #OB-JULIA-VTERM_END\n")
       ('value "\
#OB-JULIA-VTERM_BEGIN %s
import Logging, Pkg; open(\"%s\", \"w\") do io
    logger = Logging.ConsoleLogger(io)
    try
        result = include(\"%s\")
        if !(result isa Nothing)
            if isdefined(Main, :PrettyPrinting) && isdefined(PrettyPrinting, :pprint) ||
               \"PrettyPrinting\" in [p.name for p in values(Pkg.dependencies())]
                @eval import PrettyPrinting
                Base.invokelatest(PrettyPrinting.pprint, io, result)
            else
                Base.invokelatest(print, io, result)
            end
        else
            if %s
                Base.invokelatest(show, io, \"text/plain\", result)
            else
                Base.invokelatest(show, IOContext(io, :limit => true), \"text/plain\", result)
            end
        end
        result
    catch e
        msg = sprint(showerror, e, %s)
        println(logger.stream, msg)
        println(msg)
        nothing
    end
end #OB-JULIA-VTERM_END\n"))
     (substring uuid 0 8) out-file src-file
     (if (member "pp" (cdr (assq :result-params params))) "true" "false")
     (if (member "nolimit" (cdr (assq :result-params params))) "true" "false")
     (if (not (member (cdr (assq :debug params)) '(nil "no"))) "catch_backtrace()" ""))))


;; ** ob-shell

(use-package ob-shell
  :disabled
  :after org
  :config
  (setq org-babel-default-header-args:sh '((:results . "output")))
  (setq org-babel-default-header-args:shell '((:results . "output")))
  (org-babel-do-load-languages 'org-babel-load-languages '((sh . t))))

;; ** ob-latex

(use-package ob-latex
  :after org)

;; ** ob-async

;; python does not work.
(use-package ob-async
  :disabled
  :after org
  :config
  (setq ob-async-no-async-languages-alist '("python" "ipython"))
  (add-hook 'ob-async-pre-execute-src-block-hook
            #'(lambda ()
		(setq inferior-julia-program-name "/usr/local/bin/julia"))))

;; ** ob-hledger

(use-package ob-ledger
  :disabled
  :after org
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((ledger . t))))

;; ** ob-asymptote

(use-package ob-asymptote
  :straight t
  :after org
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((asymptote . t))))

(with-eval-after-load 'ob-asymptote
  (defun org-babel-execute:asymptote (body params)
    "Execute a block of Asymptote code.
This function is called by `org-babel-execute-src-block'."
    (let* ((out-file (cdr (assq :file params)))
           (format (or (file-name-extension out-file)
                       "pdf"))
           (cmdline (cdr (assq :cmdline params)))
           (in-file (org-babel-temp-file "asymptote-"))
	   (out-file-path (if out-file
			      (org-babel-process-file-name out-file)
			    nil))
           (cmd
	    (concat "asy "
		    (if out-file
			(concat
			 "-globalwrite -f " format
			 " -o " (file-name-sans-extension out-file-path))
		      "-V")
		    " " cmdline
		    " " (org-babel-process-file-name in-file))))
      (with-temp-file in-file
	(insert (org-babel-expand-body:generic
		 body params
		 (org-babel-variable-assignments:asymptote params))))
      (message cmd) (shell-command cmd)
      nil)))

;; * ORG LINK

(with-eval-after-load 'ol
  (defun org-attach-complete-link ()
    "Advise the user with the available files in the attachment directory."
    (let ((attach-dir (org-attach-dir)))
      (if attach-dir
	  (let* ((attached-dir (expand-file-name attach-dir))
		 ;; Patch this to immediately show files in attach dir
		 (file (read-file-name "File: " (concat attached-dir "/")))
		 (pwd (file-name-as-directory attached-dir))
		 (pwd-relative (file-name-as-directory
				(abbreviate-file-name attached-dir))))
	    (cond
	     ((string-match (concat "^" (regexp-quote pwd-relative) "\\(.+\\)") file)
	      (concat "attachment:" (match-string 1 file)))
	     ((string-match (concat "^" (regexp-quote pwd) "\\(.+\\)")
			    (expand-file-name file))
	      (concat "attachment:" (match-string 1 (expand-file-name file))))
	     (t (concat "attachment:" file))))
	(error "No attachment directory exist"))))
  )

;; * PLUGINS
;; ** org-modern

(straight-use-package 'org-modern)
(require 'org-modern)
(set-face-attribute 'org-modern-symbol nil :family "Iosevka")
(set-face-attribute 'org-modern-label nil :height 0.85)
(set-face-attribute 'org-modern-date-active nil :height 1.1 :overline "gray80"
		    :box '(:line-width
			   (-1 . -1)
			   :color "gray80" :style flat-button)
		    :background "gray96")
(set-face-attribute 'org-modern-date-inactive nil :height 1.1 :overline "gray80"
		    :box '(:line-width
			   (-1 . -1)
			   :color "gray80" :style flat-button)
		    :background "gray96")

(defun my/org-modern-apply-faces ()
  "Apply org modern faces"
  (set-face-attribute
   'org-modern-date-active nil :height 1.1 :overline "gray80"
   :box '(:line-width
	  (-1 . -1)
	  :color "gray80" :style flat-button)
   :background (modus-themes-get-color-value 'bg-mode-line-inactive)
   :foreground (modus-themes-get-color-value 'fg-mode-line-inactive))
  (set-face-attribute
   'org-modern-date-inactive nil :height 1.1 :overline "gray80"
   :box '(:line-width
	  (-1 . -1)
	  :color "gray80" :style flat-button)
   :background (modus-themes-get-color-value 'bg-mode-line-inactive)
   :foreground (modus-themes-get-color-value 'fg-mode-line-inactive)))

(with-eval-after-load 'modus-themes
  (add-hook 'modus-themes-after-load-theme-hook #'my/org-modern-apply-faces)
  (my/org-modern-apply-faces))

(setopt org-modern-keyword nil
	org-modern-checkbox nil
	org-modern-star 'replace
	org-modern-table nil)

(global-org-modern-mode)

;; ** org-protocol

(use-package org-protocol
  :after org
  :custom
  (org-protocol-protocol-alist
   '(("eww" :protocol "eww" :function my/open-in-eww-protocol)
     ("body" :protocol "body" :function my/org-protcol-capture-body)))
  :config
  (defun my/open-in-eww-protocol (info)
    (x-focus-frame nil)
    (let ((url (plist-get info :url)))
      (eww url)))

  (defun my/org-protcol-capture-body (info)
    (x-focus-frame nil)
    (let ((body (plist-get info :body)))
      (eww-open-file (make-temp-file "org-protocol" nil ".html" body)))))

;; ** org-contrib

(use-package org-contrib
  :straight t)

;; ** org-super-agenda

;; https://github.com/alphapapa/org-super-agenda#Usage

(use-package org-super-agenda
  :straight t
  :after org-agenda
  :hook (org-agenda-mode . org-super-agenda-mode)
  :custom
  (org-super-agenda-groups
   '(;; Each group has an implicit boolean OR operator between its selectors.
     (:name "Today"  ; Optionally specify section name
            :time-grid t  ; Items that appear on the time grid
            :todo "TODAY")  ; Items that have this TODO keyword
     (:name "Important"
            ;; Single arguments given alone
            :tag "bills"
            :priority "A")
     (:name "Diary"
	    :category "Diary")
     (:name "Habits"
	    :habit t
	    :order 2)
     (:priority<= "B"
                  ;; Show this section after "Today" and "Important", because
                  ;; their order is unspecified, defaulting to 0. Sections
                  ;; are displayed lowest-number-first.
                  :order 1)))
  :custom-face
  (org-super-agenda-header ((t (:height 0.75))))
  :bind
  (:map org-agenda-mode-map
	("C-c C-x C-a" . org-agenda-archive-default)))

;; ** org-appear

(use-package org-appear
  :straight t
  :hook org-mode
  :custom
  (org-appear-autolinks nil)
  (org-appear-autoentities t)
  (org-appear-autokeywords t))

;; ** org-fragtog

(use-package org-fragtog
  :straight t
  :after org
  ;; :hook org-mode
  :custom
  (org-startup-with-latex-preview nil)
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 2)
   (plist-put org-format-latex-options :foreground 'auto)
   (plist-put org-format-latex-options :background 'auto)))

;; ** TODO org-ref

(use-package org-ref
  :straight t
  :disabled
  :demand t
  :bind
  (:map org-mode-map
	("<f7>" . org-ref-insert-link-hydra/body)
	("C-<f7>" . org-ref-citation-hydra/body))
  (:map bibtex-mode-map
	("<f7>" . org-ref-bibtex-hydra/body)))

(with-eval-after-load 'org-ref
  (defun my/org-ref-cite-insert-consult ()
    "Insert a citation using `consult-bibtex'."
    (interactive)

    (let ((key (consult-bibtex--read-entry)))
      (if key
	  (org-ref-insert-cite-key key))))

  (setopt org-ref-insert-cite-function 'my/org-ref-cite-insert-consult)

  (defun doi-utils-crossref (doi)
    "Search DOI in CrossRef."
    (interactive "sDOI: ")
    (browse-url
     (format "http://search.crossref.org/search/works?q=%s&from_ui=yes" (url-hexify-string doi))))

  (defun my/org-ref-process-buffer--html (backend)
    "Preprocess `org-ref' citations to HTML format.

Do this only if the export backend is `html' or a derivative of
that."
    ;; `ox-hugo' is derived indirectly from `ox-html'.
    ;; ox-hugo <- ox-blackfriday <- ox-md <- ox-html
    (when (org-export-derived-backend-p backend 'html)
      (org-ref-process-buffer 'html)))

  (with-eval-after-load 'org-compat
    (add-to-list 'org-export-before-parsing-hook #'my/org-ref-process-buffer--html))  )

;; ** org-remark
(require 'org-cycle)
(use-package org-remark
  :straight t
  :bind (;; :bind keyword also implicitly defers org-remark itself.
	 ;; Keybindings before :map is set for global-map.
	 :map org-remark-mode-map
	 ("C-c r m" . org-remark-mark)
	 ("C-c r l" . org-remark-mark-line)
	 ("C-c r o" . org-remark-open)
	 ("C-c r n" . org-remark-next)
	 ("C-c r p" . org-remark-prev)
	 ("C-c r ]" . org-remark-view-next)
	 ("C-c r [" . org-remark-view-prev)
	 ("C-c r r" . org-remark-remove)
	 ("C-c r d" . org-remark-delete)
	 ("C-c r v" . org-remark-view))
  :init
  ;; (org-remark-global-tracking-mode +1)
  :hook (org-remark-open . (lambda () (org-cycle-hide-drawers 'all)))
  :custom
  (org-remark-notes-file-name "~/personal/Org/remark.org")
  (org-remark-line-minimum-left-margin-width 1)
  (org-remark-line-heading-title-max-length 70))
;;(use-package org-remark-nov  :after nov  :config (org-remark-nov-mode +1)))

;; ** org-mac-link

(use-package org-mac-link
  :straight t
  :when (eq system-type 'darwin)
  :after org
  :bind
  (:map org-mode-map ("C-c L" . my/org-mac-link-get-link))
  :init
  (setq org-mac-link-brave-app-p nil
	org-mac-link-chrome-app-p nil
	org-mac-link-acrobat-app-p nil
	org-mac-link-outlook-app-p nil
	org-mac-link-addressbook-app-p nil
	org-mac-link-qutebrowser-app-p nil
	org-mac-link-finder-app-p t
	org-mac-link-mail-app-p t
	org-mac-link-devonthink-app-p nil
	org-mac-link-safari-app-p nil
	org-mac-link-librewolf-app-p t
	org-mac-link-firefox-vimperator-p nil
	org-mac-link-evernote-app-p nil
	org-mac-link-together-app-p nil
	org-mac-link-skim-app-p nil))

(with-eval-after-load 'org-mac-link
  (defcustom org-mac-link-librewolf-app-p nil
    "Whether to use the LibreWolf.app for `org-mac-link' functions."
    :tag "Grab LibreWolf.app links"
    :group 'org-mac-link
    :type 'boolean)

  (defun my/org-mac-link-applescript-librewolf-get-frontmost-url ()
    "AppleScript to get the links to the frontmost window of the LibreWolf.app."
    (let ((result
	   (org-mac-link-do-applescript
	    (concat
	     "tell application \"System Events\"\n"
	     "   tell its application process \"LibreWolf\"\n"
	     "       set theTitle to get name of window 1\n"
	     "       set theUrl to get value of UI element 1 of combo box 1 of toolbar \"Navigation\" of first group of window 1\n"
	     "    end tell\n"
	     "end tell\n"
	     "set theResult to (get theUrl) & \"::split::\" & (get theTitle)\n"
	     "set links to {}\n"
	     "copy theResult to the end of links\n"
	     "return links as string\n"))))
      (car (split-string result "[\r\n]+" t))))

  (defun my/org-mac-link-librewolf-get-frontmost-url ()
    "Get the link to the frontmost window of the LibreWolf.app."
    (interactive)
    (message "Applescript: Getting Firefox url...")
    (org-mac-link-paste-applescript-links (my/org-mac-link-applescript-librewolf-get-frontmost-url)))

  (defun my/org-mac-link-librewolf-insert-frontmost-url ()
    "Insert the link to the frontmost window of the LibreWolf.app."
    (interactive)
    (insert (my/org-mac-link-librewolf-get-frontmost-url)))

  (defun my/org-mac-link-get-link (&optional beg end)
    "Prompt for an application to grab a link from.
When done, go grab the link, and insert it at point. If a region
is active, that will be the link's description."
    (interactive
     (if (use-region-p)
	 (list (region-beginning) (region-end))
       '()))
    (let* ((descriptors
	    `(("F" "inder" org-mac-link-finder-insert-selected ,org-mac-link-finder-app-p)
	      ("m" "ail" org-mac-link-mail-insert-selected ,org-mac-link-mail-app-p)
	      ("d" "EVONthink Pro Office" org-mac-link-devonthink-item-insert-selected
	       ,org-mac-link-devonthink-app-p)
	      ("o" "utlook" org-mac-link-outlook-message-insert-selected ,org-mac-link-outlook-app-p)
	      ("a" "ddressbook" org-mac-link-addressbook-item-insert-selected ,org-mac-link-addressbook-app-p)
	      ("s" "afari" org-mac-link-safari-insert-frontmost-url ,org-mac-link-safari-app-p)
	      ("l" "ibrewolf" my/org-mac-link-librewolf-insert-frontmost-url ,org-mac-link-librewolf-app-p)
	      ("v" "imperator" org-mac-link-vimperator-insert-frontmost-url ,org-mac-link-firefox-vimperator-p)
	      ("c" "hrome" org-mac-link-chrome-insert-frontmost-url ,org-mac-link-chrome-app-p)
	      ("b" "rave" org-mac-link-brave-insert-frontmost-url ,org-mac-link-brave-app-p)
	      ("e" "evernote" org-mac-link-evernote-note-insert-selected ,org-mac-link-evernote-app-p)
	      ("t" "ogether" org-mac-link-together-insert-selected ,org-mac-link-together-app-p)
	      ("S" "kim" org-mac-link-skim-insert-page ,org-mac-link-skim-app-p)
	      ("A" "crobat" org-mac-link-acrobat-insert-page ,org-mac-link-acrobat-app-p)
	      ("q" "utebrowser" org-mac-link-qutebrowser-insert-frontmost-url ,org-mac-link-qutebrowser-app-p)))
	   (menu-string (make-string 0 ?x))
	   input)

      ;; Create the menu string for the keymap
      (mapc (lambda (descriptor)
	      (when (elt descriptor 3)
		(setf menu-string (concat menu-string
					  "[" (elt descriptor 0) "]"
					  (elt descriptor 1) " "))))
	    descriptors)
      (setf (elt menu-string (- (length menu-string) 1)) ?:)

      ;; Prompt the user, and grab the link
      (message menu-string)
      (setq input (read-char-exclusive))
      (mapc (lambda (descriptor)
	      (let ((key (elt (elt descriptor 0) 0))
		    (active (elt descriptor 3))
		    (grab-function (elt descriptor 2)))
		(when (and active (eq input key))
		  (if (and beg end)
		      (let ((new-desc (buffer-substring beg end))
			    end-desc)
			(delete-region beg end)
			(call-interactively grab-function)
			(save-excursion
			  (backward-char 2)
			  (setq end-desc (point))
			  (search-backward "][")
			  (forward-char 2)
			  (delete-region (point) end-desc)
			  (insert new-desc)))
		    (call-interactively grab-function)))))
	    descriptors))))


;; ** org-noter


(use-package org-noter
  :straight t
  :bind
  (:map org-noter-doc-mode-map ("q" . nil))
  (:map org-mode-map
	("C-c C-x n n" . org-noter)
	("C-c C-x n k" . org-noter-kill-session)
	("C-c C-x n s" . org-noter-create-skeleton))
  :custom
  (org-noter-notes-search-path '("/Users/mw/personal/Org"))
  (org-noter--show-arrow-hook nil)
  (org-noter-default-notes-file-names '("noter.org"))
  (org-noter-always-create-frame nil)
  (org-noter-auto-save-last-location t)
  (org-noter-doc-split-fraction '(0.5 . 0.5))
  (org-noter-kill-frame-at-session-end nil)
  (org-noter-separate-notes-from-heading t)
  (org-noter-supported-modes '(doc-view-mode pdf-view-mode))
  :config
  (add-to-list 'display-buffer-alist '("Notes of" (window-width . 0.3)))
  (bind-key "C-c C-n" #'org-noter 'pdf-view-mode-map))

(with-eval-after-load 'org-noter-pdf
  (defun my/org-noter-pdf--clean-text (text)
    "Clean the text by removing word breaks"
    (replace-regexp-in-string (rx (or "-\n" (seq "\n" (not "-"))))
			      "" text))

  (defun my/org-noter-pdf--get-selected-text-clean (mode)
    "Clean the selected text, clean it."
    (when (and (eq mode 'pdf-view-mode)
	       (pdf-view-active-region-p))
      (mapconcat 'my/org-noter-pdf--clean-text
		 (pdf-view-active-region-text) ? )))

  (advice-add #'org-noter-pdf--get-selected-text :override
	      #'my/org-noter-pdf--get-selected-text-clean))

;; ** org-mind-map

(use-package org-mind-map
  :straight t
  :after ox-org
  :config
  (setq org-mind-map-engine "dot")       ; Default. Directed Graph
  ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
  ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
  ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
  ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
  ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
  ;; (setq org-mind-map-engine "circo")  ; Circular Layout
  )

;; ** org-ql

(use-package org-ql
  :straight t)

;; ** org-web-tools

(use-package org-web-tools
  :straight t)

;; ** org-vcard

(use-package org-vcard
  :disabled
  :straight t)

;; ** org-pdftools

(use-package org-pdftools
  :after org
  :straight t
  :hook (org-mode . org-pdftools-setup-link))

;; ** org-mime

(use-package org-mime
  :straight t
  :custom
  (org-mime-export-options
   '(:with-latex imagemagick
		 :section-numbers nil
		 :with-author nil
		 :with-toc nil))
  (org-mime-src--hint "")
  :bind
  (:map message-mode-map
        ("C-c M-o" . org-mime-htmlize)
	("C-c C-M-o" . org-mime-edit-mail-in-org-mode))
  :config
  (defun my/org-mime-html-styles ()
    "Set the html styles for the email."
    (org-mime-change-element-style
     "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                   "#E6E1DC" "#434444"))
    (org-mime-change-element-style
     "blockquote" "border-left: 2px solid gray; padding-left: 4px;"))

  (add-hook 'org-mime-html-hook #'my/org-mime-html-styles))

;; ** toc-org

(use-package toc-org
  :straight t)
;; ** orgit

(use-package orgit
  :straight t)

;; ** org-re-reveal
;; TODO set the org-re-reveal-root variable

(use-package org-re-reveal
  :straight t)

;; ** org-present org tree-slide

(use-package org-present
  :straight t)

(use-package org-tree-slide
  :straight t)

;; ** mixed-pitch-mode

(use-package mixed-pitch
  :straight t
  :disabled
  :hook
  ;; If you want it in all text modes:
  (text-mode . mixed-pitch-mode))

;; * LOCAL-VARIABLES

;; Local Variables:
;; outline-regexp: " *;; \\*+"
;; page-delimiter: " *;; \\**"
;; eval:(outline-minor-mode 1)
;; eval:(outline-hide-body)
;; coding: utf-8-unix
;; End:

(provide 'setup-org)
;;; setup-org.el ends here

