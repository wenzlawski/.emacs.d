;; setup-calibre.el --- calibre setup  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun my/refresh-calibre-bib ()
  "Refresh the calibre bib file."
  (interactive)
  (shell-command "calibredb catalog /tmp/cat.bib --fields=title,authors,formats,id,isbn,pubdate,tags,uuid,identifiers --entry-type mixed" )
  (shell-command (format "awk -f %s /tmp/cat.bib > ~/Calibre\\ Library/calibre.bib" (dir-concat user-emacs-directory "scripts/escape_comma.awk"))))

(use-package calibredb
  :straight t)

(use-package calibredb-search
  :after calibredb
  :bind
  (:map calibredb-search-mode-map
	("f" . calibredb-add-format)
	("C" . calibredb-search-refresh-and-clear-filter)
	("r" . calibredb-search-refresh)
	("F" . calibredb-search-next-page)
	("B" . calibredb-search-previous-page)))

(setopt calibredb-root-dir "~/Calibre Library"
	calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)
	calibredb-id-width 5
	calibredb-title-width 55
	calibredb-comment-width 0
	calibredb-tag-width 1
	calibredb-search-page-max-rows (- (frame-height) 5)
	calibredb-preferred-format "pdf"
	calibredb-library-alist '(("~/Calibre Library")))

(defun calibredb-set-results-rows (&rest _)
  "Set the number of rows for a calibre search."
  (setopt calibredb-search-page-max-rows (- (frame-height) 4)))

(advice-add #'calibredb-search-refresh-and-clear-filter :before #'calibredb-set-results-rows)
(advice-add #'calibredb-search-refresh :before #'calibredb-set-results-rows)

(defcustom calibredb-show-items '(id title format date author tag)
  "Items to show in the Calibre search view in order."
  :type '(list symbol)
  :group 'calibredb)

(defmacro calibredb--format-id (id)
  "Format the `ID'."
  `(format "%s" (propertize id 'face 'calibredb-id-face 'id id)))

(defmacro calibredb--format-title (title)
  "Format the `TITLE'."
  `(format "%s%s"
	   (if (s-contains? calibredb-favorite-keyword tag)
	       (format "%s " (propertize calibredb-favorite-icon
					 'face 'calibredb-favorite-face
					 'mouse-face 'calibredb-mouse-face
					 'help-echo "Filter the favorite items"
					 'keymap (define-key (make-sparse-keymap) [mouse-1] 'calibredb-tag-mouse-1))) "")
	   (cond
	    ((s-contains? calibredb-archive-keyword tag)
	     (propertize title 'face 'calibredb-archive-face))
	    ((s-contains? calibredb-highlight-keyword tag)
	     (propertize title 'face 'calibredb-highlight-face))
	    (t
	     (propertize title 'face (calibredb-title-face))))))

(defmacro calibredb--format-format (format)
  "Format the `FORMAT'."
  `(propertize format
	       'face 'calibredb-format-face
	       'mouse-face 'calibredb-mouse-face
	       'help-echo "Filter with this format"
	       'keymap (define-key (make-sparse-keymap) [mouse-1] 'calibredb-format-mouse-1)))

(defmacro calibredb--format-date (date)
  "Format the `DATE'."
  `(propertize (s-left 10 date) 'face 'calibredb-date-face ; only keep YYYY-MM-DD
	       'mouse-face 'calibredb-mouse-face
	       'help-echo "Filter with this date"
	       'keymap (define-key (make-sparse-keymap) [mouse-1] 'calibredb-date-mouse-1)))

(defmacro calibredb--format-author (author)
  "Format the `AUTHOR'."
  `(mapconcat
    (lambda (author)
      (propertize author
		  'author author
		  'face 'calibredb-author-face
		  'mouse-face 'calibredb-mouse-face
		  'help-echo (format "Filter with this author: %s" author)
		  'keymap (define-key (make-sparse-keymap) [mouse-1] 'calibredb-author-mouse-1)))
    (split-string author "&" t "\s+") " & "))

(defmacro calibredb--format-tag (tag)
  "Format the `TAG' with `TAG-MAP'."
  `(mapconcat
    (lambda (tag)
      (propertize tag
		  'tag tag
		  'face 'calibredb-tag-face
		  'mouse-face 'calibredb-mouse-face
		  'help-echo (format "Filter with this tag: %s" tag)
		  'keymap (define-key (make-sparse-keymap) [mouse-1] 'calibredb-tag-mouse-1)))
    (split-string tag ",") ","))

(defmacro calibredb--build-format (item)
  "Build the format-column call for `ITEM'."
  (let* ((formatsym (intern (concat "calibredb--format-" (symbol-name item))))
	 (widthsym (intern (concat "calibredb-" (symbol-name item) "-width"))))
    `(calibredb-format-column
      ,(list formatsym item)
      ,(cond ((functionp widthsym) `(,widthsym))
	     ((symbolp widthsym) widthsym))
      :left)))

;; TODO: Can't fucking figure it out
;; Everything else works fucking flawlessly but this shit does not do what it is supposed to...
(defmacro calibredb--format-build-all ()
  "Build all formats")

;;; Fix the author display

(with-eval-after-load 'calibredb
  (defun calibredb-all-author-sort nil "Get all author-sort and return as a list."
	 (seq-uniq
	  (let (l)
	    (let*
		((--cl-var-- calibredb-full-entries)
		 (entry nil))
	      (while
		  (consp --cl-var--)
		(setq entry
		      (car --cl-var--))
		(setq l
		      (append
		       (split-string
			(calibredb-getattr
			 (cdr entry)
			 :author-sort)
			"&" t "\s+")
		       l))
		(setq --cl-var--
		      (cdr --cl-var--)))
	      nil)
	    l)))

  (defun calibredb-all-author_sort ()
    "Get all author_sort and return as a list."
    (seq-uniq
     (let (l)
       (cl-loop for item in (calibredb-candidates :distinct "author_sort") do
                (if (listp item)
                    (if (car item) (setq l (append (split-string (car item ) " & ") l)) "" )
                  (setq l (append (split-string item " & ") l)))) l)))

  (defun calibredb-format-item (book-alist)
    "Format the candidate string shown in helm or ivy.
Argument BOOK-ALIST ."
    (let ((id (calibredb-getattr (list book-alist) :id))
	  (title (calibredb-getattr (list book-alist) :book-title))
	  (format (calibredb-getattr (list book-alist) :book-format))
	  (author (calibredb-getattr (list book-alist) :author-sort))
	  (tag (calibredb-getattr (list book-alist) :tag))
	  (comment (calibredb-getattr (list book-alist) :comment))
	  (size (calibredb-getattr (list book-alist) :size))
	  (ids (calibredb-getattr (list book-alist) :ids))
	  (date (calibredb-getattr (list book-alist) :last_modified))
	  (favorite-map (make-sparse-keymap)))
      (define-key favorite-map [mouse-1] 'calibredb-tag-mouse-1)
      (if calibredb-detailed-view
	  (setq title (concat title "\n")))
      (apply #'format
	     (if calibredb-detailed-view
		 (let ((num (cond (calibredb-format-all-the-icons 3)
				  (calibredb-format-icons-in-terminal 3)
				  ((>= calibredb-id-width 0) calibredb-id-width)
				  (t 0 ))))
		   (concat
		    "%s%s%s"
		    (calibredb-format-column (format "%sFormat:" (make-string num ? )) (+ 8 num) :left) "%s\n"
		    (calibredb-format-column (format "%sDate:" (make-string num ? )) (+ 8 num) :left) "%s\n"
		    (calibredb-format-column (format "%sAuthor:" (make-string num ? ))  (+ 8 num) :left) "%s\n"
		    (calibredb-format-column (format "%sTag:" (make-string num ? )) (+ 8 num) :left) "%s\n"
		    (calibredb-format-column (format "%sIds:" (make-string num ? )) (+ 8 num) :left) "%s\n"
		    (calibredb-format-column (format "%sComment:" (make-string num ? )) (+ 8 num) :left) "%s\n"
		    (calibredb-format-column (format "%sSize:" (make-string num ? )) (+ 8 num) :left) "%s"))
	       "%s%s%s %s %s %s (%s) %s %s %s")
	     `(,(cond (calibredb-format-all-the-icons
		       (concat (if (fboundp 'all-the-icons-icon-for-file)
				   (all-the-icons-icon-for-file (calibredb-get-file-path (list book-alist))) "")
			       " "))
		      (calibredb-format-icons-in-terminal
		       (concat (if (fboundp 'icons-in-terminal-icon-for-file)
				   (icons-in-terminal-icon-for-file (calibredb-get-file-path (list book-alist) ) :v-adjust 0 :height 1) "")
			       " "))
		      (calibredb-format-character-icons
		       (concat (calibredb-attach-icon-for (calibredb-get-file-path (list book-alist))) " "))
		      (t ""))

	       ,(calibredb--build-format id)
	       ,(calibredb--build-format title)
	       ,(calibredb--build-format format)
	       ,(calibredb--build-format date)
	       ,(calibredb--build-format author)
	       ,(calibredb--build-format tag)

	       ,(calibredb-format-column (propertize ids 'face 'calibredb-ids-face) (calibredb-ids-width) :left)
	       ,(if (stringp comment)
		    (propertize
		     (let ((c (if calibredb-condense-comments (calibredb-condense-comments comment) comment))
			   (w calibredb-comment-width))
		       (cond ((> w 0) (s-truncate w c))
			     ((= w 0) "")
			     (t c)))
		     'face 'calibredb-comment-face) "")
	       ,(format "%s%s"
			(if calibredb-size-show
			    (propertize size 'face 'calibredb-size-face) "")
			(if calibredb-size-show
			    (propertize "Mb" 'face 'calibredb-size-face) "")))) )))

(provide 'setup-calibre)
;;; setup-calibre.el ends here
