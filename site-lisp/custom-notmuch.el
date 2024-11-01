;;; CUSTOM-NOTMUCH ---   -*- lexical-binding: t; -*-
;;
;; Author: Marc Wenzlawski <marcwenzlawski@posteo.com>
;; Copyright © 2024, Marc Wenzlawski, all rights reserved.
;; Created:  9 October 2024
;;
;;; Commentary:
;;
;;  
;;
;;; Code:

(require 'notmuch)
(require 'consult-notmuch)
(require 'khardel)

;;;###autoload
(defun my/notmuch-search-from-by-mail ()
  "Filter by from emails."
  (interactive)
  (let ((address (consult-notmuch--address-prompt)))
    (if (string-match "\<\\(.+\\)\>" address)
	(notmuch-search (concat "from:" (match-string 1 address))))))

;;;###autoload
(defun my/notmuch-search-from-by-contact ()
  "Filter by from emails."
  (interactive)
  (let* ((emails (khardel--list-emails))
	 (address (completing-read "Select email: " emails)))
    (if (string-match "\<\\(.+\\)\>" address)
	(notmuch-search (concat "from:" (match-string 1 address))))))

;;;###autoload
(defun my/notmuch-search-by-mail ()
  "Filter by email."
  (interactive)
  (notmuch-search (consult-notmuch--address-prompt)))

;;;###autoload
(defun my/notmuch-search-by-contact ()
  "Filter by contact."
  (interactive)
  (let* ((emails (khardel--list-emails))
	 (address (completing-read "Select email: " emails)))
    (notmuch-search address)))

;;;###autoload
(defun my/notmuch-show-mark-read (&optional unread beg end)
  "Mark the current message as read.

Mark the current message as read by applying the tag changes in
`notmuch-show-mark-read-tags' to it (remove the \"unread\" tag by
default). If a prefix argument is given, the message will be
marked as unread, i.e. the tag changes in
`notmuch-show-mark-read-tags' will be reversed."
  (interactive (cons current-prefix-arg (notmuch-interactive-region)))
  (if (not (or beg end))
      (apply 'notmuch-show-tag-message
	     (notmuch-tag-change-list notmuch-show-mark-read-tags unread))
    (when notmuch-show-mark-read-tags
      (notmuch-search-tag
       (notmuch-tag-change-list notmuch-show-mark-read-tags unread) beg end))
    (when (eq beg end)
      (notmuch-search-next-thread))))

(advice-add #'notmuch-show-mark-read :override #'my/notmuch-show-mark-read)

;;;###autoload
(defun my/notmuch-draft-save ()
  "Save the current draft message in the notmuch database.

This saves the current message in the database with tags
`notmuch-draft-tags' (in addition to any default tags
applied to newly inserted messages)."
  (interactive)
  (when (notmuch-draft--has-encryption-tag)
    (notmuch-draft--query-encryption))
  (let ((id (notmuch-draft--make-message-id)))
    (with-temporary-notmuch-message-buffer
     ;; We insert a Date header and a Message-ID header, the former
     ;; so that it is easier to search for the message, and the
     ;; latter so we have a way of accessing the saved message (for
     ;; example to delete it at a later time). We check that the
     ;; user has these in `message-deletable-headers' (the default)
     ;; as otherwise they are doing something strange and we
     ;; shouldn't interfere. Note, since we are doing this in a new
     ;; buffer we don't change the version in the compose buffer.
     (cond
      ((member 'Message-ID message-deletable-headers)
       (message-remove-header "Message-ID")
       (message-add-header (concat "Message-ID: <" id ">")))
      (t
       (message "You have customized emacs so Message-ID is not a %s"
		"deletable header, so not changing it")
       (setq id nil)))
     (cond
      ((member 'Date message-deletable-headers)
       (message-remove-header "Date")
       (message-add-header (concat "Date: " (message-make-date))))
      (t
       (message "You have customized emacs so Date is not a deletable %s"
		"header, so not changing it")))
     (message-add-header "X-Notmuch-Emacs-Draft: True")
     (notmuch-draft-quote-some-mml)
     (notmuch-maildir-setup-message-for-saving)
     (if-let ((message-fetch-field "from")
	      (draftf (alist-get (message-sendmail-envelope-from)
				 my/notmuch-draft-dirs nil nil #'string-equal)))
	 (progn
	   (notmuch-maildir-notmuch-insert-current-buffer
	    draftf t notmuch-draft-tags)
	   (message "Saved draft to %s" draftf))
       (notmuch-maildir-notmuch-insert-current-buffer
	notmuch-draft-folder t notmuch-draft-tags)
       (message "Saved draft to %s" notmuch-draft-folder)))
    ;; We are now back in the original compose buffer. Note the
    ;; function notmuch-call-notmuch-process (called by
    ;; notmuch-maildir-notmuch-insert-current-buffer) signals an error
    ;; on failure, so to get to this point it must have
    ;; succeeded. Also, notmuch-draft-id is still the id of the
    ;; previous draft, so it is safe to mark it deleted.
    (notmuch-draft--mark-deleted)
    (setq notmuch-draft-id (concat "id:" id))
    (set-buffer-modified-p nil)))

(defun my/notmuch-mua-reply (query-string &optional sender reply-all duplicate)
  (let* ((duparg (and duplicate (list (format "--duplicate=%d" duplicate))))
	 (args `("reply" "--format=sexp" "--format-version=5" ,@duparg))
	 (process-crypto notmuch-show-process-crypto)
	 reply
	 original)
    (when process-crypto
      (setq args (append args '("--decrypt=true"))))
    (if reply-all
	(setq args (append args '("--reply-to=all")))
      (setq args (append args '("--reply-to=sender"))))
    (setq args (append args (list query-string)))
    ;; Get the reply object as SEXP, and parse it into an elisp object.
    (setq reply (apply #'notmuch-call-notmuch-sexp args))
    ;; Extract the original message to simplify the following code.
    (setq original (plist-get reply :original))
    ;; Extract the headers of both the reply and the original message.
    (let* ((original-headers (plist-get original :headers))
	   (reply-headers (plist-get reply :reply-headers)))
      ;; If sender is non-nil, set the From: header to its value.
      (when sender
	(plist-put reply-headers :From sender))
      (let
	  ;; Overlay the composition window on that being used to read
	  ;; the original message.
	  ((same-window-regexps '("\\*mail .*")))
	;; We modify message-header-format-alist to get around
	;; a bug in message.el.  See the comment above on
	;; notmuch-mua-insert-references.
	(let ((message-header-format-alist
	       (cl-loop for pair in message-header-format-alist
			if (eq (car pair) 'References)
			collect (cons 'References
				      (apply-partially
				       'notmuch-mua-insert-references
				       (cdr pair)))
			else
			collect pair)))
	  (notmuch-mua-mail (plist-get reply-headers :To)
			    (notmuch-sanitize (plist-get reply-headers :Subject))
			    (notmuch-headers-plist-to-alist reply-headers)
			    nil (notmuch-mua-get-switch-function))))
      ;; Create a buffer-local queue for tag changes triggered when
      ;; sending the reply.
      (when notmuch-message-replied-tags
	(setq notmuch-message-queued-tag-changes
	      (list (cons query-string notmuch-message-replied-tags))))
      ;; Insert the message body - but put it in front of the signature
      ;; if one is present, and after any other content
      ;; message*setup-hooks may have added to the message body already.
      (save-restriction
	(message-goto-body)
	(narrow-to-region (point) (point-max))
	(goto-char (point-max))
	(newline 1) ;; put the cited message after the signature and body.
	;; (if (re-search-backward message-signature-separator nil t)
	;;     (when message-signature-insert-empty-line
	;;       (forward-line -1))
	;;   (goto-char (point-max)))
	)
      (let ((from (plist-get original-headers :From))
	    (date (plist-get original-headers :Date))
	    (start (point)))
	;; notmuch-mua-cite-function constructs a citation line based
	;; on the From and Date headers of the original message, which
	;; are assumed to be in the buffer.
	(insert "From: " from "\n")
	(insert "Date: " date "\n\n")
	(insert
	 (with-temp-buffer
	   (let
	       ;; Don't attempt to clean up messages, excerpt
	       ;; citations, etc. in the original message before
	       ;; quoting.
	       ((notmuch-show-insert-text/plain-hook nil)
		;; Don't omit long parts.
		(notmuch-show-max-text-part-size 0)
		;; Insert headers for parts as appropriate for replying.
		(notmuch-show-insert-header-p-function
		 notmuch-mua-reply-insert-header-p-function)
		;; Ensure that any encrypted parts are
		;; decrypted during the generation of the reply
		;; text.
		(notmuch-show-process-crypto process-crypto)
		;; Don't indent multipart sub-parts.
		(notmuch-show-indent-multipart nil)
		;; Stop certain mime types from being inlined
		(mm-inline-override-types (notmuch--inline-override-types)))
	     ;; We don't want sigstatus buttons (an information leak and usually wrong anyway).
	     (cl-letf (((symbol-function 'notmuch-crypto-insert-sigstatus-button) #'ignore)
		       ((symbol-function 'notmuch-crypto-insert-encstatus-button) #'ignore))
	       (notmuch-show-insert-body original (plist-get original :body) 0)
	       (buffer-substring-no-properties (point-min) (point-max))))))
	(set-mark (point))
	(goto-char start)
	;; Quote the original message according to the user's configured style.
	(funcall notmuch-mua-cite-function)
	))
    ;; Crypto processing based crypto content of the original message
    (when process-crypto
      (notmuch-mua-reply-crypto (plist-get original :body))))
  ;; Push mark right before signature, if any.
  (message-goto-signature)
  (unless (eobp)
    (end-of-line -1))
  (push-mark)
  (message-goto-body)
  (set-buffer-modified-p nil))

(advice-add #'notmuch-mua-reply :override #'my/notmuch-mua-reply)

(defcustom notmuch-show-include-all nil
  "Whether to pass --exclude=false to notmuch show."
  :type 'boolean
  :group 'notmuch-show)

;; modified:
;; - exclude=true
;; - remove the queries form build-queries

(defun my/notmuch-show--build-buffer (&optional state)
  "Display messages matching the current buffer context.

Apply the previously saved STATE if supplied, otherwise show the
first relevant message.

If no messages match the query return NIL."
  (let* ((cli-args (list "--exclude=true"))
	 (cli-args (if notmuch-show-elide-non-matching-messages (cons "--entire-thread=false" cli-args) cli-args))
	 ;; "part 0 is the whole message (headers and body)" notmuch-show(1)
	 (cli-args (if notmuch-show-single-message (cons "--part=0" cli-args) cli-args))
	 (queries (notmuch-show--build-queries
		   notmuch-show-thread-id nil))
	 (forest nil)
	 ;; Must be reset every time we are going to start inserting
	 ;; messages into the buffer.
	 (notmuch-show-previous-subject ""))
    ;; Use results from the first query that returns some.
    (while (and (not forest) queries)
      (setq forest (notmuch--run-show
		    (append cli-args (list "'") (car queries) (list "'"))))
      (when (and forest notmuch-show-single-message)
	(setq forest (list (list (list forest)))))
      (setq queries (cdr queries)))
    (when forest
      (notmuch-show-insert-forest forest)
      ;; Store the original tags for each message so that we can
      ;; display changes.
      (notmuch-show-mapc
       (lambda () (notmuch-show-set-prop :orig-tags (notmuch-show-get-tags))))
      (setq header-line-format (notmuch-show--header-line-format))
      (run-hooks 'notmuch-show-hook)
      (if state
	  (notmuch-show-apply-state state)
	;; With no state to apply, just go to the first message.
	(notmuch-show-goto-first-wanted-message)))
    ;; Report back to the caller whether any messages matched.
    forest))

;; (advice-add #'notmuch-show--build-buffer :override #'my/notmuch-show--build-buffer)

(defface notmuch-message-deleted-summary-face
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "#5c5c5c")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "#b6b6b6"))
  "Face for the single-line message summary in notmuch-show-mode."
  :group 'notmuch-show
  :group 'notmuch-faces)


(defun my/notmuch-show-insert-headerline (msg-plist depth tags &optional orig-tags)
  "Insert a notmuch style headerline based on HEADERS for a
message at DEPTH in the current thread."
  (let* ((start (point))
	 (headers (plist-get msg-plist :headers))
	 (duplicate (or (plist-get msg-plist :duplicate) 0))
	 (file-count (length (plist-get msg-plist :filename)))
	 (date (or (and notmuch-show-relative-dates
			(plist-get msg-plist :date_relative))
		   (plist-get headers :Date)))
	 (from (notmuch-sanitize
		(notmuch-show-clean-address (plist-get headers :From)))))
    (when (string-match "\\cR" from)
      ;; If the From header has a right-to-left character add
      ;; invisible U+200E LEFT-TO-RIGHT MARK character which forces
      ;; the header paragraph as left-to-right text.
      (insert (propertize (string ?\x200e) 'invisible t)))
    (insert (if notmuch-show-indent-content
		(notmuch-show-spaces-n (* notmuch-show-indent-messages-width
					  depth))
	      "")
	    from
	    " ("
	    date
	    ") ("
	    (notmuch-tag-format-tags tags (or orig-tags tags))
	    ")")
    (insert
     (if (> file-count 1)
	 (let ((txt (format "%d/%d\n" duplicate file-count)))
	   (concat
	    (notmuch-show-spaces-n (max 0 (- (window-width) (+ (current-column) (length txt)))))
	    txt))
       "\n"))
    (let ((face (if (member "deleted" tags) 'notmuch-message-deleted-summary-face
		  'notmuch-message-summary-face)))
      (overlay-put (make-overlay start (point))
		   'face face))))

(advice-add #'notmuch-show-insert-headerline :override #'my/notmuch-show-insert-headerline)

(provide 'custom-notmuch)
;;; custom-notmuch.el ends here
