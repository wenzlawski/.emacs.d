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

(provide 'custom-notmuch)
;;; custom-notmuch.el ends here
