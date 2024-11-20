;;; SETUP-NOTMUCH --- setting up notmuch email  -*- lexical-binding: t; -*-
;;
;; Author: Marc Wenzlawski <marc.wenzlawski@icloud.com>
;; Copyright © 2024, Marc Wenzlawski, all rights reserved.
;; Created: 24 June 2024
;;
;;; Commentary:
;;
;;  
;;
;;; Code:

;; for sever sive https://github.com/briansniffen/muchsync

(defvar my/notmuch-draft-dirs
  '(("marcwenzlawski@posteo.com" . "posteo/Drafts")))

(defun my/notmuch-async-poll ()
  "Async notmuch new."
  (interactive)
  (start-process "notmuch-poll" nil notmuch-command "new"))

(use-package notmuch
  :straight t
  :hook
  (notmuch-mua-send . notmuch-mua-attachment-check)
  (notmuch-show . (lambda () (setq-local header-line-format nil)))
  (notmuch-hello-mode . my/notmuch-async-poll)
  :bind
  ("C-x m" . notmuch-mua-new-mail)
  (:map notmuch-message-mode-map
	("C-x C-s" . my/notmuch-draft-save))
  (:map notmuch-search-mode-map
	("." . notmuch-show-mark-read))
  :custom
  (notmuch-identities '("Marc Wenzlawski <marcwenzlawski@posteo.com>"
			"Marc Wenzlawski <marc.wenzlawski@icloud.com>"
			))
  (notmuch-fcc-dirs
   '(("marcwenzlawski@posteo.com" . "posteo/Sent -inbox +sent -unread +posteo")
     ("marc.wenzlawski@icloud.com" . "icloud/Sent -inbox +sent -unread +icloud")
     ))
  (notmuch-command (executable-find "notmuch"))
  (notmuch-show-logo nil)
  (notmuch-column-control 1.0)
  (notmuch-hello-auto-refresh t)
  (notmuch-hello-recent-searches-max 20)
  (notmuch-hello-thousands-separator "")
  (notmuch-hello-sections '(notmuch-hello-insert-saved-searches))
  (notmuch-show-all-tags-list t)
  (notmuch-saved-search-buffer-name-format "*%t %s*")
  (notmuch-saved-searches
   '((:name "Flagged" :query "tag:flagged" :key "f")
     (:name "Inbox" :key "i" :query "tag:inbox AND NOT tag:archive")
     (:name "Unread" :key "u" :query "tag:inbox AND tag:unread")
     (:name "Drafts" :query "tag:draft" :key "d")
     (:name "Queue" :key "q" :query "tag:queue")
     (:name "Sent" :key "s" :query "(tag:sent OR tag:replied) AND NOT tag:archive")
     (:name "Archive" :key "a" :query "tag:archive")
     (:name "Trash" :key "t" :query "tag:deleted")
     (:name "Posteo" :query "to:marcwenzlawski.+@posteo.com" :key "P")
     (:name "icloud" :query "to:@privaterelay.appleid.com OR to:marc.wenzlawski.+@icloud.com" :key "G")
     (:name "All mail" :query "*" :key "A"))
   )
  (notmuch-search-oldest-first nil)
  (notmuch-search-result-format
   '(("date" . "%12s  ")
     ("count" . "%-7s  ")
     ("authors" . "%-20.20s  ")
     ("subject" . "%-70.70s  ")
     ("tags" . "(%s)")))
  (notmuch-tree-result-format
   '(("date" . "%12s  ")
     ("authors" . "%-20.20s  ")
     ((("tree" . "%s")
       ("subject" . "%s"))
      . " %-70.70s  ")
     ("tags" . "(%s)")))
  (notmuch-show-mark-read-tags '("-unread"))
  (notmuch-archive-tags '("-inbox" "+archive"))
  (notmuch-tagging-keys
   '(("a" notmuch-archive-tags "Archive")
     ("u" notmuch-show-mark-read-tags "Mark read")
     ("f" ("+flagged") "Flag")
     ("s" ("+spam" "-inbox") "Mark as spam")
     ("d" ("+deleted") "Delete")))
  (notmuch-search-line-faces
   '(("unread" . notmuch-search-unread-face)
     ("flag" . italic)))
  (notmuch-show-empty-saved-searches t)
  (notmuch-message-replied-tags '("+replied"))
  (notmuch-message-forwarded-tags '("+forwarded"))
  (notmuch-draft-tags '("+draft"))
  (notmuch-draft-folder "draft")
  (notmuch-show-only-matching-messages nil)
  (notmuch-draft-save-plaintext 'ask)
  (notmuch-mua-compose-in 'current-window)
  (notmuch-message-queued-tag-changes '(("tag:drafts" "+sent" "-drafts")))
  (notmuch-mua-hidden-headers nil)
  (notmuch-address-command 'internal) ; NOTE 2024-01-09: I am not using this and must review it.
  (notmuch-always-prompt-for-sender nil)
  (notmuch-mua-cite-function 'message-cite-original-without-signature)
  (notmuch-mua-reply-insert-header-p-function 'notmuch-show-reply-insert-header-p-never)
  (notmuch-mua-user-agent-function nil)
  (notmuch-maildir-use-notmuch-insert t)
  (notmuch-crypto-process-mime t)
  (notmuch-crypto-get-keys-asynchronously t)
  (notmuch-mua-attachment-regexp   ; TODO: german?
   (concat "\\b\\(attache\?ment\\|attached\\|attach\\|"
           "pi[èe]ce\s+jointe?\\|"
           "συνημμ[εέ]νο\\|επισυν[αά]πτω\\)\\b"))
  (notmuch-show-relative-dates t)
  (notmuch-show-all-multipart/alternative-parts nil)
  (notmuch-show-indent-messages-width 0)
  (notmuch-show-indent-multipart nil)
  (notmuch-show-part-button-default-action 'notmuch-show-view-part)
  (notmuch-show-text/html-blocked-images ".") ; block everything
  (notmuch-wash-wrap-lines-length 120)
  (notmuch-unthreaded-show-out nil)
  (notmuch-message-headers '("To" "Cc" "Subject" "Date"))
  (notmuch-message-headers-visible t)
  :config
  (require 'custom-notmuch)

  (let ((count most-positive-fixnum)) ; I don't like the buttonisation of long quotes
    (setq notmuch-wash-citation-lines-prefix count
          notmuch-wash-citation-lines-suffix count))
  (advice-add #'notmuch-draft-save :after #'my/notmuch-draft-save-message))

(defun my/notmuch-draft-save-message (&rest _)
  "Save draft message."
  (message "Saved draft."))

(defun my/notmuch-async-poll ()
  "Async notmuch new."
  (interactive)
  (start-process "notmuch-poll" nil notmuch-command "new"))

(use-package ol-notmuch
  :straight t
  :after (org notmuch))

(use-package notmuch-addr
  :straight t
  :after notmuch-address
  :config
  (notmuch-addr-setup))

(use-package notmuch-indicator
  :straight t
  :custom
  (notmuch-indicator-args
   '((:terms "tag:unread AND tag:inbox" :label "💬")
     (:terms "tag:flagged" :label "‼")))
  (notmuch-indicator-refresh-count (* 60 1))
  (notmuch-indicator-hide-empty-counters t)
  (notmuch-indicator-notmuch-config-file "~/.config/notmuch/default/config")
  :config
  (defun notmuch-indicator--shell-command (terms)
    "Run shell command for `notmuch-count(1)' with TERMS."
    (replace-regexp-in-string
     "\n" ""
     (let ((default-directory "~"))
       (shell-command-to-string
	(format "%s count %s"
		notmuch-indicator-notmuch-binary
		(shell-quote-argument terms))))))

  (defun notmuch-indicator-refresh (&rest other)
    "Refresh the active indicator."
    (when (notmuch-indicator--running-p)
      (cancel-function-timers #'notmuch-indicator--indicator)
      (run-at-time nil notmuch-indicator-refresh-count #'notmuch-indicator--indicator)))

  (notmuch-indicator-mode 1))

(provide 'setup-notmuch)
;;; setup-notmuch.el ends here
