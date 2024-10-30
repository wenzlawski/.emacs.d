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

(use-package notmuch
  :straight t
  :hook
  (notmuch-mua-send . notmuch-mua-attachment-check)
  (notmuch-show . (lambda () (setq-local header-line-format nil)))
  :bind
  ("C-x m" . notmuch-mua-new-mail)
  (:map notmuch-message-mode-map
	("C-x C-s" . my/notmuch-draft-save))
  :custom
  (notmuch-identities '("Marc Wenzlawski <marcwenzlawski@posteo.com>" ;; "Marc Wenzlawski <marc.wenzlawski@icloud.com>"
			))
  (notmuch-fcc-dirs
   '(("marcwenzlawski@posteo.com" . "posteo/Sent -inbox +sent -unread +posteo")
     ;; ("marc.wenzlawski@icloud.com" . "icloud/Sent -inbox +sent -unread +icloud")
     ))
  (notmuch-command (executable-find "notmuch"))
  (notmuch-show-logo nil)
  (notmuch-column-control 1.0)
  (notmuch-hello-auto-refresh t)
  (notmuch-hello-recent-searches-max 20)
  (notmuch-hello-thousands-separator "")
  (notmuch-hello-sections '(notmuch-hello-insert-saved-searches))
  (notmuch-show-all-tags-list t)
  (notmuch-saved-searches
   '((:name "Flagged" :query "tag:flagged" :key "f")
     (:name "Inbox" :key "i"
	    :query "tag:inbox")
     (:name "Unread" :key "u"
            :query "tag:inbox AND tag:unread")
     (:name "Sent" :key "s"
            :query "tag:sent or tag:replied")
     (:name "Drafts" :query "tag:draft" :key "d")
     (:name "Archive" :key "a"
	    :query "NOT tag:inbox AND NOT tag:sent")
     (:name "Trash" :key "t"
            :query "tag:deleted")
     (:name "Posteo" :query "to:marcwenzlawski@posteo.com" :key "p")
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
  (notmuch-search-line-faces
   '(("unread" . notmuch-search-unread-face)
     ;; ;; NOTE 2022-09-19: I disable this because I add a cosmeic
     ;; ;; emoji via `notmuch-tag-formats'.  This way I do not get
     ;; ;; an intense style which is very distracting when I filter
     ;; ;; my mail to include this tag.
     ;;
     ;; ("flag" . notmuch-search-flagged-face)
     ;;
     ;; Using `italic' instead is just fine.  Though I also tried
     ;; it without any face and I was okay with it.  The upside of
     ;; having a face is that you can identify the message even
     ;; when the window is split and you don't see the tags.
     ("flag" . italic)))
  (notmuch-show-empty-saved-searches t)
  (notmuch-message-replied-tags '("+replied"))
  (notmuch-message-forwarded-tags '("+forwarded"))
  (notmuch-show-mark-read-tags '("-unread"))
  (notmuch-draft-tags '("+draft"))
  (notmuch-draft-folder "drafts")
  (notmuch-draft-save-plaintext 'ask)
  (notmuch-mua-compose-in 'current-window)
  (notmuch-mua-hidden-headers nil)
  (notmuch-address-command 'internal) ; NOTE 2024-01-09: I am not using this and must review it.
  (notmuch-always-prompt-for-sender nil)
  (notmuch-mua-cite-function 'message-cite-original-without-signature)
  (notmuch-mua-reply-insert-header-p-function 'notmuch-show-reply-insert-header-p-never)
  (notmuch-mua-user-agent-function nil)
  (notmuch-maildir-use-notmuch-insert t)
  (notmuch-crypto-process-mime t)
  (notmuch-crypto-get-keys-asynchronously t)
  (notmuch-mua-attachment-regexp   ; see `notmuch-mua-send-hook'
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

(provide 'setup-notmuch)
;;; setup-notmuch.el ends here
