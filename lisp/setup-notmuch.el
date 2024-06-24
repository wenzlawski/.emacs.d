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

(use-package notmuch
  :straight t
  :hook
  (notmuch-mua-send . notmuch-mua-attachment-check)
  (notmuch-show . (lambda () (setq-local header-line-format nil)))
  :custom
  (notmuch-show-logo nil)
  (notmuch-column-control 1.0)
  (notmuch-hello-auto-refresh t)
  (notmuch-hello-recent-searches-max 20)
  (notmuch-hello-thousands-separator "")
  (notmuch-hello-sections '(notmuch-hello-insert-saved-searches))
  (notmuch-show-all-tags-list t)
  (notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "archive" :query "tag:archive" :key "r")
     (:name "sent" :query "tag:sent" :key "s")
     (:name "icloud" :query "to:marc.wenzlawski@icloud.com" :key "c")
     (:name "posteo" :query "to:marcwenzlawski@posteo.com" :key "p")
     (:name "all" :query "*" :key "a"))
   )
  (notmuch-search-oldest-first nil)
  (notmuch-search-result-format
   '(("date" . "%12s  ")
     ("count" . "%-7s  ")
     ("authors" . "%-20s  ")
     ("subject" . "%-80s  ")
     ("tags" . "(%s)")))
  (notmuch-tree-result-format
   '(("date" . "%12s  ")
     ("authors" . "%-20s  ")
     ((("tree" . "%s")
       ("subject" . "%s"))
      . " %-80s  ")
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
  (notmuch-archive-tags nil) ; I do not archive email
  (notmuch-message-replied-tags '("+replied"))
  (notmuch-message-forwarded-tags '("+forwarded"))
  (notmuch-show-mark-read-tags '("-unread"))
  (notmuch-draft-tags '("+draft"))
  (notmuch-draft-folder "drafts")
  (notmuch-draft-save-plaintext 'ask)
  (notmuch-mua-compose-in 'current-window)
  (notmuch-mua-hidden-headers nil)
  (notmuch-address-command 'internal) ; NOTE 2024-01-09: I am not using this and must review it.
  (notmuch-always-prompt-for-sender t)
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

  :bind
  (:map notmuch-show-mode-map
	("a" . (lambda ()
		 "archive message"
		 (interactive)
		 (notmuch-show-tag (list "+archive" "-inbox" "-unread")))))
  :config
  (let ((count most-positive-fixnum)) ; I don't like the buttonisation of long quotes
    (setq notmuch-wash-citation-lines-prefix count
          notmuch-wash-citation-lines-suffix count)))

(provide 'setup-notmuch)
;;; setup-notmuch.el ends here
