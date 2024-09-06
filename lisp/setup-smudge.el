;;; setup-smudge.el --- Setup smudge.el -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defvar my/smudge-playlists
  '(("ot" minimal-techno "spotify:playlist:59KuQSm27IfRylpXxz9KrM")
    ("oy" yoga-electronica "spotify:playlist:37i9dQZF1DWYUYYlhkTuEn")))

(use-package smudge
  :straight t
  :commands (smudge-controller-apply
	     smudge-controller-previous-track
	     smudge-controller-next-track
	     smudge-track-search
	     smudge-controller-toggle-play
	     smudge-controller-toggle-repeat 
	     smudge-controller-toggle-shuffle
	     smudge-select-device
	     smudge-controller-volume-up
	     smudge-controller-volume-down
	     smudge-controller-mute-unmute
	     smudge-create-playlist
	     smudge-featured-playlists
	     smudge-playlist-search
	     smudge-my-playlists
	     smudge-user-playlists)
  :custom
  (smudge-player-use-transient-map t)
  (smudge-player-status-truncate-length nil)
  :config
  (let* ((host (car (auth-source-search :host "smudge")))
	 (secret (plist-get host :csecret))
	 (client (plist-get host :client)))
    (setq smudge-oauth2-client-secret secret
          smudge-oauth2-client-id client)))

(defun my/smudge-play-playlist (uri)
  "Play a playlist."
  (smudge-controller-apply "player-play-track" nil uri))

(with-eval-after-load 'smudge
;;; Display playlist followers
  (defun my/smudge-api-playlist-followers (playlist callback)
    "Call CALLBACK with PAGE of results of followers from PLAYLIST."
    (smudge-api-call-async
     "GET"
     (concat (format "/playlists/%s?"
		     (url-hexify-string (smudge-api-get-item-id playlist)))
	     (url-build-query-string `((fields "followers.total"))
				     nil t))
     nil
     callback)
    )

  (defun my/smudge-api-get-playlist-follower-count (json)
    "Return the number of followers of the given playlist JSON object."
    (gethash 'total (gethash 'followers json)))

  (defun my/smudge-playlist-all-followers ()
    "Get the followers of all playlists."
    (interactive)
    (cl-loop for entry in tabulated-list-entries
	     do (let ((playlist (car entry))
		      (entry (cadr entry)))
		  (my/smudge-api-playlist-followers
		   playlist
		   (lambda (data) (let ((followers (my/smudge-api-get-playlist-follower-count data)))
				    (message "Followers: %s" followers)
				    (aset entry 3 (number-to-string followers))
				    (puthash 'total followers (gethash 'followers playlist))
				    ))))))

  (defun my/smudge-playlist-get-followers ()
    "Get the followers of the current playlist."
    (interactive)
    (let* ((selected-playlist (tabulated-list-get-id))
	   (selected-entry (tabulated-list-get-entry))
	   (playlist-name (smudge-api-get-item-name selected-playlist))
	   (user-id (smudge-api-get-playlist-owner-id selected-playlist)))
      (my/smudge-api-playlist-followers
       selected-playlist
       (lambda (data)
	 (let ((followers (my/smudge-api-get-playlist-follower-count data)))
	   (aset selected-entry 3 (number-to-string followers))
	   (puthash 'total followers (gethash 'followers selected-playlist))
	   (tabulated-list-print t t)
	   )))))

  (bind-key "c" #'my/smudge-playlist-get-followers smudge-playlist-search-mode-map)
  (bind-key "C" #'my/smudge-playlist-all-followers smudge-playlist-search-mode-map)
  (bind-key "r" #'tabulated-list-revert smudge-playlist-search-mode-map)

  ;; misc setup for the above to work
  ;; here we have to add the followers column to the format
  (defun my/smudge-playlist-set-list-format ()
    "Configures the column data for the typical playlist view."
    (setq tabulated-list-format
          (vector `("Playlist Name" ,(- (window-width) 55) t)
                  '("Owner Id" 30 t)
                  '("# Tracks" 8 (lambda (row-1 row-2)
                                   (< (smudge-api-get-playlist-track-count (car row-1))
                                      (smudge-api-get-playlist-track-count (car row-2)))) :right-align t)
		  '("# Followers" 8 (lambda (row-1 row-2)
				      (< (my/smudge-api-get-playlist-follower-count (car row-1))
					 (my/smudge-api-get-playlist-follower-count (car row-2)))) :right-align t))
	  tabulated-list-padding 0))

  ;; and override the default format
  (advice-add 'smudge-playlist-set-list-format :override #'my/smudge-playlist-set-list-format)

  ;; we have to initialize followers hash table
  (defun my/smudge-playlist-search-print (playlists page)
    "Append PLAYLISTS to PAGE of the current playlist view."
    (let (entries)
      (dolist (playlist playlists)
	(let ((user-id (smudge-api-get-playlist-owner-id playlist))
              (playlist-name (smudge-api-get-item-name playlist))
	      (total (make-hash-table :test 'equal)))
	  ;; here we add the followers count to the hash table
	  (puthash 'total 0 total)
	  (puthash 'followers total playlist)
          (push (list playlist
                      (vector (cons playlist-name
                                    (list 'face 'link
                                          'follow-link t
                                          'action `(lambda (_) (smudge-playlist-tracks))
                                          'help-echo (format "Show %s's tracks" playlist-name)))
                              (cons user-id
                                    (list 'face 'link
                                          'follow-link t
                                          'action `(lambda (_) (smudge-user-playlists ,user-id))
                                          'help-echo (format "Show %s's public playlists" user-id)))
                              (number-to-string (smudge-api-get-playlist-track-count playlist))
			      ;; here we add an empty string for the followers column
			      "‏‏‎ ‎"))
		entries)))
      (when (eq 1 page) (setq-local tabulated-list-entries nil))
      (smudge-playlist-set-list-format)
      (setq-local tabulated-list-entries (append tabulated-list-entries (nreverse entries)))
      (tabulated-list-init-header)
      (tabulated-list-print t)))

  ;; and override the default function
  (advice-add 'smudge-playlist-search-print :override #'my/smudge-playlist-search-print)

;;; Format playlist titles

  ;; it goes from "THIS IS A [[PLAYLIST]] ♡" to "This Is A Playlist"
  ;; also for tracks and albums.
  (defun my/smudge-api-format-title (title)
    "Format TITLE for display."
    (s-trim (s-replace-regexp "[^[:ascii:]]" "" (s-titleized-words title))))

  (advice-add 'smudge-api-get-item-name :filter-return #'my/smudge-api-format-title)

;;; Copy to clipboard

  ;; here we get the open.spotify uri
  (defun my/smudge-api-get-item-uri (item)
    "Return the URI of the given ITEM."
    (gethash 'spotify (gethash 'external_urls item)))
  
  (defun my/smudge-copy-item-uri ()
    "Copy the URI of the current item to the clipboard."
    (interactive)
    (let* ((item (tabulated-list-get-id))
	   (uri (my/smudge-api-get-item-uri item)))
      (kill-new uri)
      (message "Copied %s to clipboard" uri)))

  (bind-key "w" #'my/smudge-copy-item-uri smudge-playlist-search-mode-map)
  (bind-key "w" #'my/smudge-copy-item-uri smudge-track-search-mode-map)

;;; Play local

  ;; this uses the spotify:album:xxx format
  (defun my/smudge-play-local ()
    "Play the current track."
    (interactive)
    (let* ((item (tabulated-list-get-id))
	   (uri (smudge-api-get-item-uri item)))
      (ns-do-applescript (format "tell application \"Spotify\" to play track \"%s\"" uri))))

  (bind-key "s" #'my/smudge-play-local smudge-playlist-search-mode-map)
  (bind-key "s" #'my/smudge-play-local smudge-track-search-mode-map))


(provide 'setup-smudge)
;;; setup-smudge.el ends here
