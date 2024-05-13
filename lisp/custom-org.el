;; -*- lexical-binding: t; -*-

(defun my/compose-letter nil
  "compose the job application letter
Get the properties of the current entry, encode them as JSON, and
pass them to the justfile to generate the letter. The letter is
then opened in a new buffer."
  (interactive)
  (let ((props (org-entry-properties))
	 (tmp ".tmp.json"))

    ;; check for an id property, if no create one
    ;; this is important for the file name
    (if (not (cdr (assoc "ID" props)))
	(progn (org-id-get-create) (setq props (org-entry-properties))))
    (setq file (format "%s.pdf" (cdr (assoc "ID" props))))

    ;; save the props to a temp file
    (with-temp-file (format "cv/%s" tmp)
      (insert (json-encode props)))

    ;; execute the shell command
    (shell-command (format "just c-letter-t \"%s\" \"%s\"" tmp file) "*test*" "*test*")

    ;; open the pdf file
    (let ((buffer (find-buffer-visiting
		   (expand-file-name file "letters"))))
      (if buffer
	  (switch-to-buffer-other-window buffer) ; if already open, switch to it
	(find-file-other-window (expand-file-name file "letters")))) ; otherwise open it
    ))

(bind-key "C-c g" #'my/compose-letter 'org-mode-map)

(provide 'custom-org)
