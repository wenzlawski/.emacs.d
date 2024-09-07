;;; custom-org.el --- Custom org functions
;;; Commentary:
;;; Code:

(require 'ov)

(defun my/org-get-subtree-contents ()
  "Get the contents of the subtree at point."
  (if (org-before-first-heading-p)
      (message "Not in or on an org heading")
    (save-excursion
      ;; If inside heading contents, move the point back to the heading
      ;; otherwise `org-agenda-get-some-entry-text' won't work.
      (unless (org-on-heading-p) (org-previous-visible-heading 1))
      (let ((contents (substring-no-properties
                       (org-agenda-get-some-entry-text
                        (point-marker)
                        most-positive-fixnum))))
	contents))))

;;;###autoload
(defun my/org-copy-subtree-contents ()
  "Get the content text of the subtree at point and add it to the `kill-ring'.
Excludes the heading and any child subtrees."
  (interactive)
  (let ((contents (my/org-get-subtree-contents)))
    (message "Copied: %s" contents)
    (kill-new contents)))

;;;###autoload
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
    (push (list "CONTENT" (my/org-get-subtree-contents)) props)

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

;; ** org open other window

;;;###autoload
(defun my/org-open-at-point-other-window ()
  "Open org link at point in other window."
  (interactive)
  (let ((org-link-frame-setup (append '((file . find-file-other-window)) org-link-frame-setup))
	(fill-col-p visual-fill-column-mode)
	(prev-buf (current-buffer)))
    (visual-fill-column-mode -1)
    (org-open-at-point)
    (if fill-col-p
	(with-current-buffer prev-buf
	  (visual-fill-column-mode fill-col-p)))))

;;;###autoload
(defun my/org-open-at-point-other-frame ()
  "Open org link at point in other frame."
  (interactive)
  (let ((org-link-frame-setup (append '((file . find-file-other-frame)) org-link-frame-setup)))
    (org-open-at-point)))


;; ** org outline numbers

;;;###autoload
(defun my/org-outline-numbers (&optional remove-p)
  "Add outline number overlays to the current buffer.
When REMOVE-P is non-nil (interactively, with prefix), remove
them.  Overlays are not automatically updated when the outline
structure changes."
  ;; NOTE: This does not necessarily play nicely with org-indent-mode
  ;; or org-bullets, but it probably wouldn't be too hard to fix that.
  (interactive (list current-prefix-arg))
  (cl-labels ((heading-number ()
		(or (when-let ((num (previous-sibling-number)))
                      (1+ num))
                    1))
              (previous-sibling-number ()
		(save-excursion
                  (let ((pos (point)))
                    (org-backward-heading-same-level 1)
                    (when (/= pos (point))
                      (heading-number)))))
              (number-list ()
		(let ((ancestor-numbers (save-excursion
                                          (cl-loop while (org-up-heading-safe)
                                                   collect (heading-number)))))
                  (nreverse (cons (heading-number) ancestor-numbers))))
              (add-overlay ()
		(let* ((ov-length (org-current-level))
                       (ov (make-overlay (point) (+ (point) ov-length)))
                       (ov-string (concat (mapconcat #'number-to-string (number-list) ".")
                                          ".")))
                  (overlay-put ov 'org-outline-numbers t)
                  (overlay-put ov 'display ov-string))))
    (remove-overlays nil nil 'org-outline-numbers t)
    (unless remove-p
      (org-with-wide-buffer
       (goto-char (point-min))
       (when (org-before-first-heading-p)
         (outline-next-heading))
       (cl-loop do (add-overlay)
                while (outline-next-heading))))))

(provide 'custom-org)
;;; custom-org.el ends here
