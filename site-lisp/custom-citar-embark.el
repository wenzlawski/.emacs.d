;; scratchy scratch

;; citar-open-files-on-page
;; needs a pdf to work

(defun citar-open-files-on-page (CITEKEY)
  "Open the citar file on page")

;; (with-current-buffer "refile.org"
;;   (let* ((element (org-element-context))
;; 	 )
;;     (my/citar-org-element-get-page element)
;;     ))

(defun my/citar-org-element-get-page (element)
  (let* ((suffix (car (org-element-property :suffix element))))
    (if suffix (progn
		 (setq suffix (substring-no-properties suffix))
		 (string-match (rx "p. " (group (one-or-more digit))) suffix)
		 (string-to-number (match-string 1 suffix)))
      nil)))


(defun my/citar-org--key-at-point-suffix ()
  "Return org-cite citation keys at point as a list for `embark'."
  (when-let ((reference (citar-org--reference-at-point)))
    (list (org-element-property :key reference)
          (cons (org-element-property :begin reference)
                (org-element-property :end reference))
	  (my/citar-org-element-get-page reference))
    ))

;; (advice-add #'citar-org--key-at-point :override #'my/citar-org--key-at-point-suffix)

(defun my/citar-dwim ()
  "Run the default action on citation keys found at point."
  (interactive)
  (if-let ((citekeys (or (citar-key-at-point) (citar-citation-at-point))))
      (citar-run-default-action (if (listp citekeys) citekeys (list citekeys)))
    (user-error "No citation keys found")))

(defun citar-file-open-at-page (file page)
  "Open FILE at page (must be PDF.)"
  (if-let* ((ext (file-name-extension file))
            (func (cdr (or (assoc-string ext citar-file-open-functions 'case-fold)
                           (assq t citar-file-open-functions)))))
      (progn
	(if (not (eq ext "pdf")) (user-error "File has to be pdf."))
	(funcall func (expand-file-name file))
	(pdf-view-goto-page page))
    (user-error "Could not find extension in `citar-file-open-functions': %s" ext)))

(defun citar-open-file-at-page (citekey page)
  "Open library file associated with citekey at page."
  (interactive (list (citar-select-refs) (read-number "At page:")))
  (citar--library-file-action citekey #'citar-file-open-at-page))






