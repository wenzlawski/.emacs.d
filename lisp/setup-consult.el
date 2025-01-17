;;; setup-consult --- Configuration for consult
;;; Commentary:
;;; Code:

(use-package consult
  :straight t
  :commands (consult-customize)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (consult-locate-args "locate")
  (consult-narrow-key "<")
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)
  :bind
  ([remap Info-search] . consult-info)
  ;; C-x bindings in `ctl-x-map'
  ("C-x M-:" . consult-complex-command)
  ("C-x b" . consult-buffer)
  ("C-'" . my/consult-buffer-projectile)
  ("C-x 4 b" . consult-buffer-other-window)
  ("C-x 5 b" . consult-buffer-other-frame)
  ("C-x t b" . consult-buffer-other-tab)
  ("C-x r b" . consult-bookmark)
  ("M-y" . consult-yank-pop)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  ("M-g g" . consult-goto-line)
  ("M-g M-g" . consult-goto-line)
  ("M-g m" . consult-mark)
  ("M-g M" . consult-global-mark)
  ("M-g o" . consult-outline)
  ;; ("M-o" . consult-outline)
  ;; M-s bindings in `search-map'
  ("M-s d" . consult-fd)
  ;; ("M-s c" . consult-locate)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s R" . consult-ripgrep)
  ("M-s M-r" . consult-ripgrep-all)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  ;; Isearch integration
  ("M-s e" . consult-isearch-history)
  (:map project-prefix-map
	("b" . consult-project-buffer))
  (:map isearch-mode-map
	("M-e" . consult-isearch-history)
	("M-s e" . consult-isearch-history)
	("M-s l" . consult-line)
	("M-s L" . consult-line-multi))
  (:map minibuffer-local-map
	("M-s" . consult-history)
	("M-r" . consult-history))
  (:map prog-mode-map
	("M-o" . consult-imenu)))

(with-eval-after-load 'consult
  (setopt consult-buffer-sources
	  '(consult--source-hidden-buffer
	    consult--source-modified-buffer
	    consult--source-buffer
	    consult--source-file-register
	    consult--source-bookmark
	    consult--source-project-buffer-hidden
	    consult--source-project-recent-file-hidden)
	  consult-project-buffer-sources
	  '(consult--source-project-recent-file
	    consult--source-project-buffer)
	  ))

(defun my/consult-outline-minor-mode-goto ()
  "Goto an outline minor mode heading."
  (interactive)
  (consult-outline)
  (recenter-top-bottom 0)
  (outline-show-entry))

(defun my/consult-buffer-projectile (&optional arg)
  "Consult buffer when in project."
  (interactive "p")
  (case arg
    (4 (projectile-find-file))
    (16 (consult-buffer))
    (t (if (projectile-project-p)
	   (consult-project-buffer)
	 (consult-buffer)))))

(defun my/denote-consult-ripgrep ()
  "Search denote directory with ripgrep."
  (interactive)
  (consult-ripgrep (denote-directory)))

(bind-key "C-c n f f" #'my/denote-consult-ripgrep)

(use-package consult-org
  :commands (consult-org-heading)
  :after consult
  :config
  (bind-key "C-c h" #'consult-org-heading 'org-mode-map))

(use-package consult-flycheck
  :straight t
  :after consult)

(use-package consult-recoll
  :straight t
  :after (consult embark)
  :config
  (consult-recoll-embark-setup))

;; still doesn't work
(with-eval-after-load 'consult-recoll
  (defvar consult-recoll--recollq (executable-find "recollq"))

  (defun consult-recoll--command (text)
    "Command used to perform queries for TEXT."
    (setq consult-recoll--current nil)
    (setq consult-recoll--index 0)
    (setq consult-recoll--snippets nil)
    `(,consult-recoll--recollq ,@(consult-recoll--search-flags) ,text))
  )

(use-package consult-notes
  :straight t
  :commands (consult-notes
	     consult-notes-denote-mode
	     consult-notes-denote--state
	     consult-notes-denote--new-note)
  :custom-face
  (consult-notes-sep ((t (:foreground "CornFlowerBlue"))))
  :bind
  ("C-c n o" . consult-notes)
  ("C-c n X" . consult-notes-search-in-all-notes)
  ("C-c n 4 o" . my/consult-notes-other-window)
  :custom
  (consult-notes-file-dir-sources '(("Org" ?o "~/personal/Org/")))
  (consult-notes-org-headings-files '("~/personal/Org/"))
  ;; search only for text files in denote dir
  :config
  (when (locate-library "denote")
    (setopt consult-notes-denote-display-id nil)
    (setopt consult-notes-denote-files-function 'denote-directory-files)
    (setopt consult-notes-denote-truncate-title 60)
    (consult-notes-denote-mode))

  ;; (consult-notes-org-headings-mode)

  (defun my/consult-notes-other-window ()
    "Open a note in another window"
    (interactive)
    (let ((consult--buffer-display #'switch-to-buffer-other-window))
      (consult-notes)))

  ;; NOTE: ' and #' don't seem to work here.
  (consult-customize consult-notes my/consult-notes-other-window consult-fd :preview-key "M-.")

  (consult-customize
   consult-theme
   :preview-key (list "C-SPC" :debounce 0.5 'any))

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key "C-SPC")

  )

;;; consult-notes-denote padding
(with-eval-after-load 'consult-notes-denote
  (defvar consult-notes-denote-truncate-title nil
    "Truncate title in Denote notes. Can be nil or a number.")
  (declare-function denote-filetype-heuristics "denote" (FILE))
  (declare-function denote-retrieve-title-value "denote" (ARG1 ARG2))
  (declare-function denote-retrieve-filename-title "denote" (FILE))
  (declare-function denote-retrieve-filename-identifier "denote" (FILE))
  (declare-function denote-extract-keywords-from-path "denote" (FILE))

  (defconst consult-notes-denote--source
    (list :name     (propertize "Denote notes" 'face 'consult-notes-sep)
	  :narrow   ?d
	  :category (bound-and-true-p consult-notes-category)
	  :annotate (bound-and-true-p consult-notes-denote-annotate-function)
	  :items    (lambda ()
		      (let* ((max-width (if consult-notes-denote-truncate-title consult-notes-denote-truncate-title 0))
			     (cands (mapcar (lambda (f)
					      (let* ((id (denote-retrieve-filename-identifier f))
						     (title-1 (or (denote-retrieve-title-value f (denote-filetype-heuristics f)) (denote-retrieve-filename-title f)))
						     (title (if (bound-and-true-p consult-notes-denote-display-id)
								(concat id " " title-1)
							      title-1))
						     (title (if consult-notes-denote-truncate-title
								(truncate-string-to-width title consult-notes-denote-truncate-title) title))
						     (dir (file-relative-name (file-name-directory f) (bound-and-true-p denote-directory)))
						     (keywords (denote-extract-keywords-from-path f)))
						(if (not consult-notes-denote-truncate-title)
						    (let ((current-width (string-width title)))
						      (when (> current-width max-width)
							(setq max-width current-width))))
						(propertize title 'denote-path f 'denote-keywords keywords)))
					    (funcall (bound-and-true-p consult-notes-denote-files-function)))))
			(mapcar (lambda (c)
				  (let* ((keywords (get-text-property 0 'denote-keywords c))
					 (path (get-text-property 0 'denote-path c))
					 (dirs (directory-file-name (file-relative-name (file-name-directory path) (bound-and-true-p denote-directory)))))
				    (concat c
					    ;; align keywords
					    (propertize " " 'display `(space :align-to (+ left ,(+ 2 max-width))))
					    (format "%18s"
						    (if keywords
							(concat (propertize "#" 'face 'consult-notes-name)
								(propertize (mapconcat 'identity keywords " ") 'face 'consult-notes-name))
						      ""))
					    (when (bound-and-true-p consult-notes-denote-dir) (format "%18s" (propertize (concat "/" dirs) 'face 'consult-notes-name))))))
				cands)))
	  ;; Custom preview
	  :state  #'consult-notes-denote--state
	  ;; Create new note on match fail
	  :new     #'consult-notes-denote--new-note))

  (defun consult-notes-my-embark-function (cand)
    "Do something with CAND."
    (interactive "fNote: ")
    (message cand))

  (with-eval-after-load 'embark
    (defvar-keymap consult-notes-map
      :doc "Keymap for Embark notes actions."
      :parent embark-file-map
      "m" #'consult-notes-my-embark-function)

    (add-to-list 'embark-keymap-alist `(,consult-notes-category . consult-notes-map))

    ;; make embark-export use dired for notes
    (setf (alist-get consult-notes-category embark-exporters-alist) #'embark-export-dired)))

(use-package consult-flyspell
  :straight t
  :after consult
  :commands (consult-flyspell)
  :config
  (with-eval-after-load 'flyspell
    (bind-key "C-;" #'consult-flyspell 'flyspell-mode-map))
  :custom
  (consult-flyspell-select-function 'flyspell-correct-at-point)
  (consult-flyspell-set-point-after-word t)
  (consult-flyspell-always-check-buffer nil))

(use-package consult-eglot
  :straight t
  :bind
  (:map eglot-mode-map
	("C-c e c" . consult-eglot-symbols)))

(use-package consult-eglot-embark
  :straight t
  :after consult-eglot
  :config
  (consult-eglot-embark-mode))

(use-package consult-todo
  :straight t
  :custom
  (consult-todo-narrow '((?t . "TODO")
			 (?f . "FIXME")
			 (?b . "BUG")
			 (?h . "HACK"))))


;; (use-package consult-bibtex
;;   :after consult
;;   :straight '(consult-bibtex :host github :repo "mohkale/consult-bibtex")
;;   :config
;;   (with-eval-after-load 'embark
;;     (add-to-list 'embark-keymap-alist '(bibtex-completion . consult-bibtex-embark-map))))

(use-package consult-yasnippet
  :after yasnippet
  :straight t
  :bind
  (:map yas-minor-mode-map
	("C-c & C-s" . consult-yasnippet)
	("C-c & s" . consult-yasnippet)
	("C-c & C-v" . consult-yasnippet-visit-snippet-file)
	("C-c & v" . consult-yasnippet-visit-snippet-file)))


(use-package consult-notmuch
  :straight t)

(require 'consult-ripgrep-all)

(provide 'setup-consult)
;;; setup-consult.el ends here
