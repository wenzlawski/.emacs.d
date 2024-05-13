;;; setup-citar.el --- Setup citar -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun my/citar-toggle-multiple ()
  (interactive)
  (if citar-select-multiple
      (setq citar-select-multiple nil)
    (setq citar-select-multiple t)))

(use-package citar
  :straight t
  :hook (org-mode . citar-capf-setup)
  :custom
  ;; NOTE: Having large bibtex files slows down org-mode through bibtex
  (org-cite-global-bibliography '("~/Zotero/bibtex-export.bib" "~/cat.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (org-cite-csl-styles-dir
   (expand-file-name "~/Zotero/styles/"))
  (citar-bibliography org-cite-global-bibliography)
  (citar-at-point-function 'embark-act)
  (citar-select-multiple nil)
  (citar-templates
   '((main . "${author editor:30%sn}     ${date year issued:4}     ${title:48}")
     (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
     (preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
     (note . "Notes on ${author editor:%etal}, ${title}")))
  :bind
  (:map org-mode-map :package org
	("C-c b" . #'org-cite-insert)
	("C-c B" . citar-dwim)))

(with-eval-after-load 'citar
  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-file_o"
              :face 'nerd-icons-green
              :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  " ; need this because the default padding is too low for these icons
     :tag "has:files"))
  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-link"
              :face 'nerd-icons-orange
              :v-adjust 0.01)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))
  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (nerd-icons-codicon
              "nf-cod-note"
              :face 'nerd-icons-blue
              :v-adjust -0.0)
     :function #'citar-has-notes
     :padding "  "
     :tag "has:notes"))
  (defvar citar-indicator-cited-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-circle_o"
              :face 'nerd-icon-green)
     :function #'citar-is-cited
     :padding "  "
     :tag "is:cited"))
  (setq citar-indicators
	(list citar-indicator-files-icons
	      citar-indicator-notes-icons)))

(use-package citar-embark
  :straight t
  :after citar embark
  :config (citar-embark-mode))

(use-package citar-denote
  :straight t
  :custom
  ;; Use package defaults
  (citar-open-always-create-notes nil)
  (citar-denote-file-type 'org)
  (citar-denote-subdir t)
  (citar-denote-keyword "bib")
  (citar-denote-use-bib-keywords nil)
  (citar-denote-title-format "author-year-title")
  (citar-denote-title-format-authors 1)
  (citar-denote-title-format-andstr "and")
  :init
  (citar-denote-mode))

(provide 'setup-citar)
;;; setup-citar.el ends here
