;;; setup-hydra.el --- Hydra setup
;;; Commentary:
;;; Code:

(use-package hydra
  :straight t)

(use-package pretty-hydra
  :after hydra
  :straight t)


;;; Launch Menu
(defvar my/hydra-launch-menu)
(pretty-hydra-define my/hydra-launch-menu
  (:title "Launch Menu" :quit-key "q" :color teal)
  ("Applications"
   (("a" #'org-agenda "Agenda")
    ("b" #'calibredb "Calibre")
    ("c" #'calendar "Calendar")
    ("e" #'eww "EWW")
    ("m" #'notmuch "Mail")
    ("n" (lambda () (interactive) (find-file "~/Org/personal.org")) "Notes")
    ("o" #'ebdb "Contacts")
    ("t" #'vterm "Terminal"))
   "Utilities"
   (("f" #'dired "Files")
    ("h" #'helpful-at-point "Help")
    ("i" (lambda () (interactive) (find-file user-init-file)) "init")
    ("j" (lambda () (interactive) (let ((projectile-project-root "~/.emacs.d/")) (projectile-find-file))) "configs")
    ("l" #'consult-line "Line")
    ("r" #'ielm "REPL")
    ("s" (lambda () (interactive) (switch-to-buffer "*scratch*")) "Scratch")
    ("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
    ("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))
    )))

(bind-key "C-`" #'my/hydra-launch-menu/body)

;;; Toggle Menu
(defvar my/hydra-toggle-menu)
(pretty-hydra-define my/hydra-toggle-menu
  (:title "Toggle Menu" :quit-key "q" :color pink)
  ("Modes"
   (("l" visual-line-mode "visual line" :toggle t)
    ("v" visual-fill-column-mode "fill column" :toggle t)
    ("p" variable-pitch-mode "variable pitch" :toggle (default-value (bound-and-true-p buffer-face-mode)))
    ("h" hl-line-mode "hl-line" :toggle t)
    ("m" hide-mode-line-mode "mode line" :toggle t)
    ("c" centered-cursor-mode "center cursor" :toggle t))
   "Display"
   (("t" #'consult-theme "theme")
    ("T" #'fontaine-set-preset "font" :exit t)
    ("d" #'toggle-frame-fullscreen "fullscreen" :toggle (frame-parameter nil 'fullscreen))
    ("w" #'my/distraction-free "writing" :toggle (default-value (bound-and-true-p olivetti-mode)))
    ("e" (lambda () (interactive) (setq visual-fill-column-center-text (not (bound-and-true-p visual-fill-column-center-text)))) "center text"
     :toggle (default-value (bound-and-true-p visual-fill-column-center-text))))
   "Editing"
   (("i" #'indent-guide-mode "indent guide")
    ("s" display-line-numbers-mode "line numbers" :toggle t)
    ("a" copilot-mode "copilot" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t)
    ("x" (lambda () (interactive) (setopt corfu-auto (not corfu-auto)) (corfu-mode -1) (corfu-mode)) "corfu auto" :toggle (default-value corfu-auto))
    ("y" yas-minor-mode "yasnippet" :toggle t)
    )))

(bind-key "t" #'my/hydra-toggle-menu/body 'help-map)

(with-eval-after-load 'outline
;;; Outline Menu
  (defvar my/hydra-outline)
  (pretty-hydra-define my/hydra-outline
    (:title "Outline Menu" :color pink :hint nil :quit-key "z")
    ("Hide"
     (("q" #'outline-hide-sublevels "sublevels")
      ("t" #'outline-hide-body "body")
      ("o" #'outline-hide-other "other")
      ("c" #'outline-hide-entry "entry")
      ("l" #'outline-hide-leaves "leaves")
      ("d" #'outline-hide-subtree "subtree"))
     "Show"
     (("a" #'outline-show-all "all")
      ("e" #'outline-show-entry "entry")
      ("i" #'outline-show-children "children")
      ("k" #'outline-show-branches "branches")
      ("s" #'outline-show-subtree "subtree"))
     "Move"
     (("u" #'outline-up-heading "up")
      ("n" #'outline-next-visible-heading "next")
      ("p" #'outline-previous-visible-heading "previous")
      ("f" #'outline-forward-same-level "forward")
      ("b" #'outline-backward-same-level "backward")
      ("/" #'consult-outline "outline"))
     "Edit"
     (("k" #'outline-headers-as-kill "kill")
      ("U" #'outline-move-subtree-up "move up")
      ("D" #'outline-move-subtree-down "move down")
      ("<" #'outline-promote "promote")
      (">" #'outline-demote "demote"))))
  (bind-key "<f8>" #'my/hydra-outline/body 'outline-minor-mode-map))

(with-eval-after-load 'org
;;; Org Refile
  (defvar my/hydra-org-refile)
  (pretty-hydra-define my/hydra-org-refile
    (:title "Refile" :color teal :quit-key "q")
    ("Refile"
     (("r" (my/refile "resources.org") "Resources")
      ("s" (my/refile "personal.org" "Someday/Maybe") "Maybe")
      ("t" (my/refile "personal.org" "Tasks") "Tasks"))))
  (bind-key "<f6>" #'my/hydra-org-refile/body 'org-mode-map))

;; (with-eval-after-load 'consult-bibtex
;;   (defvar my/hydra-bibtex)
;;   (pretty-hydra-define my/hydra-bibtex
;;     (:title "Bibtex" :color teal :quit-key "q")
;;     ("Open"
;;      (("a" consult-bibtex "Search")
;;       ("p" consult-bibtex-open-pdf "Open PDF")
;;       ("o" consult-bibtex-open-any "Open Any")
;;       ("u" consult-bibtex-open-url-or-doi "Open URL/DOI")
;;       ("s" consult-bibtex-show-entry "Show Entry"))
;;      "Insert"
;;      (("i" consult-bibtex-insert-key "Insert Key")
;;       ("b" consult-bibtex-insert-bibtex "Insert Bibtex")
;;       ("c" consult-bibtex-insert-citation "Insert Citation")
;;       ("r" consult-bibtex-insert-reference "Insert Reference"))
;;      "Edit"
;;      (("n" consult-bibtex-edit-notes "Notes")
;;       ("P" consult-bibtex-add-PDF-attachment "Add PDF Attachment")
;;       ("L" consult-bibtex-add-pdf-to-library "Add PDF to Library"))))
;;   (bind-key "C-c c" #'my/hydra-bibtex/body)
;;   )

(defvar my/hydra-citar)
(pretty-hydra-define my/hydra-citar
  (:title "Citar" :color teal :quit-key "q")
  ("Denote"
   (("n" citar-denote-open-note "Open Note")
    ("r" citar-denote-open-reference-entry "Open Entry")
    ("R" citar-denote-link-reference "Link Reference")
    ("L" citar-denote-link-note "Link Note")
    ("C" citar-denote-link-citation "Link Citation")
    ("f" citar-denote-find-reference "Find Reference")
    ("S" citar-denote-find-citation "Find Citation")
    ("d" citar-denote-dwim "DWIM"))
   "Open"
   (("oo" citar-open "Open")
    ("oe" citar-open-entry "Entry")
    ("of" citar-open-files "Files")
    ("on" citar-open-notes "Notes")
    ("ol" citar-open-links "Links"))
   "Link"
   (("ik" citar-insert-keys "Insert Keys")
    ("ic" citar-insert-citation "Citation")
    ("ir" citar-insert-reference "Reference")
    ("ib" citar-insert-bibtex "Bibtex")
    ("ip" citar-insert-preset "Preset")
    ("ie" citar-insert-edit "Edit"))
   "Other"
   (("N" citar-create-note "Note")
    ("x" my/citar-toggle-multiple "Toggle Multiple" :toggle (bound-and-true-p citar-select-multiple))
    ("y" citar-copy-reference "Copy Reference")
    ("z" citar-run-default-action "Default Action")
    )))
(bind-key "<f7>" #'my/hydra-citar/body)

;;; Smudge hydra
(defvar my/smudge-playlists
  '(("ot" minimal-techno "spotify:playlist:59KuQSm27IfRylpXxz9KrM")
    ("oy" yoga-electronica "spotify:playlist:37i9dQZF1DWYUYYlhkTuEn")))

(defvar my/hydra-smudge)
(eval
 `(pretty-hydra-define my/hydra-smudge (:title "Smudge" :color blue :quit-key "q")
    ("Track"
     (("b"  smudge-controller-previous-track "Previous" :exit nil)
      ("f"  smudge-controller-next-track "Next" :exit nil)
      ("t" smudge-track-search "Search Tracks"))
     "Playback"
     (("c"  smudge-controller-toggle-play "Play/Pause")
      ("r"  smudge-controller-toggle-repeat "Repeat")
      ("s"  smudge-controller-toggle-shuffle "Shuffle")
      ("a"  smudge-select-device "Select Device"))
     "Volume"
     (("u"  smudge-controller-volume-up "Volume Up" :exit nil)
      ("d"  smudge-controller-volume-down "Volume Down" :exit nil)
      ("m"  smudge-controller-mute-unmute "Mute"))
     "Playlists"
     (("pc" smudge-create-playlist "Create")
      ("pf" smudge-featured-playlists "Featured")
      ("ps" smudge-playlist-search "Search")
      ("pm" smudge-my-playlists "List My")
      ("pu" smudge-user-playlists "List User"))
     "Shortcuts"
     ,@(list (mapcar
	      (lambda (x)
		(list (car x)
		      (list 'my/smudge-play-playlist (caddr x))
		      (symbol-name (cadr x)))) my/smudge-playlists))
     )))

(bind-key "<f6>" #'my/hydra-smudge/body)

(with-eval-after-load 'cc-mode
  (defvar my/c-mode-hydra)
  (pretty-hydra-define my/c-mode-hydra
    (:title "C Mode" :color teal :quit-key "q" :exit t)
    ("Compile"
     (("c" compile "Compile")
      ("r" recompile "Recompile")
      ("C" recompile "Recompile"))
     "Run"
     (("q" quickrun "Quickrun")
      ("R" quickrun-region "Quickrun Region"))
     "Xref"
     (("xd" xref-find-definitions "Find Definitions")
      ("xr" xref-find-references "Find References")
      ("xD" xref-find-definitions-other-window "Find Definitions Other Window")
      ("xR" xref-find-references-other-window "Find References Other Window"))
     "Snippets"
     (("i" consult-yasnippet "Insert Snippet")
      ("n" yas-new-snippet "New Snippet")
      ("v" consult-yasnippet-visit-snippet-file "Edit Snippet"))
     "Hiding"
     (("hh" hs-toggle-hiding "Toggle")
      ("hb" hs-hide-block "Hide Block")
      ("hB" hs-show-block "Show Block")
      ("ha" hs-hide-all "Hide All")
      ("hA" hs-show-all "Show All"))
     "Debug"
     (("g" gdb "GDB"))
     "Tools"
     (("e" eglot "eglot"))))
  ;;(bind-key "C-c C-c" #'my/c-mode-hydra/body 'c-ts-base-mode-map)
  (bind-key "C-c C-c" #'my/c-mode-hydra/body 'c-mode-base-map))
;;(bind-key "C-c c" #'my/c-mode-hydra/body 'c-ts-base-mode-map)

(with-eval-after-load 'zig-ts-mode
  (defvar my/zig-mode-hydra)
  (pretty-hydra-define my/zig-mode-hydra
    (:title "Zig mode" :quit-key "q" :exit t)
    ("Code actions"
     (("c" zig-compile "Compile")
      ("f" zig-format "Format")
      ("r" zig-run "Run")
      ("t" zig-test "Test"))
     "Run"
     (("q" quickrun "Quickrun")
      ("R" quickrun-region "Quickrun Region"))
     "Xref"
     (("xd" xref-find-definitions "Definitions")
      ("xr" xref-find-references "References")
      ("xD" xref-find-definitions-other-window "Definitions OW")
      ("xR" xref-find-references-other-window "References OW"))
     "Snippets"
     (("i" consult-yasnippet "Insert Snippet")
      ("n" yas-new-snippet "New Snippet")
      ("v" consult-yasnippet-visit-snippet-file "Edit Snippet"))
     "Hiding"
     (("hh" hs-toggle-hiding "Toggle")
      ("hb" hs-hide-block "Hide Block")
      ("hB" hs-show-block "Show Block")
      ("ha" hs-hide-all "Hide All")
      ("hA" hs-show-all "Show All"))
     "Tools"
     (("e" eglot "eglot"))))

  (bind-key "C-c C-c" #'my/zig-mode-hydra/body 'zig-ts-mode-map))

(defvar my/denote-hydra)
(pretty-hydra-define my/denote-hydra
  (:title "Denote" :color teal :quit-key "q")
  ("New"
   (("n" denote "Note")
    ("t" denote-template "Template")
    ("s" denote-signature "Signature")
    ("c" denote-region "Region")
    ("z" denote-subdirectory "Subdirectory")
    ("N" denote-type "Type")
    ("d" denote-date "With Date")
    )
   "Link"
   (("i" denote-insert-link "Insert Link")
    ("l" denote-link-after-creating "Link After Creating")
    ("L" denote-link-or-create "Link or Create")
    ("h" denote-org-extras-link-to-heading "to Heading"))
   "Find"
   (("o" consult-notes "Open Note")
    ("O" my/consult-notes-other-window "Open Note Other Window")
    ("fl" denote-find-link "Find Link")
    ("fb" denote-find-backlink "Find Backlink")
    ("fs" search-denote "Search Denote")
    ("e" my/denote-explore-hydra/body "Explore" :exit t))
   "Edit"
   (("r" denote-rename-file "Rename File")
    ("R" denote-rename-file-using-front-matter "Rename File FM")
    ("b" denote-backlinks "Backlinks")
    ("P" denote-org-extras-extract-org-subtree "Extract Subtree")
    ("I" denote-add-links "Add Links")
    ("C" my/denote-rename-buffer "Rename Buffer"))
   ))

(bind-key "<f5>" #'my/denote-hydra/body)

(defvar my/denote-explore-hydra)
(pretty-hydra-define my/denote-explore-hydra
  (:title "Denote Explore" :color teal :quit-key "q")
  ("Network"
   (("n" denote-explore-network "Network")
    ("N" denote-explore-network-regenerate "Regenerate Network")
    ("r" denote-explore-random-link "Random Link")
    ("y" denote-explore-sync-metadata "Sync Metadata")
    ("d" denote-explore-degree-barchart "Degree Barchart")
    ("g" denote-explore-dashboard-activate "Activate Dashboard")
    ("G" denote-explore-dashboard-deactivate "Deactivate Dashboard")
    ("x" denote-explore-extensions-barchart "Extensions Barchart")
    )
   "Notes"
   (
    ("c" denote-explore-count-notes "Count")
    ("i" denote-explore-isolated-notes "Isolated Notes")
    ("R" denote-explore-random-note "Random Note")
    )
   "Keywords"
   (
    ("s" denote-explore-sort-keywords "Sort Keywords")
    ("z" denote-explore-zero-keywords "Zero Keywords")
    ("K" denote-explore-random-keyword "Random Keyword")
    ("e" denote-explore-rename-keyword "Rename Keyword")
    ("k" denote-explore-count-keywords "Count Keywords")
    ("E" denote-explore-rename-keywords "Rename Keywords")
    ("S" denote-explore-single-keywords "Single Keywords")
    ("B" denote-explore-keywords-barchart "Keywords Barchart")
    )
   ))


(provide 'setup-hydra)
;;; setup-hydra.el ends here
