;;; SETUP-VERTICO --- Setting up vertico  -*- lexical-binding: t; -*-  
;;
;; Author: Marc Wenzlawski <marc.wenzlawski@icloud.com>
;; Copyright © 2024, Marc Wenzlawski, all rights reserved.
;; Created: 14 June 2024
;;
;;; Commentary:
;;; Code:

(use-package vertico
  :straight t
  :custom-face
  ;;(vertico-current ((t (:background "slate"))))
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 10)
  (vertico-resize 'grow-only)
  :init
  (vertico-mode)
  ;; (advice-add #'vertico--display-candidates
  ;; 	      :override #'vertico-bottom--display-candidates)
  :config
  (bind-key "C-c C-n" #'vertico-quick-jump 'vertico-map))

(use-package vertico-grid
  :after vertico
  :custom
  (vertico-grid-min-columns 3))

(use-package vertico-multiform
  :after vertico
  :init
  (vertico-multiform-mode 1)
  :custom
  (vertico-multiform-commands
   `((consult-ripgrep buffer)
     (consult-buffer flat (vertico-cycle . t))
     (execute-extended-command 
      (+vertico-transform-functions . +vertico-highlight-enabled-mode))
     (dired grid)))
  (vertico-multiform-categories
   '((file (vertico-sort-function . sort-directories-first)
           (+vertico-transform-functions . +vertico-highlight-directory))
     (jinx grid (vertico-grid-annotate . 20)))))

(use-package vertico-quick
  :after vertico
  :bind
  (:map vertico-map
	("'" . vertico-quick-jump)
	("C-q" . vertico-quick-exit))
  :custom
  (vertico-quick1 "arstgm")
  (vertico-quick2 "neluykh"))

(use-package vertico-repeat
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(defun my/vertico-quick-jump-select ()
  "Jump to candidate using quick keys."
  (interactive)
  (if (= vertico--total 0)
      (and (minibuffer-message "No match") nil)
    (let ((idx (vertico-quick--read)))
      (when (consp idx) (setq idx (vertico-quick--read (car idx))))
      (when idx (setq vertico--index idx)
	    (vertico-directory-enter)))))

(bind-key "`" #'my/vertico-quick-jump-select 'vertico-map)

(defun vertico-bottom--display-candidates (lines)
  "Display LINES in bottom."
  (move-overlay (bound-and-true-p vertico--candidates-ov) (point-min) (point-min))
  (unless (eq (bound-and-true-p vertico-resize) t)
    (setq lines (nconc (make-list (max 0 (- (bound-and-true-p vertico-count) (length lines))) "\n") lines)))
  (let ((string (apply #'concat lines)))
    (add-face-text-property 0 (length string) 'default 'append string)
    (overlay-put (bound-and-true-p vertico--candidates-ov) 'before-string string)
    (overlay-put (bound-and-true-p vertico--candidates-ov) 'after-string nil))
  (vertico--resize-window (length lines)))

;; *** Highlight files in completing read

(defun sort-directories-first (files)
  ;; Still sort by history position, length and alphabetically
  (setq files (vertico-sort-history-length-alpha files))
  ;; But then move directories first
  (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
         (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

(defvar +vertico-transform-functions nil)

(cl-defmethod vertico--format-candidate :around
  (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
  (dolist (fun (ensure-list +vertico-transform-functions))
    (setq cand (funcall fun cand)))
  (cl-call-next-method cand prefix suffix index start))

(defun +vertico-highlight-directory (file)
  "If FILE ends with a slash, highlight it as a directory."
  (if (string-suffix-p "/" file)
      (propertize file 'face 'marginalia-file-priv-dir) ; or face 'dired-directory
    file))

;; function to highlight enabled modes similar to counsel-M-x
(defun +vertico-highlight-enabled-mode (cmd)
  "If MODE is enabled, highlight it as font-lock-constant-face."
  (let ((sym (intern cmd)))
    (if (or (eq sym major-mode)
            (and
             (memq sym minor-mode-list)
             (boundp sym)))
	(propertize cmd 'face 'font-lock-constant-face)
      cmd)))

(provide 'setup-vertico)
;;; setup-vertico.el ends here
