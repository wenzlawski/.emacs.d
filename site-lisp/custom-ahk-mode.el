;;; CUSTOM-AHK-MODE ---   -*- lexical-binding: t; -*-
;;
;; Author: Marc Wenzlawski <marcwenzlawski@posteo.com>
;; Copyright © 2025, Marc Wenzlawski, all rights reserved.
;; Created: 30 January 2025
;;
;;; Commentary:
;;
;;  
;;
;;; Code:

(define-derived-mode ahk-mode prog-mode "AutoHotkey Mode"
  "Major mode for editing AutoHotkey script (AHK).

The hook functions in `ahk-mode-hook' are run after mode initialization.

Key Bindings
\\{ahk-mode-map}"
  ;; (kill-all-local-variables)

  (set-syntax-table ahk-mode-syntax-table)

  (setq major-mode 'ahk-mode
        mode-name "AHK"
        local-abbrev-table ahk-mode-abbrev-table)

  ;; ui
  (use-local-map ahk-mode-map)
  (easy-menu-add ahk-menu)

  ;; imenu
  (setq-local imenu-generic-expression ahk-imenu-generic-expression)
  (setq-local imenu-sort-function 'imenu--sort-by-position)

  ;; font-lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((ahk-font-lock-keywords) nil t))
  ;; (set (make-local-variable 'font-lock-multiline) t)
  ;; (add-hook 'font-lock-extend-region-functions
  ;;           'ahk-font-lock-extend-region)
  ;; (setq syntax-propertize-function)

  ;; clear memory
  ;; (setq ahk-commands-regexp nil)
  ;; (setq ahk-functions-regexp nil)
  ;; (setq ahk-variables-regexp nil)
  ;; (setq ahk-keys-regexp nil)

  (if (boundp 'evil-shift-width)
      (setq-local evil-shift-width ahk-indentation))

  (setq-local comment-start ";")
  (setq-local comment-end   "")
  (setq-local comment-start-skip ";+ *")

  (setq-local block-comment-start     "/*")
  (setq-local block-comment-end       "*/")
  (setq-local block-comment-left      " * ")
  (setq-local block-comment-right     " *")
  (setq-local block-comment-top-right "")
  (setq-local block-comment-bot-left  " ")
  (setq-local block-comment-char      ?*)

  (setq-local indent-line-function   'ahk-indent-line)
  (setq-local indent-region-function 'ahk-indent-region)

  (setq-local parse-sexp-ignore-comments t)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local paragraph-start (concat "$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)

  ;; completion
  (setq-local company-tooltip-align-annotations t)
  (add-hook 'completion-at-point-functions 'ahk-completion-at-point nil t)

  (eval-after-load "auto-complete"
    '(when (listp 'ac-sources)
       (progn
         (make-local-variable 'ac-sources)
         (add-to-list 'ac-sources  'ac-source-ahk)
         (add-to-list 'ac-sources  'ac-source-directives-ahk)
         (add-to-list 'ac-sources  'ac-source-keys-ahk))))

  (run-mode-hooks 'ahk-mode-hook))

(provide 'custom-ahk-mode)
;;; custom-ahk-mode.el ends here
