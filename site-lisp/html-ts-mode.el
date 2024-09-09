;;; html-ts-mode.el --- tree-sitter support for HTML  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Free Software Foundation, Inc.

;; Author     : Theodor Thornhill <theo@thornhill.no>
;; Maintainer : Theodor Thornhill <theo@thornhill.no>
;; Created    : January 2023
;; Keywords   : html languages tree-sitter

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(require 'treesit)
(require 'sgml-mode)
(require 'css-mode)
(require 'js)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-type "treesit.c")

(defcustom html-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `html-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'html)

(defvar html-ts-mode--indent-rules
  `((html
     ((parent-is "fragment") column-0 0)
     ((node-is "/>") parent-bol 0)
     ((node-is ">") parent-bol 0)
     ((node-is "end_tag") parent-bol 0)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "element") parent-bol html-ts-mode-indent-offset)
     ((parent-is "script_element") parent-bol html-ts-mode-indent-offset)
     ((parent-is "style_element") parent-bol html-ts-mode-indent-offset)
     ((parent-is "start_tag") parent-bol html-ts-mode-indent-offset)
     ((parent-is "self_closing_tag") parent-bol html-ts-mode-indent-offset))
    (css . ,(alist-get 'css css--treesit-indent-rules))
    (javascript . ,(alist-get 'javascript js--treesit-indent-rules)))
  "Tree-sitter indent rules.")

(defun html-ts-mode--prefix-font-lock-features (prefix settings)
  "Prefix with PREFIX the font lock features in SETTINGS."
  (mapcar (lambda (setting)
            (list (nth 0 setting)
                  (nth 1 setting)
                  (intern (format "%s-%s" prefix (nth 2 setting)))
                  (nth 3 setting)))
          settings))

(defvar html-ts-mode--font-lock-settings
  (append
   js--treesit-font-lock-settings
   css--treesit-settings
   (treesit-font-lock-rules
    :language 'html
    :override t
    :feature 'comment
    `((comment) @font-lock-comment-face)
    :language 'html
    :override t
    :feature 'keyword
    `("doctype" @font-lock-keyword-face)
    :language 'html
    :override t
    :feature 'definition
    `((tag_name) @font-lock-function-name-face)
    :language 'html
    :override t
    :feature 'string
    `((quoted_attribute_value) @font-lock-string-face)
    :language 'html
    :override t
    :feature 'property
    `((attribute_name) @font-lock-variable-name-face)))
  "Tree-sitter font-lock settings for `html-ts-mode'.")

(defvar html-ts-mode--range-settings
  (treesit-range-rules
   :embed 'js
   :host 'html
   '((script_element (raw_text) @cap))

   :embed 'css
   :host 'html
   '((style_element (raw_text) @cap))

   :embed 'css
   :host 'html
   :local t
   '((attribute
      ((attribute_name) @_name (:match "style" @_name))
      (quoted_attribute_value (attribute_value) @capture)))))

(defun html-ts-mode--treesit-language-at-point (point)
  "Return the language at POINT."
  (let* ((range nil)
         (language-in-range
          (cl-loop
           for parser in (treesit-parser-list)
           do (setq range
                    (cl-loop
                     for range in (treesit-parser-included-ranges parser)
                     if (and (>= point (car range)) (<= point (cdr range)))
                     return parser))
           if range
           return (treesit-parser-language parser))))
    (or language-in-range 'html)))

(defun html-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (when (equal (treesit-node-type node) "tag_name")
    (treesit-node-text node t)))

;;;###autoload
(define-derived-mode html-ts-mode html-mode "HTML"
  "Major mode for editing Html, powered by tree-sitter."
  :group 'html

  (unless (treesit-ready-p 'html)
    (error "Tree-sitter for HTML isn't available"))

  (unless (treesit-ready-p 'css)
    (error "Tree-sitter grammar for CSS isn't available"))

  (unless (treesit-ready-p 'javascript)
    (error "Tree-sitter grammar for JavaScript isn't available"))

  (treesit-parser-create 'html)

  ;; Indent.
  (setq-local treesit-simple-indent-rules html-ts-mode--indent-rules
	      css-indent-offset html-ts-mode-indent-offset
	      js-indent-level html-ts-mode-indent-offset)

  ;; Navigation.
  (setq-local treesit-defun-type-regexp "element")

  (setq-local treesit-defun-name-function #'html-ts-mode--defun-name)

  (setq-local treesit-thing-settings
              `((html
                 (sexp ,(regexp-opt '("element"
                                      "text"
                                      "attribute"
                                      "value")))
                 (sentence "tag")
                 (text ,(regexp-opt '("comment" "text"))))))

  ;; Font-lock.
  (setq-local treesit-font-lock-settings html-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment keyword definition selector query)
                (property string)
                () ()))

  ;; Imenu.
  (setq-local treesit-simple-imenu-settings
              '(("Element" "\\`tag_name\\'" nil nil)))

  ;; Embedded languages
  (setq-local treesit-range-settings html-ts-mode--range-settings
	      treesit-language-at-point-function
	      #'html-ts-mode--treesit-language-at-point)


  ;; Outline minor mode.
  (setq-local treesit-outline-predicate "\\`element\\'")
  ;; `html-ts-mode' inherits from `html-mode' that sets
  ;; regexp-based outline variables.  So need to restore
  ;; the default values of outline variables to be able
  ;; to use `treesit-outline-predicate' above.
  (kill-local-variable 'outline-regexp)
  (kill-local-variable 'outline-heading-end-regexp)
  (kill-local-variable 'outline-level)

  (treesit-major-mode-setup))

(derived-mode-add-parents 'html-ts-mode '(html-mode))

(if (treesit-ready-p 'html)
    (add-to-list 'auto-mode-alist '("\\.html\\'" . html-ts-mode)))

(provide 'html-ts-mode)

;;; html-ts-mode.el ends here
