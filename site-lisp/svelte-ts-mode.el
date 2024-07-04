;;; svelte-ts-mode.el --- Major mode for editing Svelte templates  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ruby Iris Juric

;; Author: Ruby Iris Juric <ruby@srxl.me>
;; Homepage: https://github.com/Sorixelle/svelte-ts-mode
;; Version: 2.0.0
;; Package-Requires: ((emacs "29"))
;; Keywords: languages

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a major mode with syntax highlighting for Svelte
;; templates. It leverages Emacs' built-in tree-sitter support, as well as
;; virchau13's tree-sitter grammar for Svelte.
;;
;; More info:
;; README: https://github.com/Sorixelle/svelte-ts-mode
;; tree-sitter-svelte: https://github.com/virchau13/tree-sitter-svelte
;; Svelte: https://svelte.build/

;;; Code:

(require 'treesit)
(require 'typescript-ts-mode)
(require 'css-mode)

(defgroup svelte ()
  "Major mode for editing Svelte templates."
  :group 'languages)

(defcustom svelte-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `svelte-ts-mode'."
  :type 'integer
  :group 'svelte
  :package-version '(svelte-ts-mode . "1.0.0"))

;; TODO indent rules for typescript and css work but we need initial indent to account
;; for the parent node
;; NOTE: indentation for css is fixed, but there are issues for typescript.

(defvar svelte-ts-mode--indent-rules
  `((svelte
     ((parent-is "document") column-0 0)
     ((node-is "/>") parent-bol 0)
     ((node-is ">") parent-bol 0)
     ;; ((and (parent-is "script_element")
     ;; 	   (node-is "end_tag")) column-0 0)
     ((node-is "end_tag") parent-bol 0)
     ((node-is "if_end") parent-bol 0)
     ((node-is "each_end") parent-bol 0)
     ((node-is "else_block") parent-bol 0)
     ((node-is "else_if_block") parent-bol 0)
     ((query ((else_if_block (expression (svelte_raw_text) @text) @out
			     (:match ":else" @text)))) parent-bol 0)
     ((node-is "then_block") parent-bol 0)
     ((node-is "catch_block") parent-bol 0)
     ((node-is "await_end") parent-bol 0)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "element") parent-bol svelte-ts-mode-indent-offset)
     ((parent-is "script_element") parent-bol svelte-ts-mode-indent-offset)
     ((parent-is "style_element") parent-bol svelte-ts-mode-indent-offset)
     ((parent-is "start_tag") parent-bol svelte-ts-mode-indent-offset)
     ((parent-is "self_closing_tag") parent-bol svelte-ts-mode-indent-offset)
     ((parent-is "if_statement") parent-bol svelte-ts-mode-indent-offset)
     ((parent-is "each_statement") parent-bol svelte-ts-mode-indent-offset)
     ((parent-is "else_block") parent-bol svelte-ts-mode-indent-offset)
     ((parent-is "else_if_block") parent-bol svelte-ts-mode-indent-offset)
     ((parent-is "await_statement") parent-bol svelte-ts-mode-indent-offset)
     ((parent-is "then_block") parent-bol svelte-ts-mode-indent-offset)
     ((parent-is "catch_block") parent-bol svelte-ts-mode-indent-offset))
    (css . ,(append (alist-get 'css css--treesit-indent-rules)
                    '(((parent-is "stylesheet") column-0 0))))
    ;;(typescript . ,(append '(((parent-is "program") column-0 0))
    ;;			   (alist-get 'typescript (typescript-ts-mode--indent-rules 'typescript)))))
    (typescript . ,(alist-get 'typescript (typescript-ts-mode--indent-rules 'typescript))))
  "Tree-sitter indentation rules for `svelte-ts-mode'.")

(defun svelte-ts-mode--prefix-font-lock-features (prefix settings)
  "Prefix with PREFIX the font lock features in SETTINGS."
  (mapcar (lambda (setting)
            (list (nth 0 setting)
                  (nth 1 setting)
                  (intern (format "%s-%s" prefix (nth 2 setting)))
                  (nth 3 setting)))
          settings))

(defvar svelte-ts-mode--font-lock-settings
  (append
   (svelte-ts-mode--prefix-font-lock-features
    "typescript"
    (typescript-ts-mode--font-lock-settings 'typescript))
   (svelte-ts-mode--prefix-font-lock-features "css" css--treesit-settings)
   (treesit-font-lock-rules
    :language 'svelte
    :feature 'svelte-comment
    '((comment) @font-lock-comment-face)

    :language 'svelte
    :feature 'svelte-definition
    '((tag_name) @font-lock-function-name-face)

    :language 'svelte
    :feature 'svelte-string
    '((quoted_attribute_value) @font-lock-string-face
      (attribute_name) @font-lock-constant-face)

    :language 'svelte
    :feature 'svelte-bracket
    '((["<" ">" "</" "/>" "{" "}"]) @font-lock-bracket-face)

    :language 'svelte
    :feature 'svelte-tag
    '((script_element
       [(start_tag (tag_name) @font-lock-doc-face)
	(end_tag (tag_name) @font-lock-doc-face)]))

    :language 'svelte
    :feature 'svelte-tag
    '([(start_tag (tag_name) @font-lock-function-call-face)
       (self_closing_tag (tag_name) @font-lock-function-call-face)
       (end_tag (tag_name)  @font-lock-function-call-face)])

    :language 'svelte
    :feature 'svelte-raw
    '((svelte_raw_text) @font-lock-doc-face)

    :language 'typescript
    :feature 'typescript-expression
    '((binary_expression
       left: (identifier) @font-lock-variable-name-face
       right: (number) @font-lock-number-face))))
  "Tree-sitter font-lock settings for `svelte-ts-mode'.")

;; NOTE: in emacs 30 there is an option :local 't to make parts of the ranges
;; local. This solves the issue of overlapping and extended ranges.

(defvar svelte-ts-mode--range-settings
  (treesit-range-rules
   :embed 'typescript
   :host 'svelte
   '((script_element (raw_text) @cap))

   :embed 'typescript
   :host 'svelte
   :local t
   '((expression (svelte_raw_text) @cap))

   :embed 'css
   :host 'svelte
   '((style_element (raw_text) @cap))

   :embed 'css
   :host 'svelte
   :local t
   '((attribute
      ((attribute_name) @_name (:match "style" @_name))
      (quoted_attribute_value (attribute_value) @capture)))))

(defun svelte-ts-mode--advice-for-treesit-buffer-root-node (&optional lang)
  "Return the current ranges for the LANG parser in the current buffer.

If LANG is omitted, return ranges for the first language in the parser list.

If `major-mode' is currently `svelte-ts-mode', or if LANG is 'svelte, this function
instead always returns t."
  (if (or (eq lang 'svelte) (not (eq major-mode 'svelte-ts-mode)))
      t
    (treesit-parser-included-ranges
     (treesit-parser-create
      (or lang (treesit-parser-language (car (treesit-parser-list))))))))

(defun svelte-ts-mode--advice-for-treesit--merge-ranges (_ new-ranges _ _)
  "Return truthy if `major-mode' is `svelte-ts-mode', and if NEW-RANGES is non-nil."
  (and (eq major-mode 'svelte-ts-mode) new-ranges))

(defun svelte-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (when (equal (treesit-node-type node) "tag_name")
    (treesit-node-text node t)))

(defun svelte-ts-mode--treesit-language-at-point (point)
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
    (or language-in-range 'svelte)))

;; TODO Make this for for JS, TS, CSS, Less, SCSS, etc. basically all the thing
;; that are legal in Svelte.

;;;###autoload
(define-derived-mode svelte-ts-mode html-mode "Svelte"
  "Major mode for editing Svelte templates, powered by tree-sitter."
  :group 'svelte

  (unless (treesit-ready-p 'svelte)
    (error "Tree-sitter grammar for Svelte isn't available"))

  (unless (treesit-ready-p 'css)
    (error "Tree-sitter grammar for CSS isn't available"))

  (unless (treesit-ready-p 'typescript)
    (error "Tree-sitter grammar for Typescript/TYPESCRIPT isn't available"))

  (when (treesit-ready-p 'typescript)
    (treesit-parser-create 'svelte)

    ;; Comments and text content
    (setq-local treesit-text-type-regexp
		(regexp-opt '("comment" "text")))

    ;; Indentation rules
    (setq-local treesit-simple-indent-rules svelte-ts-mode--indent-rules
		css-indent-offset svelte-ts-mode-indent-offset)

    ;; Font locking
    (setq-local treesit-font-lock-settings svelte-ts-mode--font-lock-settings
		treesit-font-lock-feature-list
		'((svelte-comment svelte-definition css-selector svelte-tag svelte-raw
				  css-comment css-query css-keyword typescript-comment
				  typescript-declaration)
                  (svelte-string css-property css-constant css-string typescript-keyword
				 typescript-string typescript-escape-sequence)
                  (css-error css-variable css-function css-operator typescript-constant
                             typescript-expression typescript-identifier typescript-pattern
                             typescript-property)
                  (svelte-bracket css-bracket typescript-function typescript-bracket
				  typescript-delimiter)))

    ;; Embedded languages
    (setq-local treesit-range-settings svelte-ts-mode--range-settings
		treesit-language-at-point-function
		#'svelte-ts-mode--treesit-language-at-point)

    (treesit-major-mode-setup)))

(if (treesit-ready-p 'svelte)
    (add-to-list 'auto-mode-alist '("\\.svelte\\'" . svelte-ts-mode)))

;; HACK: treesit-buffer-root-node seems to be returning a node spanning the
;;       whole file if treesit-parser-included-ranges returns nil for that
;;       language (ie. that language doesn't appear in the file). This screws
;;       with font locking, causing CSS syntax highlighting to be applied over
;;       the whole file if there's no <style> tag. To work around this, we
;;       advise treesit-buffer-root-node to make it return nil if there's no
;;       range for the language, instead of a node covering the file. I haven't
;;       seen any adverse effects come out of this, and I've done my best to
;;       make sure this stays isolated to svelte-ts-mode buffers, so hopefully
;;       nothing explodes too hard. I feel like this is a bug in treesit tbh,
;;       I'll have to report it there. But yeah, I'm so sorry about this. This
;;       is awful, I know. I hate it too. I don't know what else to do though.
(advice-add
 #'treesit-buffer-root-node
 :before-while
 #'svelte-ts-mode--advice-for-treesit-buffer-root-node)

;; HACK: treesit--merge-ranges doesn't properly account for when new-ranges is
;;       nil (ie. the code block covering that range was deleted), and returns
;;       old-ranges when it should probably also be returning nil. As a result,
;;       syntax highlighting from the old language sticks around and tries to
;;       apply itself to whatever takes it's place, which is usually a different
;;       language. This looks weird. We can work around this by advising
;;       treesit--merge-ranges to just short circuit and return nil if
;;       new-ranges is also nil. Another bug in treesit to report.
(advice-add
 #'treesit--merge-ranges
 :before-while
 #'svelte-ts-mode--advice-for-treesit--merge-ranges)

(provide 'svelte-ts-mode)
;;; svelte-ts-mode.el ends here
