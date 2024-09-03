;;; zig-ts-mode.el --- tree-sitter support for Zig   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Marc Wenzlawski

;; Author: Marc Wenzlawski <marcwenzlawski@gmail.com>
;; Keywords: languages tree-sitter zig

;; This program is free software; you can redistribute it and/or modify
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

;; TODO: format strings a la "{d:0>8}" and such

;; This package provides tree-sitter support for Zig.

;;; Code:


(require 'treesit)
(require 'c-ts-common)
(eval-when-compile 'rx)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-child "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-prev-sibling "treesit.c")
(declare-function treesit-node-first-child-for-pos "treesit.c")
(declare-function treesit-node-next-sibling "treesit.c")
(declare-function treesit-parser-set-included-ranges "treesit.c")
(declare-function treesit-query-compile "treesit.c")

;;; Custom variables

(defcustom zig-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `zig-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'zig)

(defcustom zig-zig-bin "zig"
  "Path to zig executable."
  :type 'file
  :safe #'stringp)

(defcustom zig-run-optimization-mode "Debug"
  "Optimization mode to run code with."
  :type 'string
  :safe #'stringp)

(defcustom zig-test-optimization-mode "Debug"
  "Optimization mode to run tests with."
  :type 'string
  :safe #'stringp)


;; zig CLI commands

(defun zig--run-cmd (cmd &optional source &rest args)
  "Use compile command to execute a zig CMD with ARGS if given.
If given a SOURCE, execute the CMD on it."
  (let ((cmd-args (if source (cons source args) args)))
    (projectile-save-project-buffers)
    (compilation-start (mapconcat 'shell-quote-argument
                                  `(,zig-zig-bin ,cmd ,@cmd-args) " "))))

;;;###autoload
(defun zig-compile ()
  "Compile using `zig build`."
  (interactive)
  (zig--run-cmd "build"))

;;;###autoload
(defun zig-build-exe ()
  "Create executable from source or object file."
  (interactive)
  (zig--run-cmd "build-exe" (file-local-name (buffer-file-name))))

;;;###autoload
(defun zig-build-lib ()
  "Create library from source or assembly."
  (interactive)
  (zig--run-cmd "build-lib" (file-local-name (buffer-file-name))))

;;;###autoload
(defun zig-build-obj ()
  "Create object from source or assembly."
  (interactive)
  (zig--run-cmd "build-obj" (file-local-name (buffer-file-name))))

;;;###autoload
(defun zig-test-buffer ()
  "Test buffer using `zig test`."
  (interactive)
  (projectile-save-project-buffers)
  (zig--run-cmd "test" (file-local-name (buffer-file-name)) "-O" zig-test-optimization-mode))

;;;###autoload
(defun zig-run ()
  "Create an executable from the current buffer and run it immediately."
  (interactive)
  (zig--run-cmd "run" (file-local-name (buffer-file-name)) "-O" zig-run-optimization-mode))

;;;###autoload
(defun zig-docs-serve ()
  "Serve the zig std docs."
  (interactive)
  (start-process "zig-docs" "*zig-docs*" "zig" "std"))

;;;###autoload
(defun zig-docs-kill ()
  "Stop the zig std server."
  (interactive)
  (kill-buffer "*zig-docs*"))

(defun zig-docs--get-url (buffer)
  "Get the url from the buffer."
  (with-current-buffer
      buffer
    (goto-char (point-min))
    (buffer-substring-no-properties (point-min) (line-end-position))))

;;;###autoload
(defun zig-docs-open ()
  "Stop the zig std server."
  (interactive)
  (if-let ((buffer (get-buffer "*zig-docs*")))
      (browse-url-default-browser (zig-docs--get-url buffer))
    (zig-docs-serve)))

(defvar zig-docs--toc nil)

(defcustom zig-docs--toc-url "https://ziglang.org/documentation/" "Default url for the Zig Documentation.")

(defcustom zig-docs--toc-version
  "master"
  "Version of zig docs."
  :options '("0.1.1" "0.2.0" "0.3.0" "0.4.0" "0.5.0" "0.6.0" "0.7.1" "0.8.1" "0.9.1" "0.10.1" "0.11.0" "0.12.0" "0.13.0" "master" ))

(defun zig-docs--toc-make-url (&optional path)
  "Make the url."
  (concat zig-docs--toc-url zig-docs--toc-version "/" path))

(defun zig-docs--toc-construct-element (el)
  "Construct the element."
  (list (intern (dom-text el)) (alist-get 'href (dom-attributes el))))

(defun zig-docs--toc-retrieve ()
  "Retrieve the zig docs toc."
  (require 'dom)
  (let ((buffer (url-retrieve-synchronously (zig-docs--toc-make-url))))
    (with-current-buffer buffer
      (let* ((tree (libxml-parse-html-region (point-min) (point-max)))
	     (as (dom-by-tag (nth 2 (dom-children (car (dom-by-id tree "^navigation$")))) 'a))
	     (strings (mapcar #'zig-docs--toc-construct-element as)))
	(setq zig-docs--toc strings)
	(with-temp-buffer
	  (insert (format "%S" strings))
	  (write-file (dir-concat user-emacs-directory (format "zig-docs-toc-%s.eld" zig-docs--toc-version))))))))

(defun zig-docs--toc-init ()
  "Init zig docs toc."
  (let ((file (zig-docs--toc-make-url)))
    (if (file-exists-p file)
	(let (buffer (find-file-noselect file))
	  (setq zig-docs--toc (read buffer))
	  (kill-buffer buffer))
      (zig-docs--toc-retrieve))))

;;;###autoload
(defun zig-docs-open-section (&optional intern)
  "Interactively open a section of the Zig Docs."
  (interactive "P")
  (unless zig-docs--toc (zig-docs--toc-init))
  (let* ((section (completing-read "Section:" zig-docs--toc))
	 (path (cadr (assoc (intern section) zig-docs--toc)))
	 (browse (if intern #'eww #'browse-url-default-browser)))
    (apply browse (zig-docs--toc-make-url path) nil)))

;;;###autoload
(defun zig-docs-lang-ref (&optional intern)
  "Open the language reference."
  (interactive "P")
  (let ((url (zig-docs--toc-make-url))
	(browse (if intern #'eww #'browse-url-default-browser)))
    (apply browse url nil)))

;;; Syntax table

(defvar zig-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?+   "."      table)
    (modify-syntax-entry ?-   "."      table)
    (modify-syntax-entry ?=   "."      table)
    (modify-syntax-entry ?%   "."      table)
    (modify-syntax-entry ?&   "."      table)
    (modify-syntax-entry ?|   "."      table)
    (modify-syntax-entry ?!   "."      table)
    (modify-syntax-entry ?@   "."      table)
    (modify-syntax-entry ?<   "."      table)
    (modify-syntax-entry ?>   "."      table)
    (modify-syntax-entry ?/   ". 12"   table)
    (modify-syntax-entry ?*   "."      table)
    (modify-syntax-entry ?'   "\""     table)
    (modify-syntax-entry ?\"  "\""     table)
    (modify-syntax-entry ?\\  "\\"     table)
    (modify-syntax-entry ?\n  ">"      table)
    table)
  "Syntax table for `rust-ts-mode'.")

;;; Indent

(defvar zig-ts-mode--indent-rules
  `((zig
     ((parent-is "source_file") column-0 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") (and parent parent-bol) 0)
     ((parent-is "ParamDeclList") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "InitList") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "ContainerDecl") parent-bol zig-ts-mode-indent-offset)
     ((parent-is "SwitchExpr") parent-bol zig-ts-mode-indent-offset)))
  "Tree-sitter indent rules for `zig-ts-mode'.")

(defvar zig-ts-mode--keywords
  '("break" "return" "continue" "asm" "defer" "errdefer" "unreachable"
    "try" "catch" "async" "nosuspend" "await" "suspend" "resume"
    "const" "var" "extern" "packed" "export" "pub" "noalias" "inline"
    "noinline" "comptime" "callconv" "volatile" "allowzero"
    "align" "linksection" "threadlocal" "addrspace"
    "struct" "enum" "union" "error" "opaque"
    "if" "else" "switch" "and" "or" "orelse"
    "while" "for" "fn" "usingnamespace" "test"
    )
  "Zig keywords for tree-sitter font-locking.")

(defvar zig-ts-mode--operators '("=" "+" "*" "**" "=>" ".?" ".*" "?" "/" "%" "&" "|" "!" "<" ">")
  "Zig operators for tree-sitter font-locking.")

(setq zig-ts-mode--font-lock-settings
      (treesit-font-lock-rules
       :language 'zig
       :feature 'bracket
       '([
	  "[" "]" "(" ")" "{" "}"
	  (Payload "|")
	  (PtrPayload "|")
	  (PtrIndexPayload "|")
	  ] @font-lock-bracket-face)

       :language 'zig
       :feature 'punctuation
       '([ ".." "..." ] @font-lock-misc-punctuation-face)

       :language 'zig
       :feature 'builtin
       '(([(BUILTINIDENTIFIER)]) @font-lock-builtin-face)

       :language 'zig
       :feature 'comment
       '(([(line_comment) (doc_comment) (container_doc_comment)]) @font-lock-comment-face)

       :language 'zig
       :feature 'delimiter
       '((["," "." ";" ":"]) @font-lock-delimiter-face)

       :language 'zig
       :feature 'keyword
       `([,@zig-ts-mode--keywords] @font-lock-keyword-face)

       :language 'zig
       :feature 'number
       '([(FLOAT) (INTEGER)] @font-lock-number-face)

       :language 'zig
       :feature 'function
       '((SuffixExpr
	  [
	   variable_type_function: (IDENTIFIER) @font-lock-property-use-face
	   field_constant: (IDENTIFIER) @font-lock-function-call-face])
	 (FieldOrFnCall
          [
	   field_access: (IDENTIFIER) @font-lock-property-use-face
	   function_call: (IDENTIFIER) @font-lock-function-call-face])
	 (FnProto
	  function: (IDENTIFIER) @font-lock-function-call-face))
       
       :language 'zig
       :feature 'assignment
       '((VarDecl variable_type_function: (_) @font-lock-variable-name-face)
	 (PtrListPayload variable: (IDENTIFIER) @font-lock-variable-name-face)
	 (Payload variable: (IDENTIFIER) @font-lock-variable-name-face))

       :language 'zig
       :feature 'operator
       `([(CompareOp)
	  (BitwiseOp)
	  (BitShiftOp)
	  (AdditionOp)
	  (AssignOp)
	  (MultiplyOp)
	  (PrefixOp)
	  ,@zig-ts-mode--operators] @font-lock-operator-face)

       :language 'zig
       :feature 'string
       '([(STRINGLITERALSINGLE)
	  (LINESTRING)] @font-lock-string-face)

       :language 'zig
       :feature 'type
       '([(BuildinTypeExpr)] @font-lock-type-face)

       :language 'zig
       :feature 'constant
       '(["null" "unreachable" "undefined" (CHAR_LITERAL)] @font-lock-builtin-face)

       :language 'zig
       :feature 'builtin
       '([ "true" "false" ] @font-lock-builtin-face)

       :language 'zig
       :feature 'number
       '(((INTEGER) @font-lock-number-face)
	 ((FLOAT) @font-lock-number-face))

       :language 'zig
       :feature 'builtin
       '((BUILTINIDENTIFIER) @font-lock-builtin-face)

       :language 'zig
       :feature 'constant
       '(
	 ("." field_constant: (IDENTIFIER) @font-lock-constant-face)
	 ((ErrorSetDecl field_constant: (IDENTIFIER) @font-lock-constant-face)))

       :language 'zig
       :feature 'builtin
       '((
	  ((IDENTIFIER) @font-lock-builtin-face)
	  (:equal @font-lock-builtin-face "_")
	  )
	 ((FnProto exception: "!" @font-lock-builtin-face))
	 ((PtrTypeStart "c" @font-lock-builtin-face)))

       :language 'zig
       :feature 'function
       '(((FnProto function: (IDENTIFIER) @font-lock-function-name-face))
	 ((SuffixExpr variable_type_function: (IDENTIFIER) @font-lock-function-call-face (FnCallArguments)))
	 ((FieldOrFnCall function_call: (IDENTIFIER) @font-lock-function-call-face)))

       :language 'zig
       :feature 'variable
       '(((SuffixExpr variable_type_function: (IDENTIFIER) @font-lock-variable-use-face))
	 ((VarDecl variable_type_function: (IDENTIFIER) @font-lock-variable-name-face))
	 ((ParamDecl parameter: (IDENTIFIER) @font-lock-variable-name-face))
	 ((FieldOrFnCall field_access: (IDENTIFIER) @font-lock-variable-use-face))
	 ((ContainerField field_member: (IDENTIFIER) @font-lock-variable-name-face)))

       :language 'zig
       :feature 'type
       :override t
       '(([
	   (VarDecl variable_type_function: (IDENTIFIER) @font-lock-type-face)
	   (SuffixExpr variable_type_function: (IDENTIFIER) @font-lock-type-face)
	   (ParamDecl parameter: (IDENTIFIER) @font-lock-type-face)
	   (FieldOrFnCall field_access: (IDENTIFIER) @font-lock-type-face)
	   ]
	  (:match "^[A-Z]\\([a-z]+[A-Za-z_0-9]*\\)*$" @font-lock-type-face)))

       :language 'zig
       :feature 'constant
       :override t
       '(([
	   (VarDecl variable_type_function: (IDENTIFIER) @font-lock-constant-face)
	   (SuffixExpr variable_type_function: (IDENTIFIER) @font-lock-constant-face)
	   (FieldOrFnCall field_access: (IDENTIFIER) @font-lock-constant-face)
	   ]
	  (:match "^[A-Z][A-Z_0-9]+$" @font-lock-constant-face)))
       ))


;; TODO: Is this necessary?
(defun zig-ts-mode--syntax-propertize (beg end)
  "Apply syntax properties to special characters between BEG and END.

Apply syntax properties to various special characters with
contextual meaning between BEG and END.

The apostrophe \\=' should be treated as string when used for char literals.

< and > are usually punctuation, e.g., as greater/less-than.  But
when used for types, they should be considered pairs.

This function checks for < and > in the changed RANGES and apply
appropriate text property to alter the syntax of template
delimiters < and >'s."
  (goto-char beg)
  (while (search-forward "'" end t)
    (when (string-equal "CHAR_LITERAL"
                        (treesit-node-type
                         (treesit-node-at (match-beginning 0))))
      (put-text-property (match-beginning 0) (match-end 0)
                         'syntax-table (string-to-syntax "\""))))
  (goto-char beg)
  (while (re-search-forward (rx (or "<" ">")) end t)
    (pcase (treesit-node-type
	    (treesit-node-parent
	     (treesit-node-at (match-beginning 0))))
      ((or "type_arguments" "type_parameters")
       (put-text-property (match-beginning 0)
			  (match-end 0)
			  'syntax-table
			  (pcase (char-before)
			    (?< '(4 . ?>))
			    (?> '(5 . ?<))))))))

(defun zig-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ("TestDecl"
     (treesit-node-text
      (treesit-node-child node 1) t))
    ("FnProto"
     (treesit-node-text
      (treesit-node-child-by-field-name node "function") t))
    ("ContainerDeclType"
     (treesit-node-text
      (treesit-node-child-by-field-name
       (treesit-node-get node '((parent 4))) "variable_type_function")))))

;;; Mode definition

;; taken trom c-ts-mode.el
(defvar-keymap zig-ts-mode-map
  :doc "Keymap for Zig major mode with tree-sitter."
  :parent prog-mode-map
  "C-c C-b" #'zig-compile
  "C-c C-r" #'zig-run
  "C-c C-t" #'zig-test-buffer
  "C-c C-k" #'comment-region
  "C-c C-d d" #'zig-docs-open
  "C-c C-d s" #'zig-docs-serve
  "C-c C-d k" #'zig-docs-kill
  "C-c C-d r" #'zig-docs-lang-ref)

;;;###autoload
(define-derived-mode zig-ts-mode prog-mode "Zig"
  "Major mode for editing Zig, powered by tree-sitter.

\\{zig-ts-mode-map}"
  :group 'zig
  :syntax-table zig-ts-mode--syntax-table

  (when (treesit-ready-p 'zig)
    (treesit-parser-create 'zig)

    ;; Syntax.
    (setq-local syntax-propertize-function
                #'zig-ts-mode--syntax-propertize)

    ;; Comments.
    (c-ts-common-comment-setup)

    ;; Font-lock.
    (setq-local treesit-font-lock-settings zig-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '(( comment )		; definitions
		  ( keyword string types )
		  ( assignment number builtin constant ) ; constants, literals
		  ( bracket operator function delimiter punctuation variable))) ; delimiters, punctuation, functions properties variables

    ;; Imenu.
    (setq-local
     treesit-simple-imenu-settings
     `(("Struct" "\\`ContainerDeclType\\'"
	(lambda (n) (string= "struct" (treesit-node-text n t)))
	nil)
       ("Enum" "\\`ContainerDeclType"
	(lambda (n) (string-match-p "enum" (treesit-node-text n t)))
	nil)
       ("Fn" "\\`FnProto\\'" nil nil)
       ("Test" "\\`TestDecl\\'" nil nil)))

    ;; Indent.
    (setq-local indent-tabs-mode nil
                treesit-simple-indent-rules zig-ts-mode--indent-rules)

    ;; Electric
    (setq-local electric-indent-chars
                (append "{}():;,#" electric-indent-chars))

    ;; Navigation.
    ;; FIXME: Does not work for end-of-defun
    (setq-local treesit-defun-type-regexp
                (regexp-opt '("TestDecl"
			      "FnProto")))
    (setq-local treesit-defun-name-function #'zig-ts-mode--defun-name)

    (treesit-major-mode-setup)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(zig\\|zon\\)\\'" . zig-ts-mode))

(provide 'zig-ts-mode)
;;; zig-ts-mode.el ends here
