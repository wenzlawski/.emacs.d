;; Note: put `rga' in your PATH.  -*- lexical-binding: t; -*-
(require 'consult)

(defcustom consult-ripgrep-all-args
  "rga --null --line-buffered --color=never --max-columns=1000 --path-separator /\  --smart-case --no-heading --with-filename --line-number"
  "Command line arguments for ripgrep, see `consult-ripgrep-all'.
The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :type '(choice string (repeat (choice string expression))))


(defun consult--ripgrep-all-make-builder (paths)
  "Create ripgrep command line builder given PATHS."
  (let* ((cmd (consult--build-args consult-ripgrep-all-args))
         (type (if (consult--grep-lookahead-p (car cmd) "-P") 'pcre 'extended)))
    (lambda (input)
      (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                   (flags (append cmd opts))
                   (ignore-case
                    (and (not (or (member "-s" flags) (member "--case-sensitive" flags)))
                         (or (member "-i" flags) (member "--ignore-case" flags)
                             (and (or (member "-S" flags) (member "--smart-case" flags))
                                  (let (case-fold-search)
                                    ;; Case insensitive if there are no uppercase letters
                                    (not (string-match-p "[[:upper:]]" arg))))))))
        (if (or (member "-F" flags) (member "--fixed-strings" flags))
            (cons (append cmd (list "-e" arg) opts paths)
                  (apply-partially #'consult--highlight-regexps
                                   (list (regexp-quote arg)) ignore-case))
          (pcase-let ((`(,re . ,hl) (funcall consult--regexp-compiler arg type ignore-case)))
            (when re
              (cons (append cmd (and (eq type 'pcre) '("-P"))
                            (list "-e" (consult--join-regexps re type))
                            opts paths)
                    hl))))))))

;;;###autoload
(defun consult-ripgrep-all (&optional dir initial)
  "Search with `rga' for files in DIR where the content matches a regexp.
The initial input is given by the INITIAL argument. See `consult-grep'
for more details."
  (interactive "P")
  (consult--grep "Ripgrep-all" #'consult--ripgrep-all-make-builder dir initial))


(provide 'consult-ripgrep-all)
