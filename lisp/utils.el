;;; utils.el --- Utility functions -*- lexical-binding: t; -*-

(defun strip-emoji (str)
  "Remove characters which are part of the `emoji' script from STR."
  (cl-remove-if (lambda (c)
                  (equal (aref char-script-table c) 'emoji))
                str))

(provide 'utils)
;;; utils.el ends here
