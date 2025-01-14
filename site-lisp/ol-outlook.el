;;; OL-OUTLOOK ---   -*- lexical-binding: t; -*-
;;
;; Author: Marc Wenzlawski <marcwenzlawski@posteo.com>
;; Copyright © 2025, Marc Wenzlawski, all rights reserved.
;; Created:  9 January 2025
;;
;;; Commentary:
;;
;;  
;;
;;; Code:

(require 'ol)

(org-link-set-parameters "outlook"
                         :follow #'org-outlook-open
                         :export #'org-outlook-export)

(defcustom org-outlook-command "outlook.exe"
  "The Emacs command to be used to display a outlook link."
  :group 'org-link
  :type 'str)

;; TODO change this to the outlook args. Maybe even use the powershell outlook:
(defcustom org-outlook-args '("/recycle" "/select")
  "Outlook args."
  :group 'org-link
  :type 'list)

(defun org-outlook-open (path _)
  "Visit the outlook link on PATH.
PATH should be a topic that can be thrown at the outlook command."
  (eval `(call-process ,org-outlook-command nil 0 nil ,@org-outlook-args ,(format "outlook:%s" path))))

(defun org-outlook-export (link description format _)
  "Export a outlook link from Org files."
  (let ((path (format "http://outlook.com/?topic=%s&section=all" link))
        (desc (or description link)))
    (pcase format
      (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
      (`latex (format "\\href{%s}{%s}" path desc))
      (`texinfo (format "@uref{%s,%s}" path desc))
      (`ascii (format "%s (%s)" desc path))
      (t path))))

(provide 'ol-outlook)
;;; ol-outlook.el ends here
