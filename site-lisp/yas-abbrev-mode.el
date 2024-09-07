;;; YAS-ABBREV-MODE ---   -*- lexical-binding: t; -*-
;;
;; Author: Marc Wenzlawski <marc.wenzlawski@icloud.com>
;; Copyright © 2024, Marc Wenzlawski, all rights reserved.
;; Created:  7 September 2024
;;
;;; Commentary:
;;
;;  
;;
;;; Code:

(require 'yasnippet)

;;;###autoload
(define-minor-mode yas-abbrev-mode
  "Install Tempel templates as abbrevs."
  :group 'yas
  :global nil
  (setq-local abbrev-minor-mode-table-alist
              (assq-delete-all 'yas-abbrev-mode abbrev-minor-mode-table-alist))
  (when (eq abbrev-minor-mode-table-alist
            (default-value 'abbrev-minor-mode-table-alist))
    (kill-local-variable 'abbrev-minor-mode-table-alist))
  (when yas-abbrev-mode
    (abbrev-mode 1)
    (let ((table (make-abbrev-table)))
      (dolist (template (yas--all-templates (yas--get-snippet-tables major-mode)))
        (let* ((name (yas--template-key template))
               (hook #'yas-expand))
          (put hook 'no-self-insert t)
          (define-abbrev table name 'Template hook :system t)))
      (setq-local abbrev-minor-mode-table-alist
                  (cons `(yas-abbrev-mode . ,table)
                        abbrev-minor-mode-table-alist)))))

(provide 'yas-abbrev-mode)
;;; yas-abbrev-mode.el ends here
