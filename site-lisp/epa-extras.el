;;; epa-extras.el -*- lexical-binding: t; -*-
;;
;; Author: Marc Wenzlawski
;; Created: 2025-09-05
;; Last Modified: Time-stamp: "2025-09-05 15:14:43 Marc Wenzlawski"
;;
;;; Changelog:
;;
;;  
;;
;;; Commentary:
;;
;;  TODO Make this into global-minor-mode and local-minor-mode options
;;  global-minor-mode enables it for all
;;  local enables it for certain gpg files
;;
;;; Code:

(require 'epa)
(require 'epa-hook)

(defvar epa-extras-quit-idle-encrypted-buffers-timer nil "Timer for cleaning idle encrypted buffers.")

(defvar epa-extras-quit-encrypted-buffers-timer nil "Timer for cleaning encrypted buffers on emacs idle.")

(defcustom epa-extras-epa-buffer-max-idle-seconds (* 60 60)
  "Max idle time for encrypted buffers in seconds."
  :type 'int :group 'epa-extras)

(defcustom epa-extras-emacs-max-idle-seconds (* 60 10)
  "Max idle time for Emacs after which encrypted buffers are quit."
  :type 'int :group 'epa-extras)


(defvar buffer-last-change-time nil "The last change time")
(make-variable-buffer-local 'buffer-last-change-time)

(defun epa-extras-clean-encrypted-buffers ()
  (dolist (buf (match-buffers epa-file-name-regexp))
    (when (> (buffer-local-value 'buffer-last-change-time buf) epa-extras-epa-buffer-max-idle-seconds)
      (with-current-buffer buf
        (save-buffer)
	(kill-buffer)))))

(defun buffer-record-last-change-time (beg end len)
  (setq buffer-last-change-time (current-time)))

(defun epa-extras-kill-all-encrypted-buffers ()
  (interactive)
  (save-some-buffers t (lambda () (and (buffer-file-name) (epa-file-name-p (buffer-file-name)))))
  (kill-matching-buffers-no-ask epa-file-name-regexp)
  (message "Killed all encrypted buffers."))

(defun epa-extras-clean-buffers-init ()
  (add-to-list 'after-change-functions 'buffer-record-last-change-time)
  (setq epa-extras-quit-idle-encrypted-buffers-timer (run-with-timer epa-extras-epa-buffer-max-idle-seconds epa-extras-epa-buffer-max-idle-seconds #'epa-extras-clean-encrypted-buffers)
	epa-extras-quit-encrypted-buffers-timer (run-with-idle-timer epa-extras-emacs-max-idle-seconds t #'epa-extras-kill-all-encrypted-buffers)))

(provide 'epa-extras)
;;; usql.el ends here
