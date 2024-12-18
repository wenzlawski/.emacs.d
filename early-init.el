;;; early-init.el ---   -*- lexical-binding: t -*-

;; Copyright (C) 2022 Marc Wenzlawski
;;
;; Keywords: keyword1 keyword2
;; Author: Marc Wenzlawski <marc.wenzlawski@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defconst is-darwin (eq system-type 'darwin))
(defconst is-win (eq system-type 'windows-nt))
(defconst is-linoux (eq system-type 'gnu/linux))

;; Temporarily increase the garbage collection threshold.  These
;; changes help shave off about half a second of startup time.  The
;; `most-positive-fixnum' is DANGEROUS AS A PERMANENT VALUE.  See the
;; `emacs-startup-hook' a few lines below for what I actually use.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Initialise installed packages at this early stage, by using the
;; available cache.  I had tried a setup with this set to nil in the
;; early-init.el, but (i) it ended up being slower and (ii) various
;; package commands, like `describe-package', did not have an index of
;; packages to work with, requiring a `package-refresh-contents'.
(setq package-enable-at-startup t
      package-quickstart t
      load-prefer-newer t)

(unless (or (daemonp) noninteractive)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    ;; `file-name-handler-alist' is consulted on each `require', `load' and
    ;; various path/io functions. You get a minor speed up by unsetting this.
    ;; Some warning, however: this could cause problems on builds of Emacs where
    ;; its site lisp files aren't byte-compiled and we're forced to load the
    ;; *.el.gz files (e.g. on Alpine).
    (setq-default file-name-handler-alist nil)
    ;; ...but restore `file-name-handler-alist' later, because it is needed for
    ;; handling encrypted or compressed files, among other things.
    (defun my/reset-file-handler-alist ()
      (setq file-name-handler-alist
            ;; Merge instead of overwrite because there may have bene changes to
            ;; `file-name-handler-alist' since startup we want to preserve.
            (delete-dups (append file-name-handler-alist
                                 old-file-name-handler-alist))))
    (add-hook 'emacs-startup-hook #'my/reset-file-handler-alist 101))

  ;; Without this, Emacs will try to resize itself to a specific column size
  (setq frame-inhibit-implied-resize t)

  ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
  ;; No second pass of case-insensitive search over auto-mode-alist.
  (setq auto-mode-case-fold nil)

  ;; Reduce *Message* noise at startup. An empty scratch buffer (or the
  ;; dashboard) is more than enough, and faster to display.
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name)
  (setq initial-buffer-choice nil
        inhibit-startup-buffer-menu t
        inhibit-x-resources t)

  ;; Disable bidirectional text scanning for a modest performance boost.
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)

  ;; Shave seconds off startup time by starting the scratch buffer in
  ;; `fundamental-mode'
  (setq initial-major-mode 'lisp-interaction-mode
        initial-scratch-message nil)


  (setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redisplay)))

  ;; Site files tend to use `load-file', which emits "Loading X..." messages in
  ;; the echo area, which in turn triggers a redisplay. Redisplays can have a
  ;; substantial effect on startup times and in this case happens so early that
  ;; Emacs may flash white while starting up.
  (define-advice load-file (:override (file) silence)
    (load file nil 'nomessage))

  ;; Undo our `load-file' advice above, to limit the scope of any edge cases it
  ;; may introduce down the road.
  (define-advice startup--load-user-init-file (:before (&rest _) nomessage-remove)
    (advice-remove #'load-file #'load-file@silence)))

;; This is bound to MacOS. In the future, there should be an OS
;; agnostic way of calling these functions. This would be handled by
;; making a boolean checking for OS type at startup and then assigning
;; respective variables to the desktop commands. That way we won't
;; have to constantly do the checking for OS, just do it once.
(defun my/theme-gsettings-dark-p ()
  "Return non-nil if defaults (MACOS) has a dark theme.
Return nil if the OS is not darwin"
  (if (eq system-type 'darwin)
      (string-match-p
       "Dark"
       (shell-command-to-string "defaults read -g AppleInterfaceStyle"))))

(defun my/avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs, if needed.
New frames are instructed to call `prot-emacs-re-enable-frame-theme'."
  (when (my/theme-gsettings-dark-p)
    (set-face-attribute 'default nil :background "#000000"
                        :foreground "#ffffff")
    ;; (set-face-attribute 'mode-line nil :background "#000000"
    ;;                     :foreground "#ffffff" :box 'unspecified)
    ))

(setq mode-line-format nil)
(my/avoid-initial-flash-of-light)

(setq default-frame-alist
      (append
       default-frame-alist
       '((height . 80)
	 (width . 250)
	 ;; (fullscreen . maximized)
	 (title . " \n")
	 (name . " \n")
	 (right-divider-width . 2)
	 (bottom-divider-width . 0)
	 (internal-border-width . 8)
	 (menu-bar-lines . 0)
	 (tool-bar-lines . 0)
	 (vertical-scroll-bars)
	 (undecorated-round . t))))

;; Some features that are not represented as packages can be found in
;; `features', but this can be inconsistent. The following enforce consistency:
(if (fboundp #'json-parse-string)
    (push 'jansson features))
(if (string-match-p "HARFBUZZ" system-configuration-features) ; no alternative
    (push 'harfbuzz features))
(if (bound-and-true-p module-file-suffix)
    (push 'dynamic-modules features))

;; Increase how much is read from processes in a single chunk (default is 4kb).
(setq read-process-output-max (* 512 1024)) ; 512kb

;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)

(setq warning-suppress-types '((lexical-binding)))

;; By default, Emacs "updates" its ui more often than it needs to
(setq which-func-update-delay 1.0)

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      ring-bell-function 'ignore
      use-dialog-box nil ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t
      use-package-enable-imenu-support t)

;; Suppress compiler warnings and don't inundate users with their popups.
(setq native-comp-async-report-warnings-errors 'silent)

(setq native-comp-warning-on-missing-source nil)

(setq debug-on-error nil
      jka-compr-verbose nil)

(setq byte-compile-warnings nil)
(setq byte-compile-verbose nil)


;; Disable GUI elements
(menu-bar-mode -1)
(add-hook 'after-init-hook (lambda nil (menu-bar-mode -1)))
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-language-environment "UTF-8")

;; Same idea as above for the `file-name-handler-alist' and the
;; `vc-handled-backends' with regard to startup speed optimisation.
;; Here I am storing the default value with the intent of restoring it
;; via the `emacs-startup-hook'.
(defvar my/file-name-handler-alist file-name-handler-alist)
(defvar my/vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 20)
                  gc-cons-percentage 0.2
                  file-name-handler-alist my/file-name-handler-alist
                  vc-handled-backends my/vc-handled-backends)))

;; Prevent unwanted runtime builds in gccemacs (native-comp); packages are
;; compiled ahead-of-time when they are installed and site files are compiled
;; when gccemacs is installed.
(setq comp-deferred-compilation nil)

(defun dir-concat (dir file)
  "Join path DIR with filename FILE correctly."
  (concat (file-name-as-directory dir) file))

(byte-recompile-directory (dir-concat user-emacs-directory "site-lisp") 0)

;;; early-init.el ends here
