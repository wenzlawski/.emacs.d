;;; SETUP-EGLOT --- eglot  -*- lexical-binding: t; -*-
;;
;; Author: Marc Wenzlawski <marc.wenzlawski@icloud.com>
;; Copyright © 2024, Marc Wenzlawski, all rights reserved.
;; Created:  9 July 2024
;;
;;; Commentary:
;;
;;  
;;
;;; Code:

(use-package eglot
  :straight t
  :hook
  (eglot-managed-mode . (lambda ()
			  (cond ((derived-mode-p 'python-base-mode)
				 (add-hook 'flymake-diagnostic-functions 'python-flymake nil t))
				(t nil))))
  :bind
  (:map eglot-mode-map
	("C-c e f" . eglot-format)
	("C-c e q" . eglot-shutdown)
	("C-c e Q" . eglot-shutdown-all)
	("C-c e l" . eglot-list-connections)
	("C-c e r" . eglot-rename)
	("C-c e i" . eglot-inlay-hints-mode))
  :init
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))
  :config
  (dolist (mode
	   '(
	     ;; (nix-mode . ("nixd"))
	     ((svelte-mode svelte-ts-mode) . ("svelteserver" "--stdio"))
	     ((zig-mode zig-ts-mode) . ("zls"))
	     ))
    (add-to-list 'eglot-server-programs mode))

  (set-face-attribute
   'eglot-highlight-symbol-face nil
   :bold t :underline nil :background (modus-themes-get-color-value 'bg-yellow-intense)))

(use-package emacs-lsp-booster
  :after eglot
  :straight (:host github :repo "blahgeek/emacs-lsp-booster"))

(use-package eglot-booster
  :straight (:host github :repo "jdtsmith/eglot-booster")
  :after eglot emacs-lsp-booster
  :config
  (eglot-booster-mode))

(with-eval-after-load 'eglot
  (defun my/eglot-capf ()
    (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
    (setq-local completion-at-point-functions
		(list (cape-capf-super
		       #'yasnippet-capf
		       #'tempel-expand
		       #'eglot-completion-at-point
		       #'cape-file))))

  (with-eval-after-load 'cape
    (add-hook 'eglot-managed-mode-hook #'my/eglot-capf))

  (defun my/eglot-apply-faces ()
    "Apply eglot faces"
    (set-face-attribute
     'eglot-highlight-symbol-face nil
     :bold t :underline nil :background (modus-themes-get-color-value 'bg-yellow-intense)))

  (with-eval-after-load 'modus-themes
    (add-hook 'modus-themes-after-load-theme-hook #'my/eglot-apply-faces)
    (my/eglot-apply-faces)))

(provide 'setup-eglot)
;;; setup-eglot.el ends here
