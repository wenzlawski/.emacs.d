;;; setup-mini-frame -- Setting up mini frame  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(use-package mini-frame
  :straight t
  :custom
  (mini-frame-ignore-commands
   '("edebug-eval-expression"
     ctrlf-forward-default
     ctrlf-backward-default
     ctrlf-forward-symbol-at-point
     ctrlf-backward-symbol-at-point
     debugger-eval-expression))
  (mini-frame-color-shift-step 0)
  
  (mini-frame-show-parameters #'my/mini-frame-show-parameters)
  :config
  (with-eval-after-load 'modus-themes
    (setopt mini-frame-background-color-function (lambda () (modus-themes-get-color-value 'bg-blue-nuanced t)))))

(with-eval-after-load 'mini-frame

  (defun my/mini-frame-show-parameters ()
    "Determine the frame parameters on show."
    (let ((params '((top . 0.25)
		    (left . 0.5)))
	  (framew (frame-width)))
      (cond
       ((when (> framew 200)
	  (push '(width . 0.6) params)))
       ((when (> framew 120)
	  (push '(width . 0.8) params)))
       (t (push '(width . 1.0) params)))))
  
  (defun my/mini-frame-update-color ()
    "Update the color of the posframe"
    (setopt mini-frame-internal-border-color (modus-themes-get-color-value 'bg-blue-nuanced t))
    (when mini-frame-internal-border-color
      (set-face-background 'child-frame-border mini-frame-internal-border-color mini-frame-frame)
      (set-face-background 'internal-border mini-frame-internal-border-color mini-frame-frame)))

  (defun my/mini-frame--make-frame (parameters)
    "Make frame with common parameters and PARAMETERS."
    (let ((frame (make-frame (append parameters
                                     '((visibility . nil)
				       (user-position . t)
				       (user-size . t)
				       (keep-ratio . t)
				       (undecorated . t)
				       (desktop-dont-save . t)
				       (child-frame-border-width . 12)
				       (internal-border-width . 3)
				       (drag-internal-border . t)
				       (z-group . above))))))
      (set-face-background 'fringe nil frame)
      (when mini-frame-internal-border-color
	(set-face-background 'child-frame-border mini-frame-internal-border-color frame)
	(set-face-background 'internal-border mini-frame-internal-border-color frame))
      (fontaine-apply-current-preset)
      frame))

  (advice-add #'mini-frame--make-frame :override #'my/mini-frame--make-frame)

  (with-eval-after-load 'modus-themes
    (setopt mini-frame-show-parameters
	    `((top . 250)
	      (width . 0.8)
	      (left . 0.5)
	      ;; (background-color . ,(modus-themes-get-color-value 'bg-blue-nuanced t))
	      ))

    (defun my/mini-frame-update-color (&optional _)
      "Update the color of the posframe"
      (when mini-frame-mode
	(setopt mini-frame-internal-border-color (modus-themes-get-color-value 'bg-blue-nuanced t))
	(when (and mini-frame-internal-border-color mini-frame-frame)
	  (set-face-background 'child-frame-border mini-frame-internal-border-color mini-frame-frame)
	  (set-face-background 'internal-border mini-frame-internal-border-color mini-frame-frame))))
    
    (add-hook 'modus-themes-post-load-hook #'my/mini-frame-update-color)
    (add-to-list 'after-make-frame-functions #'my/mini-frame-update-color)
    (my/mini-frame-update-color)))

(provide 'setup-mini-frame)
;;; setup-mini-frame.el ends here
