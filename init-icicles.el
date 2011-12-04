(eval-after-load "ring" '(progn (require 'ring+)))
; explicitly defining this to exclude `bbdb-complete-name'
(setq icicle-functions-to-redefine
      '(comint-dynamic-complete
		comint-dynamic-complete-filename
		comint-replace-by-expanded-filename
		customize-apropos customize-apropos-faces
		customize-apropos-groups
		customize-apropos-options
		customize-apropos-options-of-type
		customize-face
		customize-face-other-window
		dabbrev-completion
		dired-read-shell-command
		ess-complete-object-name
		gud-gdb-complete-command
		lisp-complete-symbol
		lisp-completion-at-point
		minibuffer-default-add-completions
		read-color
		read-from-minibuffer
		read-shell-command
		read-string
		recentf-make-menu-items
		repeat-complex-command))
(require 'icicles)
(icy-mode)

(provide 'init-icicles)
