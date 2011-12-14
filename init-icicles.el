(eval-after-load "ring" '(progn (require 'ring+)))

;; explicitly defining this to exclude `bbdb-complete-name'
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

(setq icicle-help-in-mode-line-delay 0)
(setq icicle-Completions-text-scale-decrease 0.0)
(setq icicle-image-files-in-Completions t)
(setq icicle-completions-format 'vertical)
(setq icicle-candidate-action-keys '([C-return]))


(provide 'init-icicles)
