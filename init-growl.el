(require 'todochiku) ;; growl notifications when compilation finishes
(setq todochiku-icons-directory (expand-file-name "~/.emacs.d/site-lisp/todochiku-icons"))
(setq todochiku-compile-message nil)
(setq todochiku-tooltip-too t)
(setq compilation-finish-functions nil)

(defun fg/notify-finished-compilation (buf msg)
  (todochiku-message "*compilation*"
                     msg
                     (todochiku-icon 'emacs)))

(add-hook 'compilation-finish-functions
          'fg/notify-finished-compilation)

(provide 'init-growl)
