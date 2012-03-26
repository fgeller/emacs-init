(setq todochiku-icons-directory (expand-file-name "~/.emacs.d/site-lisp/todochiku-icons"))
(setq todochiku-compile-message nil)
(setq todochiku-tooltip-too t)
(setq compilation-finish-functions nil)

(defun fg/notify-finished-compilation (buf msg)
  (todochiku-message (format "Compilation in %s:" buf)
                     msg
                     (todochiku-icon 'emacs)))

(add-hook 'compilation-finish-functions
          'fg/notify-finished-compilation)

(require 'todochiku) ;; growl notifications when compilation finishes

(provide 'init-growl)
