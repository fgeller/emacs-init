(require 'todochiku) ;; growl notifications when compilation finishes
(setq todochiku-icons-directory (expand-file-name "~/.emacs.d/site-lisp/todochiku-icons"))
(setq todochiku-compile-message nil)

(add-hook 'compilation-finish-functions
          (lambda (buf msg)
            (todochiku-message "*compilation*"
                               msg
                               (todochiku-icon 'emacs)
                               t)))

(provide 'init-growl)
