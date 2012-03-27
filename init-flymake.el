;; don't want this on Mac OS X -- http://koansys.com/tech/emacs-hangs-on-flymake-under-os-x
(setq flymake-gui-warnings-enabled nil)
(setq flymake-start-syntax-check-on-newline t)
(setq flymake-compilation-prevents-syntax-check nil)

(require 'flymake)
(require 'flymake-cursor)

;; I want my copies in the system temp dir.
(setq flymake-run-in-place nil)

(provide 'init-flymake)
