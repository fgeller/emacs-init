(setq flymake-gui-warnings-enabled t)
(setq flymake-start-syntax-check-on-newline t)

;; Stop flymake from breaking when ruby-mode is invoked by mmm-mode,
;; at which point buffer-file-name is nil
(eval-after-load 'flymake
  '(progn
         (defun flymake-can-syntax-check-file (file-name)
           "Determine whether we can syntax check FILE-NAME.
Return nil if we cannot, non-nil if we can."
           (if (and file-name (flymake-get-init-function file-name)) t nil))
         ))


(require 'flymake)
(require 'flymake-cursor)

;; I want my copies in the system temp dir.
(setq flymake-run-in-place nil)

(provide 'init-flymake)
