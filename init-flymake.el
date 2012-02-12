(setq flymake-gui-warnings-enabled nil)
(setq flymake-start-syntax-check-on-newline nil)
(setq flymake-no-changes-timeout 5)

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

(provide 'init-flymake)
