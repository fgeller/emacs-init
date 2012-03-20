(require 'dired)

(require 'dired+)
(setq dired-recursive-deletes 'top)
(define-key dired-mode-map [mouse-2] 'dired-find-file)
(setq dired-details-hidden-string "")
(require 'dired-details)
(require 'dired-details+)

;; github.com/defunkt/emacs
; remap 'o' in dired mode to open a file
(defun fg/dired-open-mac ()
  (interactive)
  (let ((file-name (dired-get-file-for-visit)))
    (if (file-exists-p file-name)
        (call-process "/usr/bin/open" nil 0 nil file-name))))

(provide 'init-dired)
