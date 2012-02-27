(eval-after-load 'dired
  '(progn
     (require 'dired+)
     (setq dired-recursive-deletes 'top)
     (define-key dired-mode-map [mouse-2] 'dired-find-file)
     (setq dired-details-hidden-string "… ")
     (require 'dired-details)
     (require 'dired-details+)
     ))


(provide 'init-dired)
