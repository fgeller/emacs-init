(require 'anything)
(require 'anything-config)
(require 'anything-match-plugin)

(setq anything-input-idle-delay 0.01)

(defun fg/anything-jump ()
  (interactive)
  (anything-other-buffer
   '(anything-c-source-buffers
     anything-c-source-recentf
     anything-c-source-bookmarks
     fg/anything-c-source-file-search)
   " *fg/anything-jump*"))

(defun fg/anything-describe ()
  (interactive)
  (anything-other-buffer
   '(anything-c-source-de
     anything-c-source-recentf
     fg/anything-c-source-file-search)
   " *fg/anything-jump*"))

(defun fg/anything-man-pages ()
  (interactive)
  (anything-other-buffer
   '(anything-c-source-man-pages)
   " *fg/anything-man-pages*"))

(defun fg/anything-info-pages ()
  (interactive)
  (anything-other-buffer
   '(anything-c-source-info-pages)
   " *fg/anything-info-pages*"))

;; http://www.emacswiki.org/emacs/AnythingSources#toc14
(defvar fg/anything-c-source-file-search
  '((name . "File Search")
    (init . (lambda () (setq anything-default-directory default-directory)))
    (candidates . (lambda ()
                    (let ((args
                           (format "%s -iname '*%s*' -print"
                                   anything-default-directory
                                   anything-pattern)))
                      (start-process-shell-command "file-search-process" nil
                                                   "find" args))))
    (type . file)
    (requires-pattern . 4)
    (delayed))
  "Source for searching matching files recursively.")

(defun fg/anything-rgrep ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'anything-do-grep)))


(setq anything-ff-auto-update-initial-value nil)
(ac-mode -1)


(provide 'init-anything)
