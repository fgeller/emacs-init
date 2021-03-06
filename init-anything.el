(require 'anything)
(require 'anything-config)
(require 'anything-match-plugin)
(require 'anything-etags+)

(setq anything-input-idle-delay 0.01)

(defun fg/anything-jump ()
  (interactive)
  (anything-other-buffer
   '(anything-c-source-buffers-list
     anything-c-source-bookmarks
     anything-c-source-recentf
     fg/anything-c-source-file-search
     anything-c-source-buffer-not-found)
   "*fg/anything-jump*"))

(defun fg/anything-tag ()
  (interactive)
  (anything-other-buffer
   '(anything-c-source-etags+-select
     anything-c-source-etags+-history
     anything-c-source-bookmarks)
   " *fg/anything-tag*"))

(defun fg/anything-help ()
  (interactive)
  (anything-other-buffer
   '(
     anything-c-source-info-emacs
     anything-c-source-man-pages
     )
   "*fg/anything-help"))

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


(defun fg/anything-contact ()
  (interactive)
  (anything-other-buffer
   '(fg/anything-goobook-contact-search)
   "*fg/anything-contacts*"))

;; taken from full-ack.el
(defvar fg/project-root-file-patterns
  '(".project\\'" ".xcodeproj\\'" ".sln\\'" "\\`Project.ede\\'"
    "\\`.git\\'" "\\`.bzr\\'" "\\`_darcs\\'" "\\`.hg\\'"))

(defun fg/guess-project-root ()
  (interactive)
  (catch 'root
    (let ((dir (expand-file-name (if buffer-file-name
                                     (file-name-directory buffer-file-name)
                                   default-directory)))
          (prev-dir nil)
          (pattern (mapconcat 'identity fg/project-root-file-patterns "\\|")))
      (while (not (equal dir prev-dir))
        (when (directory-files dir nil pattern t)
          (throw 'root dir))
        (setq prev-dir dir
              dir (file-name-directory (directory-file-name dir)))))))

;; http://www.emacswiki.org/emacs/AnythingSources#toc14
(defvar fg/anything-c-source-file-search
  '((name . "Project File Search")
    (init . (lambda () (setq anything-default-directory default-directory
                        project-root-folder (fg/guess-project-root))))
    (candidates . (lambda ()
                    (let ((args
                           (format "%s -iname '*%s*' -print"
                                   project-root-folder
                                   anything-pattern)))
                      (start-process-shell-command "file-search-process" nil
                                                   "find" args))))
    (type . file)
    (requires-pattern . 4)
    (delayed))
  "Source for searching matching files recursively.")


(defun fg/anything-insert-contact (candidate)
  (insert candidate))

(defvar fg/anything-goobook-contact-search
  '((name . "Contact Search")
    (candidates . (lambda ()
                    (start-process-shell-command "contact-search-process" nil
                                                 "goobook-notmuch" anything-pattern)))
    (type . string)
    (action . (("Insert" . fg/anything-insert-contact)))
    (requires-pattern . 2))
  "Source for searching contact via goobook.")

(defun fg/anything-rgrep ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'anything-do-grep)))

(setq anything-ff-auto-update-initial-value nil)
(setq anything-ff-transformer-show-only-basename t)

(ac-mode 1)


(provide 'init-anything)
