(require 'helm-config)
(require 'helm-misc)
(require 'helm-tags)
(require 'helm-semantic)
(require 'helm-ring)

(defun fg/helm-jump ()
  (interactive)
  (helm-other-buffer
   '(
     helm-c-source-buffers-list
;     helm-c-source-global-mark-ring
     helm-c-source-semantic
     helm-c-source-ctags
     helm-c-source-recentf
     fg/helm-c-source-file-search
     helm-c-source-bookmarks
     helm-c-source-buffer-not-found
     )
   "*fg/helm-jump*"))

(defun fg/helm-tag ()
  (interactive)
  (helm-other-buffer
   '(helm-c-source-etags+-select
     helm-c-source-etags+-history
     helm-c-source-bookmarks)
   " *fg/helm-tag*"))

(defun fg/helm-help ()
  (interactive)
  (helm-other-buffer
   '(
     helm-c-source-info-emacs
     helm-c-source-man-pages
     )
   "*fg/helm-help"))

(defun fg/helm-man-pages ()
  (interactive)
  (helm-other-buffer
   '(helm-c-source-man-pages)
   " *fg/helm-man-pages*"))

(defun fg/helm-info-pages ()
  (interactive)
  (helm-other-buffer
   '(helm-c-source-info-pages)
   " *fg/helm-info-pages*"))


(defun fg/helm-contact ()
  (interactive)
  (helm-other-buffer
   '(fg/helm-goobook-contact-search)
   "*fg/helm-contacts*"))

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
(defvar fg/helm-c-source-file-search
  '((name . "Project File Search")
    (init . (lambda () (setq helm-default-directory default-directory
                        project-root-folder (fg/guess-project-root))))
    (candidates . (lambda ()
                    (let ((args
                           (format "%s -iname '*%s*' -print"
                                   project-root-folder
                                   helm-pattern)))
                      (start-process-shell-command "file-search-process" nil
                                                   "find" args))))
    (type . file)
    (requires-pattern . 4)
    (delayed))
  "Source for searching matching files recursively.")


(defun fg/helm-insert-contact (candidate)
  (insert candidate))

(defvar fg/helm-goobook-contact-search
  '((name . "Contact Search")
    (candidates . (lambda ()
                    (start-process-shell-command "contact-search-process" nil
                                                 "goobook-notmuch" helm-pattern)))
    (type . string)
    (action . (("Insert" . fg/helm-insert-contact)))
    (requires-pattern . 2))
  "Source for searching contact via goobook.")

(defun fg/helm-rgrep ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'helm-do-grep)))

(setq helm-ff-auto-update-initial-value nil)
(setq helm-ff-transformer-show-only-basename t)

(helm-mode 1)
(provide 'init-helm)
