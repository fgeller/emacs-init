(load-file "~/.emacs.d/addons/cedet/cedet-devel-load.el")
(setq semanticdb-default-save-directory "~/.emacs.d/semanticdb")

;; Semantic

;; Add further minor-modes to be enabled by semantic-mode.
;; See doc-string of `semantic-default-submodes' for other things
;; you can use here.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)

(semantic-mode 1)

(defvar fg/project-root-file-patterns
  '(".project\\'" ".xcodeproj\\'" ".sln\\'" "\\`Project.ede\\'"
    "\\`.git\\'" "\\`.bzr\\'" "\\`_darcs\\'" "\\`.hg\\'"))

(defun fg/semanticdb-guess-project-root (dirname)
  (catch 'root
    (let ((dir dirname)
          (prev-dir nil)
          (pattern (mapconcat 'identity
                              '(".project\\'" ".xcodeproj\\'" ".sln\\'" "\\`Project.ede\\'"
                                "\\`.git\\'" "\\`.bzr\\'" "\\`_darcs\\'" "\\`.hg\\'")
                              "\\|")))
      (while (not (equal dir prev-dir))
        (when (directory-files dir nil pattern t)
          (throw 'root dir))
        (setq prev-dir dir
              dir (file-name-directory (directory-file-name dir)))))))

(add-to-list 'semanticdb-project-root-functions 'fg/semanticdb-guess-project-root)

;; based on `projman-semanticdb-analyze' at http://www.emacswiki.org/emacs/projman.el
(defun fg/semanticdb-analyze-project (pattern)
  (interactive "s")
  (let* (
         (dir (expand-file-name (if buffer-file-name
                                    (file-name-directory buffer-file-name)
                                  default-directory)))
         (recentf-exclude (list ".*")) ; don't want scanned files in recentf list
         (find-command (format "find . -type f -name \"%s\"" pattern))
         (files (split-string (shell-command-to-string find-command) "[\r\n]+" t))
         (file))
    (message "Visiting files...")
    (while files
      (setq file (car files)
            files (cdr files))
      (unless (find-buffer-visiting file)
        (message "Visiting %s" file)
        (let ((buf (find-file-noselect file))
              (tags))
          (setq tags (semantic-fetch-tags))
          (kill-buffer buf)))))
  (semanticdb-save-all-db)
  (message "Finished analyzing files."))


(provide 'init-cedet)
