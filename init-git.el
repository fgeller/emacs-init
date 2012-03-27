(autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git." t)


(autoload 'magit-status "magit")
(setq magit-save-some-buffers nil
      magit-process-popup-time 10
      magit-completing-read-function 'magit-ido-completing-read)

(defun magit-status-somedir ()
  (interactive)
  (let ((current-prefix-arg t))
    (magit-status default-directory)))

(autoload 'rebase-mode "rebase-mode")
(add-to-list 'auto-mode-alist '("git-rebase-todo" . rebase-mode))

(eval-after-load 'gist
  ;; Fix from https://github.com/defunkt/gist.el/pull/16
  '(defun gist-region (begin end &optional private &optional callback)
     "Post the current region as a new paste at gist.github.com
Copies the URL into the kill ring.

With a prefix argument, makes a private paste."
     (interactive "r\nP")
     (let* ((file (or (buffer-file-name) (buffer-name)))
            (name (file-name-nondirectory file))
            (ext (or (cdr (assoc major-mode gist-supported-modes-alist))
                     (file-name-extension file)
                     "txt")))
       (gist-request
        (format "https://%s@gist.github.com/gists"
                (or (car (github-auth-info)) ""))
        (or callback 'gist-created-callback)
        `(,@(if private '(("action_button" . "private")))
          ("file_ext[gistfile1]" . ,(concat "." ext))
          ("file_name[gistfile1]" . ,name)
          ("file_contents[gistfile1]" . ,(buffer-substring begin end)))))))

(defun fg/magit-log-edit-initialization ()
  (auto-fill-mode 1))

(add-hook 'magit-log-edit-mode-hook 'fg/magit-log-edit-initialization)

(provide 'init-git)
