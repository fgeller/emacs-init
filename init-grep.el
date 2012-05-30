(require 'grep)
(require 'wgrep)

(add-to-list 'grep-find-ignored-directories "env")
(setq-default find-program "gfind")
(grep-apply-setting 'grep-find-command "gfind . -type f -exec grep -nH -e  {} +")
(grep-apply-setting 'grep-find-template "gfind . <X> -type f <F> -exec grep <C> -nH -e <R> {} +")

(defun fg/rgrep ()
  (let ((default-directory (fg/guess-project-root ())))
    (call-interactively 'rgrep)))


(provide 'init-grep)
