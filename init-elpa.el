(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(require-package 'bookmark+)
(require-package 'json)
(require-package 'js-comint)
(require-package 'paredit)
(require-package 'mic-paren)
(require-package 'eldoc-eval)
(require-package 'slime)
(require-package 'rainbow-mode)
(require-package 'rainbow-delimiters)
(require-package 'session)
(require-package 'tidy)
(require-package 'whole-line-or-region)
(require-package 'move-text)
(require-package 'hl-sexp)
(require-package 'pointback)
(require-package 'regex-tool)
(require-package 'flymake-cursor)
(require-package 'maxframe)

(provide 'init-elpa)
