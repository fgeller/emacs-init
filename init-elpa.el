(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

;; When switching between Emacs 23 and 24, we always use the bundled package.el in Emacs 24
(let ((package-el-site-lisp-dir (expand-file-name "~/.emacs.d/site-lisp/package")))
  (when (and (file-directory-p package-el-site-lisp-dir)
             (> emacs-major-version 23))
    (message "Removing local package.el from load-path to avoid shadowing bundled version")
    (setq load-path (remove package-el-site-lisp-dir load-path))))

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))


;;------------------------------------------------------------------------------
;; Also use Melpa for some packages built straight from VC
;;------------------------------------------------------------------------------

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Only take certain packages from Melpa
(setq package-filter-function
      (lambda (package version archive)
        (or (not (string-equal archive "melpa"))
            (memq package '(magit rvm slime mmm-mode dired+ csv-mode
                                  pretty-mode darcsum org-fstree textile-mode
                                  ruby-mode js3 git-blame todochiku)))))

(package-initialize)

;(require-package 'ido-ubiquitous)
(when (< emacs-major-version 24)
  (require-package 'color-theme))
(require-package 'jabber)
;; (require-package 'fringe-helper)
;; (require-package 'gnuplot)
;; (require-package 'haskell-mode)
;; (require-package 'flymake-cursor)
(require-package 'json)
(require-package 'js3)
(require-package 'lua-mode)
(require-package 'project-local-variables)
(require-package 'ruby-mode)
(require-package 'inf-ruby)
(require-package 'yari)
(require-package 'rvm)
(require-package 'yaml-mode)
(require-package 'paredit)
(require-package 'eldoc-eval)
(require-package 'slime)
(require-package 'slime-fuzzy)
(require-package 'slime-repl)
(require-package 'gist)
(require-package 'haml-mode)
(require-package 'sass-mode)
(require-package 'elein)
(require-package 'durendal)
(require-package 'markdown-mode)
;; (require-package 'smex)
(require-package 'rainbow-mode)
(require-package 'maxframe)
(when (< emacs-major-version 24)
  (require-package 'org))
(require-package 'htmlize)
(require-package 'clojure-mode)
(require-package 'clojure-test-mode)
(require-package 'diminish)
(require-package 'autopair)
(require-package 'js-comint)
(require-package 'php-mode)
(require-package 'scratch)
(require-package 'mic-paren)
(require-package 'rainbow-delimiters)
(require-package 'marmalade)

;; I maintain this chunk:
;; (require-package 'ac-slime)
;; (require-package 'vc-darcs)
(require-package 'session)
(require-package 'tidy)
(require-package 'whole-line-or-region)
(require-package 'ibuffer-vc)
(require-package 'coffee-mode)
;; (require-package 'elisp-slime-nav)
(require-package 'mwe-log-commands)
(require-package 'move-text)
;; (require-package 'less-css-mode)
(require-package 'hl-sexp)
(require-package 'dsvn)
(require-package 'pointback)
(require-package 'crontab-mode)
(require-package 'regex-tool)
(require-package 'rinari)
(require-package 'ruby-compilation)
(require-package 'iy-go-to-char)

(provide 'init-elpa)
