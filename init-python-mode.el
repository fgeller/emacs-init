(require 'python)

(autoload 'doctest-mode "doctest-mode" "Python doctest editing mode." t)

(setq auto-mode-alist
      (append '(("\\.py$" . python-mode)
                ("SConstruct$" . python-mode)
                ("SConscript$" . python-mode))
              auto-mode-alist))

(setq interpreter-mode-alist
      (cons '("python" . python-mode) interpreter-mode-alist))

(add-hook 'python-mode-hook
          (lambda ()
            (make-variable-buffer-local 'tab-width)
            (make-variable-buffer-local 'indent-tabs-mode)
            (make-variable-buffer-local 'whitespace-style)
            (add-hook 'before-save-hook 'whitespace-cleanup nil 'local)
            (setq
             tab-width 2
             python-indent 2
             indent-tabs-mode t
             python-guess-indent nil
             whitespace-style '(face
                                tabs
                                spaces
                                trailing
                                lines
                                space-before-tab::tab
                                newline
                                indentation::tab
                                empty
                                space-after-tab::tab
                                space-mark
                                tab-mark
                                newline-mark))))

(defun flymake-python-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "pycheckers"  (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-python-init))

(add-hook 'python-mode-hook
       (lambda () (unless (eq buffer-file-name nil) (flymake-mode 1))))

;; Initialize Pymacs
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; Initialize Rope
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)
;; (ac-ropemacs-initialize)
;; http://gunnarwrobel.de/wiki/Python.html
;; (setq pdb-path '/usr/lib/python2.7/pdb.py
;;       gud-pdb-command-name 'pdb)
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2003-10/msg00577.html
;; (defadvice pdb (before gud-query-cmdline activate)
;;   "Provide a better default command line when called interactively."
;;   (interactive
;;    (list (gud-query-cmdline pdb-path
;;              (file-name-nondirectory buffer-file-name)))))

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;          ;; (setq ac-sources (add-to-list 'ac-sources 'ac-source-ropemacs))))
;;          (setq ac-sources (add-to-list 'ac-sources))))
;; (add-hook 'python-mode-hook
;;        (lambda () (unless (eq buffer-file-name nil) (flymake-mode 1))))
;; http://code.google.com/p/autopair/
;; (add-hook 'python-mode-hook
;;        #'(lambda ()
;;            (setq autopair-handle-action-fns
;;                  (list #'autopair-default-handle-action
;;                        #'autopair-python-triple-quote-action))))

;; python
;; (require 'python)
;; Initialize Pymacs
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; Initialize Rope
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)
;; (ac-ropemacs-initialize)
;; http://gunnarwrobel.de/wiki/Python.html
;; (setq pdb-path '/usr/lib/python2.7/pdb.py
;;       gud-pdb-command-name 'pdb)
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2003-10/msg00577.html
;; (defadvice pdb (before gud-query-cmdline activate)
;;   "Provide a better default command line when called interactively."
;;   (interactive
;;    (list (gud-query-cmdline pdb-path
;;				(file-name-nondirectory buffer-file-name)))))
;; (defun flymake-pyflakes-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                   'flymake-create-temp-inplace))
;;       (local-file (file-relative-name
;;                    temp-file
;;                    (file-name-directory buffer-file-name))))
;;	(list "pycheckers"  (list local-file))))
;; (add-to-list 'flymake-allowed-file-name-masks
;;           '("\\.py\\'" flymake-pyflakes-init))
;; Hooks
;; (add-hook 'python-mode-hook
;;        (lambda ()
;;			(setq tab-width 4)
;;			(setq indent-tabs-mode t)))
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;			;; (setq ac-sources (add-to-list 'ac-sources 'ac-source-ropemacs))))
;;			(setq ac-sources (add-to-list 'ac-sources))))
;; (add-hook 'python-mode-hook
;;        (lambda () (unless (eq buffer-file-name nil) (flymake-mode 1))))
;; http://code.google.com/p/autopair/
;; (add-hook 'python-mode-hook
;;        #'(lambda ()
;;            (setq autopair-handle-action-fns
;;					(list #'autopair-default-handle-action
;;                        #'autopair-python-triple-quote-action))))
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             ;; Scan the file for nested code blocks
;;             (imenu-add-menubar-index)
;;             (hs-minor-mode t)))

(provide 'init-python-mode)
