(defcustom preferred-python-mode 'python
  "Major python mode to use."
  :type 'symbol
  :group 'programming
  :options '(python python-mode))

(eval-after-load "python-mode"
  '(setq py-install-directory "~/.emacs.d/addons/python-mode"
         pdb-path "/System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/pdb.py"
         py-smart-indentation nil
         py-shell-name "ipython"
         py-outline-minor-mode-p nil)
  )

(eval-after-load "python"
  '(progn
     (setq python-shell-interpreter "ipython"
           python-shell-interpreter-args ""
           python-shell-prompt-regexp "In \\[[0-9]+\\]: "
           python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
           python-shell-completion-setup-code
           "from IPython.core.completerlib import module_completion"
           python-shell-completion-module-string-code
           "';'.join(module_completion('''%s'''))\n"
           python-shell-completion-string-code
           "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
     (define-key python-mode-map (kbd "RET") 'newline-and-indent))
  )


(require preferred-python-mode)
(require 'virtualenv)
(require 'nose)
(setq gud-pdb-command-name "ipdb")

;; http://www.masteringemacs.org/articles/2012/05/29/compiling-running-scripts-emacs/
(defun python-insert-breakpoint ()
  "Inserts a python breakpoint using `pdb'"
  (interactive)
  (back-to-indentation)
  ;; this preserves the correct indentation in case the line above
  ;; point is a nested block
  (split-line)
  (insert python--pdb-breakpoint-string))

(defvar python--pdb-breakpoint-string "import ipdb; ipdb.set_trace() ## DEBUG ##"
  "Python breakpoint string used by `python-insert-breakpoint'")

(defadvice compile (before ad-compile-smart activate)
  "Advises `compile' so it sets the argument COMINT to t
if breakpoints are present in `python-mode' files"
  (when (derived-mode-p major-mode 'python-mode)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (if (re-search-forward (concat "^\\s-*" python--pdb-breakpoint-string "$")
                               (point-max) t)
            ;; set COMINT argument to `t'.
            (ad-set-arg 1 t)
          (message "could not find breakpoint string"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; pylookup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq pylookup-dir "~/.emacs.d/addons/pylookup")
(eval-when-compile (require 'pylookup))
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))
(setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))

(autoload 'pylookup-lookup "pylookup" t)
(autoload 'pylookup-update "pylookup" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Flymake ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flymake-python-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-intemp))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "pycheckers"  (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-python-init))


;;;;;;;;;;;;;;; Buffer local variables and minor modes ;;;;;;;;;;;;;;;

(defun fg/python-mode-initialization ()
  (setq mode-name
        (if (eq "python-mode" preferred-python-mode)
            "py-mode"
          "python"))
  (wrap-region-mode 1)
  (camelCase-mode 1)
  (pretty-symbols-mode 1)
  ;; flymake
  (unless (eq buffer-file-name nil)
    (flymake-mode 1))
  ;; whitespace cleanup
  (add-hook 'before-save-hook 'whitespace-cleanup nil 'local))

(add-hook 'python-mode-hook 'fg/python-mode-initialization)

(defun fg/run-python-test ()
  (interactive)
  (let* ((file-name buffer-file-name)
         (project-root (fg/guess-project-root))
         (class-name (fg/find-backward "class \\(.+\\)("))
         (fun-name (fg/find-backward "def \\(test.+\\)("))
         (cmd (format
               "cd %s && TESTSEL=%s:%s.%s make tests"
               project-root
               file-name
               class-name
               fun-name)))
    (let ((compilation-buffer-name-function (lambda (x) "*tests*")))
      (compile cmd t))))

(defun fg/re-run-tests ()
  (interactive)
  (with-current-buffer "*tests*"
    (compile compile-command t)))

(provide 'init-python-mode)
