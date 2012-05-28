;;;;;;;;;;;;;;;;;
;; python-mode ;;
;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;
;; pylookup ;;
;;;;;;;;;;;;;;
(setq pylookup-dir "~/.emacs.d/addons/pylookup")
(eval-when-compile (require 'pylookup))
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))
(setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))

(autoload 'pylookup-lookup "pylookup" t)
(autoload 'pylookup-update "pylookup" t)

;;;;;;;;;;;;;
;; Flymake ;;
;;;;;;;;;;;;;
(defun flymake-python-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-intemp))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "pycheckers"  (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-python-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer local variables and minor modes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fg/python-mode-initialization ()
  (setq mode-name
        (if (eq "python-mode" preferred-python-mode)
            "py-mode"
          "python"))
  (wrap-region-mode 1)
  (camelCase-mode 1)
  (jumper-mode 1)
  ;; flymake
  (unless (eq buffer-file-name nil)
    (flymake-mode 1))
  ;; whitespace cleanup
  (add-hook 'before-save-hook 'whitespace-cleanup nil 'local))

(add-hook 'python-mode-hook 'fg/python-mode-initialization)

(provide 'init-python-mode)
