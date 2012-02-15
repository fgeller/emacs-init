;;;;;;;;;;;;
;; Pymacs ;;
;;;;;;;;;;;;
(require 'pymacs "~/.emacs.d/site-lisp/Pymacs/pymacs.el")

;;;;;;;;;;;;;;;;;
;; python-mode ;;
;;;;;;;;;;;;;;;;;
(setq py-install-directory "~/.emacs.d/addons/python-mode")
(setq pdb-path "/System/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/pdb.py")
(setq py-smart-indentation nil)
(setq py-shell-name "ipython")
(setq py-outline-minor-mode-p nil)
(require 'python-mode)

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
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "pycheckers"  (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-python-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer local variables and minor modes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook
          (lambda ()
            (wrap-region-mode 1)
            ;; pylookup
            (make-local-variable browse-url-browser-function)
            (setq browse-url-browser-function 'w3m)
            ;; update ctags
            (setq ctags-update-delay-seconds (* 5 60))
            (ctags-update-minor-mode 1)
            ;; flymake
            (unless (eq buffer-file-name nil)
              (flymake-mode 1))
            ;; whitespace cleanup
            (add-hook 'before-save-hook 'whitespace-cleanup nil 'local)))

(provide 'init-python-mode)
