(defcustom preferred-javascript-mode 'js2-mode
  "Javascript mode to use for .js files"
  :type 'symbol
  :group 'programming
  :options '(js3-mode js2-mode js-mode))
(defvar preferred-mmm-javascript-mode 'js-mode)
(defvar preferred-javascript-indent-level 2)

(add-to-list 'auto-mode-alist
             `("\\.js$" . ,preferred-javascript-mode))


(autoload 'js2-mode "js2-mode" nil t)
(require 'js2-mode)

;; standard javascript-mode
(setq javascript-indent-level preferred-javascript-indent-level)

(autoload 'inferior-moz-mode "moz" "MozRepl Inferior Mode" t)
(autoload 'moz-minor-mode "moz" "MozRepl Minor Mode" t)

;; Mode specific configurations follow.

(add-hook 'js3-mode-hook
          (lambda ()
            (setq js3-auto-indent-p t)         ; it's nice for commas to right themselves.
            (setq js3-enter-indents-newline t) ; don't need to push tab before typing
            (setq js3-indent-on-enter-key t)   ; fix indenting before moving on
            ))

(add-hook 'js2-mode-hook
          (lambda ()
            (make-variable-buffer-local 'tab-width)
            (make-variable-buffer-local 'indent-tabs-mode)
            (make-variable-buffer-local 'whitespace-style)
            (wrap-region-mode 1)
            (hs-minor-mode 1)
            (rainbow-mode 1)
            (moz-minor-mode 1)
            (setq mode-name "js2")
            (add-hook 'before-save-hook 'whitespace-cleanup nil 'local)
            (setq js2-use-font-lock-faces t)
            (setq js2-mode-must-byte-compile nil)
            (setq js2-basic-offset preferred-javascript-indent-level)
            (setq js2-indent-on-enter-key t)
            (setq js2-auto-indent-p t)
            (setq js2-enter-indents-newline t)
            (setq js2-bounce-indent-p nil)
            (setq js2-auto-insert-catch-block t)
            (setq js2-cleanup-whitespace t)
            (setq js2-global-externs '(Ext console))
            (setq js2-highlight-level 2)
            (setq js2-mirror-mode nil) ; conflicts with autopair
            (setq js2-mode-escape-quotes t) ; t disables
            (setq js2-mode-squeeze-spaces t)
            (setq js2-pretty-multiline-decl-indentation-p t)
            (setq js2-consistent-level-indent-inner-bracket-p t)
            (setq
             tab-width 2
             js2-basic-offset 2
             indent-tabs-mode t
             whitespace-style '(face tabs spaces trailing lines space-before-tab::tab newline indentation::tab empty space-after-tab::tab space-mark tab-mark newline-mark)
             )
            ))

(defun fg/send-browser-reload ()
  (interactive)
  (comint-send-string (inferior-moz-process) "BrowserReload()"))

(add-hook 'js-mode-hook
          (lambda ()
            (make-variable-buffer-local 'tab-width)
            (make-variable-buffer-local 'indent-tabs-mode)
            (make-variable-buffer-local 'whitespace-style)
            (add-hook 'before-save-hook 'whitespace-cleanup nil 'local)
            (setq js-indent-level preferred-javascript-indent-level)
            (setq
             tab-width 2
             indent-tabs-mode t
             whitespace-style '(face tabs spaces trailing lines space-before-tab::tab newline indentation::tab empty space-after-tab::tab space-mark tab-mark newline-mark))))

;; On-the-fly syntax checking
(eval-after-load 'js
  '(add-hook 'js-mode-hook 'flymake-jslint-load))

(setq inferior-js-program-command "/usr/local/bin/node")
(defun add-inferior-js-keys ()
  (local-set-key "\C-x\C-e" 'js-send-last-sexp)
  (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
  (local-set-key "\C-cb" 'js-send-buffer)
  (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
  (local-set-key "\C-cl" 'js-load-file-and-go))
(add-hook 'js2-mode-hook 'add-inferior-js-keys)
(add-hook 'js-mode-hook 'add-inferior-js-keys)


(require 'js-comint)
(setq inferior-js-program-command "/usr/local/bin/node")

(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list 'comint-preoutput-filter-functions
                     (lambda (output)
                       (replace-regexp-in-string
                        ".*1G\.\.\..*5G" "..."
                        (replace-regexp-in-string ".*1G.*3G" "> " output))))))


(provide 'init-javascript)
