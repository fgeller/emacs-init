(defcustom preferred-javascript-mode 'js2-mode
  "Javascript mode to use for .js files"
  :type 'symbol
  :group 'programming
  :options '(js2-mode js-mode))
(defvar preferred-mmm-javascript-mode 'js-mode)
(defvar preferred-javascript-indent-level 2)

(autoload 'js2-mode "js2-mode" nil t)
(require 'js2-mode)

(add-hook 'js2-mode-hook
          (lambda ()
            (make-variable-buffer-local 'tab-width)
            (make-variable-buffer-local 'indent-tabs-mode)
            (make-variable-buffer-local 'whitespace-style)
            (add-hook 'before-save-hook 'whitespace-cleanup nil 'local)
            (setq
             tab-width 2
             js2-basic-offset 2
             indent-tabs-mode t
             whitespace-style '(face tabs spaces trailing lines space-before-tab::tab newline indentation::tab empty space-after-tab::tab space-mark tab-mark newline-mark))))
(add-hook 'js-mode-hook
          (lambda ()
            (make-variable-buffer-local 'tab-width)
            (make-variable-buffer-local 'indent-tabs-mode)
            (make-variable-buffer-local 'whitespace-style)
            (add-hook 'before-save-hook 'whitespace-cleanup nil 'local)
            (setq
             tab-width 2
             indent-tabs-mode t
             whitespace-style '(face tabs spaces trailing lines space-before-tab::tab newline indentation::tab empty space-after-tab::tab space-mark tab-mark newline-mark))))

;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
;; (setq auto-mode-alist (cons `("\\.js\\(\\.erb\\)?$" . ,preferred-javascript-mode)
;;                             (loop for entry in auto-mode-alist
;;                                   unless (eq preferred-javascript-mode (cdr entry))
;;                                   collect entry)))

(add-to-list 'auto-mode-alist
             `("\\.js$" . ,preferred-javascript-mode))

;; On-the-fly syntax checking
(eval-after-load 'js
  '(add-hook 'js-mode-hook 'flymake-jslint-load))


;; js2-mode
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

;; js-mode
(setq js-indent-level preferred-javascript-indent-level)

;; standard javascript-mode
(setq javascript-indent-level preferred-javascript-indent-level)


;; MMM submode regions in html
(eval-after-load 'mmm-vars
  `(progn
     (mmm-add-group
      'html-js
      '((js-script-cdata
         :submode ,preferred-mmm-javascript-mode
         :face mmm-code-submode-face
         :front "<script[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
         :back "[ \t]*\\(//\\)?]]>[ \t\n]*</script>"
         :insert ((?j js-tag nil @ "<script language=\"JavaScript\">"
                      @ "\n" _ "\n" @ "</script>" @)))
        (js-script
         :submode ,preferred-mmm-javascript-mode
         :face mmm-code-submode-face
         :front "<script[^>]*>[ \t]*\n?"
         :back "[ \t]*</script>"
         :insert ((?j js-tag nil @ "<script language=\"JavaScript\">"
                      @ "\n" _ "\n" @ "</script>" @)))
        (js-inline
         :submode ,preferred-mmm-javascript-mode
         :face mmm-code-submode-face
         :front "on\w+=\""
         :back "\"")))
     (dolist (mode (list 'html-mode 'nxml-mode))
       (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?$" 'html-js))))

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(eval-after-load 'coffee-mode
  `(setq coffee-js-mode preferred-javascript-mode
         coffee-tab-width preferred-javascript-indent-level))

(add-hook 'coffee-mode-hook 'flymake-coffee-load)


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
