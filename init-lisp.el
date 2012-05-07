;; ----------------------------------------------------------------------------
;; Highlight current sexp
;; ----------------------------------------------------------------------------


;; Prevent flickery behaviour due to hl-sexp-mode unhighlighting before each command
(eval-after-load 'hl-sexp
  '(defadvice hl-sexp-mode (after unflicker (turn-on) activate)
     (when turn-on
       (remove-hook 'pre-command-hook #'hl-sexp-unhighlight))))



;; ----------------------------------------------------------------------------
;; Enable desired features for all lisp modes
;; ----------------------------------------------------------------------------

(defun smp-lisp-setup ()
  "Enable features useful in any Lisp mode."
  (make-variable-buffer-local 'whitespace-style)
  (setq whitespace-style '(face tabs spaces trailing lines space-before-tab::space newline indentation::space empty space-after-tab::space space-mark tab-mark newline-mark))
  (add-hook 'before-save-hook 'whitespace-cleanup nil 'local)
  (turn-on-eldoc-mode))


(defun smp-emacs-lisp-setup ()
  "Enable features useful when working with elisp."
  (require 'elisp-slime-nav)
  (checkdoc-minor-mode))

(let* ((elispy-hooks '(emacs-lisp-mode-hook ielm-mode-hook))
       (lispy-hooks (append elispy-hooks '(lisp-mode-hook inferior-lisp-mode-hook lisp-interaction-mode-hook))))
  (dolist (hook lispy-hooks)
    (add-hook hook 'smp-lisp-setup))
  (dolist (hook elispy-hooks)
    (add-hook hook 'smp-emacs-lisp-setup)))

(require 'eldoc-eval)

(add-to-list 'auto-mode-alist '("\\.emacs-project$" . emacs-lisp-mode))

(define-key emacs-lisp-mode-map (kbd "C-x C-a") 'pp-macroexpand-last-sexp)

(provide 'init-lisp)
