(autoload 'turn-on-pretty-mode "pretty-mode")

;; ----------------------------------------------------------------------------
;; Paredit
;; ----------------------------------------------------------------------------
(autoload 'enable-paredit-mode "paredit")


(defun maybe-map-paredit-newline ()
  (unless (or (eq major-mode 'inferior-emacs-lisp-mode) (minibufferp))
    (local-set-key (kbd "RET") 'paredit-newline)))

(add-hook 'paredit-mode-hook 'maybe-map-paredit-newline)

(eval-after-load "paredit"
  '(progn
     ;; These are handy everywhere, not just in lisp modes
     (global-set-key (kbd "M-(") 'paredit-wrap-round)
     (global-set-key (kbd "M-[") 'paredit-wrap-square)
     (global-set-key (kbd "M-{") 'paredit-wrap-curly)

     (global-set-key (kbd "M-)") 'paredit-close-round-and-newline)
     (global-set-key (kbd "M-]") 'paredit-close-square-and-newline)
     (global-set-key (kbd "M-}") 'paredit-close-curly-and-newline)

     (dolist (binding (list (kbd "C-<left>") (kbd "C-<right>")
                            (kbd "C-M-<left>") (kbd "C-M-<right>")))
       (define-key paredit-mode-map binding nil))

     ;; Disable kill-sentence, which is easily confused with the kill-sexp
     ;; binding, but doesn't preserve sexp structure
     (define-key paredit-mode-map [remap kill-sentence] nil)
     (define-key paredit-mode-map [remap backward-kill-sentence] nil)))


;; Compatibility with other modes

(defadvice enable-paredit-mode (before disable-autopair activate)
  (setq autopair-dont-activate t)
  (autopair-mode -1))

(suspend-mode-during-cua-rect-selection 'paredit-mode)


;; Use paredit in the minibuffer
(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

(defvar paredit-minibuffer-commands '(eval-expression
                                      pp-eval-expression
                                      eval-expression-with-eldoc)
  "Interactive commands for which paredit should be enabled in the minibuffer.")

(defun conditionally-enable-paredit-mode ()
  "Enable paredit during lisp-related minibuffer commands."
  (if (memq this-command paredit-minibuffer-commands)
      (enable-paredit-mode)))


;; ----------------------------------------------------------------------------
;; Highlight current sexp
;; ----------------------------------------------------------------------------


;; Prevent flickery behaviour due to hl-sexp-mode unhighlighting before each command
(eval-after-load "hl-sexp"
  '(defadvice hl-sexp-mode (after unflicker (turn-on) activate)
     (when turn-on
       (remove-hook 'pre-command-hook #'hl-sexp-unhighlight))))



;; ----------------------------------------------------------------------------
;; Enable desired features for all lisp modes
;; ----------------------------------------------------------------------------

(defun smp-lisp-setup ()
  "Enable features useful in any lisp mode."
  (make-variable-buffer-local 'whitespace-style)
  (setq whitespace-style '(face tabs spaces trailing lines space-before-tab::space newline indentation::space empty space-after-tab::space space-mark tab-mark newline-mark))
  (add-hook 'before-save-hook 'whitespace-cleanup nil 'local)
  (enable-paredit-mode)
  (hl-sexp-mode t)
  (turn-on-eldoc-mode)
  (turn-on-pretty-mode))


(defun smp-emacs-lisp-setup ()
  "Enable features useful when working with elisp."
  (require 'elisp-slime-nav)
  ;; (elisp-slime-nav-mode t)
  ;; (set-up-hippie-expand-for-elisp)
  (ac-emacs-lisp-mode-setup)
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
