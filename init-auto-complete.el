(defvar ac-dictionary-directories '())
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")

(require 'auto-complete)
(require 'auto-complete-config)

(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
(global-auto-complete-mode t)

(setq ac-dwim t) ; To get pop-ups with docs even if a word is uniquely completed
(setq ac-quick-help-delay 0.1)
(setq ac-auto-show-menu t)
(setq ac-stop-flymake-on-completing t)

(set-default 'ac-sources
             '(ac-source-yasnippet
               ac-source-filename
               ac-source-files-in-current-dir
               ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers))


;;;;;;;;;;;;;;;;;;;;;;; Additional ac sources ;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ETAGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface ac-etags-candidate-face
  '((t (:background "gainsboro" :foreground "deep sky blue")))
  "Face for etags candidate")

(defface ac-etags-selection-face
  '((t (:background "deep sky blue" :foreground "white")))
  "Face for the etags selected candidate.")

(ac-define-source etags
  '((candidates . (lambda ()
                    (all-completions ac-target (tags-completion-table))))
    (candidate-face . ac-etags-candidate-face)
    (selection-face . ac-etags-selection-face)
    (requires . 3)))

(defun ac-etags-setup ()
  (setq ac-sources (append '(ac-source-etags) ac-sources)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ropemacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ac-nropemacs-candidates ()
  (mapcar (lambda (completion)
            (concat ac-prefix completion))
          (ignore-errors
            (rope-completions))))

(defun ac-nropemacs-get-doc (candidate)
  (catch 'loop
    (mapcar (lambda (tpl)
              (if (string= (car tpl) (substring candidate (length ac-prefix)))
                  (throw 'loop (cadr tpl))))
            (rope-extended-completions))))

(ac-define-source nropemacs
  '((candidates . ac-nropemacs-candidates)
    (document . ac-nropemacs-get-doc)
    (symbol     . "π")))

(ac-define-source nropemacs-dot
  '((candidates . ac-nropemacs-candidates)
    (document . ac-nropemacs-get-doc)
    (symbol     . "π")
    (prefix     . c-dot)
    (requires   . 0)))

(defun ac-nropemacs-setup ()
  (setq ac-delay 1)
  (setq ac-sources (append '(ac-source-nropemacs
                             ac-source-nropemacs-dot) ac-sources)))


(provide 'init-auto-complete)
