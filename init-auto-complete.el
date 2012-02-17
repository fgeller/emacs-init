(defvar ac-dictionary-directories '())
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")

(require 'auto-complete)
(require 'auto-complete-config)

;; Key-bindings
(define-key ac-mode-map (kbd "C-;") 'ac-expand)
(define-key ac-mode-map (kbd "C-:") 'ac-complete)
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)

(setq ac-dwim t) ; To get pop-ups with docs even if a word is uniquely completed
(ac-config-default)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ropemacs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ac-ropemacs-candidates ()
  (mapcar (lambda (completion)
      (concat ac-prefix completion))
    (rope-completions)))

(ac-define-source nropemacs
  '((candidates . ac-ropemacs-candidates)
    (symbol     . "p")))

(ac-define-source nropemacs-dot
  '((candidates . ac-ropemacs-candidates)
    (symbol     . "p")
    (prefix     . c-dot)
    (requires   . 0)))

(defun ac-nropemacs-setup ()
  (setq ac-delay 1)
  (setq ac-sources (append '(ac-source-nropemacs
                             ac-source-nropemacs-dot) ac-sources)))
(defun ac-python-mode-setup ()
  (setq ac-sources '(ac-source-etags
                     ac-source-yasnippet
                     ac-source-filename
                     ac-source-dictionary)))

(provide 'init-auto-complete)
