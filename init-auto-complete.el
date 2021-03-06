(defvar ac-dictionary-directories '())
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")

(require 'auto-complete)
(require 'auto-complete-config)

(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
(global-auto-complete-mode t)

(setq ac-dwim t) ; To get pop-ups with docs even if a word is uniquely completed
(setq ac-auto-show-menu t)
(setq ac-stop-flymake-on-completing t)
(setq ac-menu-height 0)

(set-default 'ac-sources
             '(ac-source-yasnippet
               ac-source-filename
               ac-source-semantic
               ac-source-semantic-raw
               ac-source-imenu
               ac-source-words-in-buffer
               ))

(provide 'init-auto-complete)
