(load "~/.emacs.d/site-lisp/haskell-mode/haskell-site-file.el")

(setq haskell-program-name (executable-find "ghci"))
(setq haskell-font-lock-symbols t)

(add-hook 'haskell-mode-hook
          (lambda ()
            (define-key haskell-mode-map [?\C-c h] 'hoogle-lookup)
            (turn-on-haskell-doc-mode)
            (turn-on-haskell-indent)))


(provide 'init-haskell)
