(load "~/.emacs.d/site-lisp/haskell-mode/haskell-site-file.el")

(setq haskell-font-lock-symbols t)

(add-hook 'haskell-mode-hook
          (lambda ()
            (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
            (turn-on-haskell-doc-mode)
            (turn-on-haskell-indent)))


(provide 'init-haskell)
