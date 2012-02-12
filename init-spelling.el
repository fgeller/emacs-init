(when (executable-find "aspell")
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra")))


(provide 'init-spelling)
