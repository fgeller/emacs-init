(require 'multi-term)
(setq multi-term-program "/bin/bash")
(add-to-list 'term-unbind-key-list "C-l")

(setq term-default-bg-color "#fcf4dc")
(setq term-default-fg-color "#52676f")

(provide 'init-term)
