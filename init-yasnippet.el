(require 'yasnippet)
(yas/global-mode 1)
(yas/load-directory "~/.emacs.d/addons/yasnippet/snippets")
(setq yas/prompt-functions '(yas/dropdown-prompt))

(provide 'init-yasnippet)
