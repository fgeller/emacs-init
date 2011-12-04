(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/addons/yasnippet/snippets/text-mode")
(setq yas/prompt-functions '(yas/dropdown-prompt))

(provide 'init-yasnippet)
