(require 'tramp)
(add-to-list 'Info-default-directory-list "~/.emacs.d/addons/tramps/info/")
(setq remote-file-name-inhibit-cache nil)

(setq tramp-auto-save-directory "~/.emacs.d/saved/")

(provide 'init-tramp)
