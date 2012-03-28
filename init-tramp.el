(require 'tramp)
(add-to-list 'Info-default-directory-list "~/.emacs.d/addons/tramps/info/")
(setq remote-file-name-inhibit-cache nil)

;; http://www.gnu.org/software/tramp/#Frequently-Asked-Questions
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(provide 'init-tramp)
