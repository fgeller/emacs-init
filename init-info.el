(add-to-list 'Info-directory-list "~/.emacs.d/info")

(if *is-a-mac*
    (add-to-list 'Info-directory-list
                 "/Applications/Emacs.app/Contents/Resources/info"))

(provide 'init-info)
