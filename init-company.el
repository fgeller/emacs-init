(autoload 'company-mode "company" nil t)

(setq company-idle-delay 0.1)
(setq company-backends nil)

(add-hook 'python-mode-hook (lambda ()
                              (setq company-backends '((
                                                       company-keywords
                                                       company-etags
                                                       company-files
                                                       )))
                              (company-mode 1)))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq company-backends '((
                                                            company-elisp
                                                            company-files
                                                            )))
                                  (company-mode 1)))



(provide 'init-company)
