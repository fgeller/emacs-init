(setq
 bbdb-complete-mail-allow-cycling t
 bbdb-completion-display-record nil
 bbdb-message-all-addresses nil
 bbdb/message-update-records-p (lambda nil
                                 (let ((bbdb-update-records-p (quote query)))
                                   (bbdb-select-message))))

(provide 'init-bbdb)
