(require 'message)
(setq user-full-name "Felix Geller")
(setq user-mail-address "fgeller@gmail.com")
(setq
 message-kill-buffer-on-exit t
 message-send-mail-partially-limit nil
 send-mail-function 'sendmail-send-it
 mail-from-style 'angles)

(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)
(add-hook 'message-mode-hook 'turn-on-auto-fill)

(define-key message-mode-map "\C-c\C-b"
  (lambda ()
    (interactive)
    (message-goto-body)
    (if (re-search-forward "mode=sign>" (point-at-eol) t)
        (newline))))

(provide 'init-email)
