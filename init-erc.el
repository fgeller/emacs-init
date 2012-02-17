(setq erc-nick "felix^^")
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
(setq erc-auto-query 'frame)

(defun fg/notify-privmsg (proc parsed)
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
        (target (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    (when (and (erc-current-nick-p target)
               (not (erc-is-message-ctcp-and-not-action-p msg)))
      (todochiku-message (format "ERC message from: %s" nick)
                         msg
                         (todochiku-icon 'chat)
                         t)))
  nil)


(add-hook 'erc-server-PRIVMSG-functions 'fg/notify-privmsg t)
(add-hook 'erc-server-PRIVMSG-functions 'erc-server-PRIVMSG t)

(provide 'init-erc)
