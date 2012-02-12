(setq erc-nick "felix^^")
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
(setq erc-auto-query 'frame)

(defun fg/notify-privmsg (proc parsed)
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
        (target (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    (when (and (erc-current-nick-p target)
               (not (erc-is-message-ctcp-and-not-action-p msg)))
      (message "ERC message")))
  nil)

(add-hook 'erc-server-PRIVMSG-functions 'fg/notify-privmsg t)


(provide 'init-erc)
