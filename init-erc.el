(setq erc-nick "felix^^")
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
(setq erc-auto-query 'frame)

(defun fg/connect-to-bitlbee ()
  (interactive)
  (erc :server "localhost"
       :nick "me"))

;; http://emacs-fu.blogspot.de/2012/03/social-networking-with-bitlbee-and-erc.html
(defun fg/bitlbee-identify ()
  (when (and (string= "localhost" erc-session-server)
             (string= "&bitlbee" (buffer-name)))
    (erc-message "PRIVMSG" (format "%s identify %s"
                                   (erc-default-target)
                                   fg/bitlbee-password))))

(add-hook 'erc-join-hook 'fg/bitlbee-identify)

(defun fg/connect-to-iptego ()
  (interactive)
  (erc :server "irc.iptego"
       :nick "felix^^"))

(defun fg/connect-to-freenode ()
  (interactive)
  (erc :server "irc.freenode.net"
       :nick "felix^^"
       :password fg/freenode-password))

(defun fg/notify-privmsg (proc parsed)
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
        (target (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    (when (and (erc-current-nick-p target)
               (not (erc-is-message-ctcp-and-not-action-p msg)))
      (todochiku-message (format "ERC message from: %s" nick)
                         msg
                         (todochiku-icon 'irc)
                         nil)))
  nil)

(defun fg/notify-nick-mentioned (match-type nick msg)
  (when (eq match-type 'current-nick)
    (todochiku-message (format "%s mentioned your nick." (car (split-string nick "!")))
                       msg
                       (todochiku-icon 'irc)
                       t)))

(add-hook 'erc-text-matched-hook 'fg/notify-nick-mentioned)
(add-hook 'erc-server-PRIVMSG-functions 'fg/notify-privmsg t)
(add-hook 'erc-server-PRIVMSG-functions 'erc-server-PRIVMSG t)

(provide 'init-erc)
