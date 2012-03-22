;; notmuch
(require 'notmuch)
(require 'org-notmuch)
(setq notmuch-fcc-dirs nil
      notmuch-mua-user-agent-function 'notmuch-mua-user-agent-emacs
      notmuch-search-oldest-first nil
      notmuch-show-logo nil
      notmuch-crypto-process-mime t)


(require 'message)
(setq user-full-name "Felix Geller")
(setq user-mail-address "fgeller@gmail.com")
(setq
 message-kill-buffer-on-exit t
 message-send-mail-partially-limit nil
 send-mail-function 'sendmail-send-it
 mail-from-style 'angles
 ;; http://notmuchmail.org/emacstips/#index12h2
 mail-specify-envelope-from t
 message-sendmail-envelope-from 'header
 mail-envelope-from 'header)

(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)
(add-hook 'message-mode-hook 'turn-on-auto-fill)

(defun fg/goto-message-body ()
  (interactive)
  (message-goto-body)
  (if (re-search-forward "sign>" (point-max) t)
      (newline)))

(defun fg/quit-untag-inbox-unread ()
  (interactive)
  (notmuch-search-operate-all "-unread -inbox")
  (notmuch-search-quit))

(defun fg/decrypt-inlined-messages-in-buffer ()
  (save-excursion
    (let ((prefix "-----BEGIN PGP MESSAGE-----")
          (suffix "-----END PGP MESSAGE-----")
          start end)
      (goto-char (point-max))
      (while (re-search-backward suffix (point-min) t)
        (setq end (point))
        (re-search-backward prefix)
        (setq start (point))
        (epa-decrypt-region start end)))))
(add-hook 'notmuch-show-hook 'fg/decrypt-inlined-messages-in-buffer)

(provide 'init-email)
