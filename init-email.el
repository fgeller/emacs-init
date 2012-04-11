;; notmuch
(require 'notmuch)
(require 'org-notmuch)
(setq notmuch-fcc-dirs nil
      notmuch-mua-user-agent-function 'notmuch-mua-user-agent-emacs
      notmuch-search-oldest-first nil
      notmuch-show-logo nil
      notmuch-crypto-process-mime t)

(setq notmuch-saved-searches
      '(("inbox" . "tag:inbox")
        ("i/inbox" . "tag:i/inbox")))

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
 mail-envelope-from 'header
 gnus-inhibit-images t
 message-signature "Felix Geller")

(defun fg/insert-message-sign-pgpmime ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (re-search-forward "--text follows this line--" (point-max) t)
    (end-of-line)
    (newline)
   (insert "<#secure method=pgpmime mode=sign>")
   (newline)))

(add-hook 'message-setup-hook 'fg/insert-message-sign-pgpmime)
(add-hook 'message-mode-hook 'turn-on-auto-fill)

(defun fg/goto-message-body ()
  (interactive)
  (message-goto-body)
  (if (re-search-forward "sign>" (point-max) t)
      (newline)))

(defun fg/quit-untag-inbox-unread ()
  (interactive)
  (notmuch-search-operate-all "-unread -inbox -i/inbox")
  (notmuch-search-quit))

(defun fg/notmuch-mark-read ()
  (interactive
   (notmuch-search-operate-all "-unread")
   (notmuch-search-quit)))

(defun fg/notmuch-mark-read-and-drop-inbox-tag ()
  (interactive
   (notmuch-search-operate-all "-unread -inbox -i/inbox")
   (notmuch-search-quit)))

;; use open for PDFs (rather than gv) and images (rather than display)
(setcdr (assoc 'viewer (cdr (assoc "pdf" (assoc "application"  mailcap-mime-data))))
        "open %s")
(setcdr (assoc 'viewer (cdr (assoc ".*" (assoc "image"  mailcap-mime-data))))
        "open %s")
(setcdr (assoc 'test (cdr (assoc ".*" (assoc "image"  mailcap-mime-data))))
        'window-system)

(provide 'init-email)
