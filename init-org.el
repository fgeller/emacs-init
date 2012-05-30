(require 'org-install)

(setq
 org-log-done t
 org-completion-use-ido t
 org-edit-timestamp-down-means-later t
 org-agenda-start-on-weekday nil
 org-agenda-ndays 14
 org-agenda-include-diary t
 org-agenda-window-setup 'current-window
 org-agenda-files `(,org-directory)
 org-contacts-files (list (format "%s/contacts.org" org-directory))
 org-fast-tag-selection-single-key 'expert
 org-tags-column 80
 org-default-notes-file (concat org-directory "/Notes.org")
 org-footnote-auto-label (quote plain)
 org-hide-emphasis-markers t
 org-hide-leading-stars t
 org-refile-targets '((org-agenda-files :maxlevel . 5))
 org-src-fontify-natively t
 org-agenda-remove-tags t
 calendar-week-start-day 1
 org-refile-use-outline-path 'file
 org-refile-targets  '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))
 org-outline-path-complete-in-steps nil
 org-todo-keywords '(
                     (sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                     (sequence "WAITING(w@/!)" "SOMEDAY(S)" "PROJECT(P@)" "|" "CANCELLED(c@/!)"))
 org-clock-persistence-insinuate t
 org-clock-persist t
 org-clock-in-resume t
 org-clock-in-switch-to-state "STARTED"
 org-clock-into-drawer t
 org-clock-out-remove-zero-time-clocks t
 appt-activate 1
 appt-message-warning-time 15
 appt-display-mode-line t
 appt-display-format 'window
 org-modules '(org-habit org-notmuch org-checklist org-eshell org-crypt org-info org-irc)
 org-habit-preceding-days 14
 org-habit-show-habits-only-for-today t
 org-habit-completed-glyph ?.
 org-habit-today-glyph ?o
 org-special-ctrl-k t
 org-special-ctrl-a t
 org-irc-link-to-logs t
)

(eval-after-load 'org-mode
  '(progn
     (require 'org-contacts)))

;; hooks
(add-hook 'org-mode-hook 'turn-on-auto-fill)


;; notifications
(setq org-show-notification-handler
      '(lambda (notification)
         (todochiku-message "org-mode notification" notification
                            (todochiku-icon 'emacs))))


;; capture
(setq org-capture-templates
      `(("t" "Task"
         entry (file+headline ,fg/org-task-file "Tasks")
         "* TODO %?\n  SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"\"))\n\n  %a\n  %i\n")
        ("w" "Work task"
         entry (file+headline ,fg/work-org-task-file "Tasks")
         "* TODO %?\n  SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"\"))\n\n  %a\n  %i\n")
        ("c" "Contacts" entry (file "~/Org/contacts.org")
         "* %(org-contacts-template-name)
                  :PROPERTIES:
                  :EMAIL: %(org-contacts-template-email)
                  :END:")))


;; agenda
(setq org-agenda-custom-commands
      `(("d" . "Completed / archived items")
        ("dt" "[t]oday"
         tags "ARCHIVE_TIME>=\"<today>\""
         ((org-agenda-archives-mode t)))
        ("dy" "[y]esterday"
         tags "ARCHIVE_TIME>=\"<-1d>\"&ARCHIVE_TIME<\"<today>\""
         ((org-agenda-archives-mode t)))
        ("dw" "[w]eek"
         tags "ARCHIVE_TIME>=\"<-1w>\""
         ((org-agenda-archives-mode t)))
        ("I" "Import diary from iCal" agenda ""
         ((org-agenda-mode-hook
           (lambda ()
             (org-mac-iCal)))))
        ("w" "Show work related tasks only" agenda ""
         ((org-agenda-files ',fg/work-org-agenda-files)))))


(provide 'init-org)
