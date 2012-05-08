(setq calendar-week-start-day 1)

(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Various preferences
(setq org-log-done t
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
)


; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps nil)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S)" "PROJECT(P@)" "|" "CANCELLED(c@/!)"))))


;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persistence-insinuate t)
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show iCal calendars in the org agenda
(eval-after-load "org"
  '(require 'org-mac-iCal))
(setq org-agenda-include-diary t)


(add-hook 'org-agenda-cleanup-fancy-diary-hook
          (lambda ()
            (goto-char (point-min))
            (save-excursion
              (while (re-search-forward "^[a-z]" nil t)
                (goto-char (match-beginning 0))
                (insert "0:00-24:00 ")))
            (while (re-search-forward "^ [a-z]" nil t)
              (goto-char (match-beginning 0))
              (save-excursion
                (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
              (insert (match-string 0)))))


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

(eval-after-load 'org
  '(progn
     (require 'org-exp)
     (require 'org-clock)
     (require 'org-checklist)
     (require 'org-export)
     (require 'org-contacts)
     ))

(defun fg/publish-state-entry-state-change-to-gcal ()
  (let ((new-title (org-get-heading))
    (gcal-id (org-entry-get nil "GCalId")))
    (when gcal-id
      (start-process
       "push2gcal" "*push2gcal*"
       "gcal2org.py" "fgeller@gmail.com" "update" gcal-id "title" new-title))))
(add-hook 'org-after-todo-state-change-hook
      'fg/publish-state-entry-state-change-to-gcal)


(provide 'init-org)
