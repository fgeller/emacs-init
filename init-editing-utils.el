;; -*- coding:utf-8 -*-

;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default
 blink-cursor-delay 0
 blink-cursor-interval 0.4
 bookmark-default-file "~/.emacs.d/.bookmarks.el"
 buffers-menu-max-size 30
 case-fold-search t
 compilation-scroll-output t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 grep-highlight-matches t
 grep-scroll-output t
 indent-tabs-mode nil
 line-spacing 0.2
 make-backup-files nil
 mouse-yank-at-point t
 set-mark-command-repeat-pop t
 show-trailing-whitespace nil
 tooltip-delay 1.5
 size-indication-mode t
 truncate-lines t
 truncate-partial-width-windows nil
 visible-bell nil
 column-number-mode t
 transient-mark-mode t
 goto-address-mail-face 'link
 revert-without-query '(".*")
 align-text-modes (quote (text-mode outline-mode org-mode))
 blink-matching-paren t
 electric-pair-mode nil
 electric-indent-mode nil
 browse-url-browser-function 'browse-url-default-macosx-browser
 enable-recursive-minibuffers t
 )

(add-hook 'find-file-hooks 'goto-address-prog-mode)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(setq whitespace-style
      '(face
        tabs
        spaces
        trailing
        lines
        space-before-tab
        newline
        indentation
        empty              ; remove all empty lines at beginning/end of buffer
        space-after-tab
        space-mark
        tab-mark
        newline-mark))

(defalias 'yes-or-no-p 'y-or-n-p)

;;----------------------------------------------------------------------------
;; auto-save game
;;----------------------------------------------------------------------------
(set-variable 'auto-save-default t)
(defvar autosave-dir
  (concat "~/.emacs.d/saved"))
(make-directory autosave-dir t)
(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))
(defun make-auto-save-file-name ()
  (concat autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name (concat "#%" (buffer-name) "#")))))
(defvar backup-dir (concat "~/.emacs.d/saved"))
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq delete-old-versions nil)

;;----------------------------------------------------------------------------
;; Don't disable narrowing commands
;;----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;----------------------------------------------------------------------------
;; Show matching parens
;;----------------------------------------------------------------------------
(show-paren-mode t)
(paren-activate)     ; activating mic-paren

;;----------------------------------------------------------------------------
;; Fix per-window memory of buffer point positions
;;----------------------------------------------------------------------------
(global-pointback-mode)

;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;----------------------------------------------------------------------------
;; Rectangle selections, and overwrite text when the selection is active
;;----------------------------------------------------------------------------
(cua-selection-mode t)                  ; for rectangles, CUA is nice

(defun duplicate-line ()
  (interactive)
  (save-excursion
    (let ((line-text (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))
      (move-end-of-line 1)
      (newline)
      (insert line-text))))

;;----------------------------------------------------------------------------
;; Shift lines up and down with M-up and M-down
;;----------------------------------------------------------------------------
(move-text-default-bindings)

;;----------------------------------------------------------------------------
;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
;;----------------------------------------------------------------------------
(defun backward-up-sexp (arg)
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

;;----------------------------------------------------------------------------
;; Cut/copy the current line if no region is active
;;----------------------------------------------------------------------------
(whole-line-or-region-mode t)
(make-variable-buffer-local 'whole-line-or-region-mode)

(defun suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (let ((flagvar (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
        (advice-name (intern (format "suspend-%s" mode-name))))
    (eval-after-load 'cua-rect
      `(progn
         (defvar ,flagvar nil)
         (make-variable-buffer-local ',flagvar)
         (defadvice cua--activate-rectangle (after ,advice-name activate)
           (setq ,flagvar (and (boundp ',mode-name) ,mode-name))
           (when ,flagvar
             (,mode-name 0)))
         (defadvice cua--deactivate-rectangle (after ,advice-name activate)
           (when ,flagvar
             (,mode-name 1)))))))

(suspend-mode-during-cua-rect-selection 'whole-line-or-region-mode)

;;----------------------------------------------------------------------------
;; Unfill regions or paragraphs (see http://xahlee.org/emacs/emacs_unfill-paragraph.html)
;;----------------------------------------------------------------------------
(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the reverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil)))

(defun unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the reverse of `fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

;;----------------------------------------------------------------------------
;; Random line sorting
;;----------------------------------------------------------------------------
(defun sort-lines-random (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert
the character typed."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t                    (self-insert-command (or arg 1))) ))


;; this uses `forward-whitespace' from `thingatpt.el'
(defun fg/backward-whitespace ()
  (interactive)
  (forward-whitespace -1))


;;  http://dorophone.blogspot.com/2011/11/how-to-make-emacs-scratch-buffer.html
(defvar persistent-scratch-filename
  "~/.emacs.d/emacs-persistent-scratch"
  "Location of *scratch* file contents for persistent-scratch.")
(defvar persistent-scratch-backup-directory
  "~/.emacs.d/emacs-persistent-scratch-backups/"
  "Location of backups of the *scratch* buffer contents for
    persistent-scratch.")

(defun make-persistent-scratch-backup-name ()
  "Create a filename to backup the current scratch file by
  concatenating PERSISTENT-SCRATCH-BACKUP-DIRECTORY with the
  current date and time."
  (concat
   persistent-scratch-backup-directory
   (replace-regexp-in-string
    (regexp-quote " ") "-" (format-time-string "%d%m%y_%H%M%S"))))

(defun save-persistent-scratch ()
  "Write the contents of *scratch* to the file name
  PERSISTENT-SCRATCH-FILENAME, making a backup copy in
  PERSISTENT-SCRATCH-BACKUP-DIRECTORY."
  (with-current-buffer (get-buffer "*scratch*")
    (if (file-exists-p persistent-scratch-filename)
        (copy-file persistent-scratch-filename
                   (make-persistent-scratch-backup-name)))
    (write-region (point-min) (point-max)
                  persistent-scratch-filename)))

(defun load-persistent-scratch ()
  "Load the contents of PERSISTENT-SCRATCH-FILENAME into the
  scratch buffer, clearing its contents first."
  (if (file-exists-p persistent-scratch-filename)
      (with-current-buffer (get-buffer "*scratch*")
        (delete-region (point-min) (point-max))
        (insert-file-contents persistent-scratch-filename))))

;; Load persisted scratch buffer upon start-up
(load-persistent-scratch)
;; Hook into saving for persisting
(push #'save-persistent-scratch kill-emacs-hook)

(require 'inline-string-rectangle)
(require 'mark-more-like-this)

(require 'expand-region)
(eval-after-load "text-mode"    '(require 'text-mode-expansions))
(require 'undo-tree)

(require 'wrap-region)
(wrap-region-add-wrapper "`" "'")
(wrap-region-add-wrapper "(" ")")
(wrap-region-add-wrapper "[" "]")
(wrap-region-add-wrapper "{" "}")

(require 'highlight-indentation)

(require 'compile+)

(require 'rebox2)
(require 'line-comment-banner)

(require 'enclose)

(autoload 'camelCase-mode "camelCase-mode" nil t)

(require 'ace-jump-mode)

(require 'saaxy)
(setq saaxy-prompt (propertize "> " 'font-lock-face `(:foreground "#2075c7")))
(defun color-for (str)
  "#259185")

(require 'jumper)


;; http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; more variables:
;; mode-line-mule-info
;; mode-line-client
;; mode-line-modified
;; mode-line-remote
;; mode-line-frame-identification
;; mode-line-buffer-identification
;; mode-line-position
;; mode-line-modes

(setq-default
 mode-line-format
 (list
  " "
  mode-line-remote
  '(:eval (propertize "%b" 'face 'mode-line-buffer-id
                      'help-echo (buffer-file-name)))
  '(:eval (when buffer-read-only
            (propertize "%"
                        'face 'mode-line
                        'help-echo "Buffer is read-only")))
  '(:eval (when (buffer-modified-p)
            (propertize "*"
                        'face 'mode-line-highlight
                        'help-echo "Buffer has been modified")))
  " "
  (propertize "%l" 'face 'mode-line)
  ":"
  (propertize "%c" 'face 'mode-line)
  " "
  (propertize "%p" 'face 'mode-line) ;; where are we
  "/"
  (propertize "%I" 'face 'mode-line) ;; buffer size
  " "
  '(:propertize
    ("" mode-name)
    help-echo "Major mode\nmouse-1: Display major mode menu\nmouse-2: Show help for major mode\nmouse-3: Toggle minor modes"
    mouse-face
    mode-line-highlight
    local-map
    (keymap
     (mode-line keymap
                (mouse-2 . describe-mode)
                (down-mouse-1 menu-item "Menu Bar" ignore :filter
                              (lambda
                                (_)
                                (mouse-menu-major-mode-map))))))
  '(:eval
    (cond
     ((string= ":exit [2]" mode-line-process) (propertize " x" 'face 'error))
     ((string= ":exit [0]" mode-line-process) " ✓")
     (t mode-line-process)))
  '(vc-mode vc-mode)
  '(:eval (when (and flymake-mode-line-e-w
                     (not (string= "" flymake-mode-line-e-w))
                     (not (string= "0/0" flymake-mode-line-e-w)))
            (progn
              (string-match
               "\\([0-9]+\\)/\\([0-9]+\\)"
               flymake-mode-line-e-w
               )
              (let ((error-str (match-string 1 flymake-mode-line-e-w))
                    (warn-str (match-string 2 flymake-mode-line-e-w)))
                (concat
                 " τ:"
                 (propertize error-str 'face 'flymake-errline)
                 ","
                 (propertize warn-str 'face 'flymake-warnline)
                 )))))
  ))

;; https://github.com/defunkt/emacs/blob/master/defunkt/defuns.el
(defun fg/zap-to-char (arg char)
  "Kill up to but excluding ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
This emulates Vim's `dt` behavior, which rocks."
  (interactive "p\ncZap to char: ")
  (if (char-table-p translation-table-for-input)
      (setq char (or (aref translation-table-for-input char) char)))
  (kill-region (point)
               (progn
                 (search-forward (char-to-string char) nil nil arg)
                 (- (point) 1)))
  (backward-char 1))

;; http://www.reddit.com/r/emacs/comments/nfj0e/emacs_wizards_how_do_you_move_about_in_source_code/
(defun fg/jump-to-next-char (c &optional count)
  "Jump forward or backward to a specific character.  With a
count, move that many copies of the character."
  (interactive "cchar: \np")
  (when (string= (string c) (buffer-substring (point) (+ 1 (point))))
    (setq count (+ 1 count)))
  (and
   (search-forward (string c) nil t count)
   (> count 0)
   (backward-char)))

;; fix kill-word
(defun fg/kill-word (arg)
  "Special version of kill-word which swallows spaces separate from words"
  (interactive "p")

  (let ((whitespace-regexp "\\s-+"))
    (kill-region (point)
                 (cond
                  ((looking-at whitespace-regexp) (re-search-forward whitespace-regexp) (point))
                  ((looking-at "\n") (kill-line) (defunkt-kill-word arg))
                  (t (forward-word arg) (point))))))

(defun fg/backward-kill-word (arg)
  "Special version of backward-kill-word which swallows spaces separate from words"
  (interactive "p")
  (if (looking-back "\\s-+")
      (kill-region (point) (progn (re-search-backward "\\S-") (forward-char 1) (point)))
    (backward-kill-word arg)))


(require 'thingatpt)
(defun fg/change-num-at-point (fn)
  (let* ((num (string-to-number (thing-at-point 'word)))
         (bounds (bounds-of-thing-at-point 'word)))
    (save-excursion
      (goto-char (car bounds))
      (fg/kill-word 1)
      (insert (number-to-string (funcall fn num 1))))))

(defun fg/inc-num-at-point ()
  (interactive)
  (fg/change-num-at-point '+))

(defun fg/dec-num-at-point ()
  (interactive)
  (fg/change-num-at-point '-))


(provide 'init-editing-utils)
