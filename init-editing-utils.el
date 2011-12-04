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
 show-trailing-whitespace t
 tooltip-delay 1.5
 size-indication-mode t
 truncate-lines t
 truncate-partial-width-windows nil
 visible-bell nil
 column-number-mode 1
 transient-mark-mode t
 goto-address-mail-face 'link
 scroll-conservatively 123)

(add-hook 'find-file-hooks 'goto-address-prog-mode)
;; (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
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

(add-hook 'before-save-hook 'whitespace-cleanup)

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
;; Zap *up* to char is a more sensible default
;;----------------------------------------------------------------------------
(autoload 'zap-up-to-char "misc" "Kill up to ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

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
;; Autopair quotes and parentheses
;;----------------------------------------------------------------------------
(require 'autopair)
(require 'auto-pair+)
;; (autopair-global-mode)
;; (setq autopair-autowrap t)
(electric-pair-mode)
(electric-indent-mode)

;;----------------------------------------------------------------------------
;; Fix per-window memory of buffer point positions
;;----------------------------------------------------------------------------
(require 'pointback)
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


;;----------------------------------------------------------------------------
;; Handy key bindings
;;----------------------------------------------------------------------------
;; To be able to M-x without meta
;; (global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Vimmy alternatives to M-^ and C-u M-^
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c J") (lambda () (interactive) (join-line 1)))

(global-set-key (kbd "M-T") 'transpose-lines)
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)
(global-set-key (kbd "C-;") 'iy-go-to-char)
(global-set-key (kbd "C-\,") 'iy-go-to-char-backward)

(defun duplicate-line ()
  (interactive)
  (save-excursion
    (let ((line-text (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))
      (move-end-of-line 1)
      (newline)
      (insert line-text))))

(global-set-key (kbd "C-c p") 'duplicate-line)

;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])


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

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up


;;----------------------------------------------------------------------------
;; Cut/copy the current line if no region is active
;;----------------------------------------------------------------------------
(whole-line-or-region-mode t)
(diminish 'whole-line-or-region-mode)
(make-variable-buffer-local 'whole-line-or-region-mode)

(defun suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (let ((flagvar (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
        (advice-name (intern (format "suspend-%s" mode-name))))
    (eval-after-load "cua-rect"
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

(defvar nc-minor-mode-map (make-keymap)
  "nc-minor-mode keymap.")
(let ((f (lambda (m)
           `(lambda () (interactive)
              (message (concat "No! use " ,m " instead."))))))
  (dolist (l '(("<left>" . "C-b") ("<right>" . "C-f") ("<up>" . "C-p")
               ("<down>" . "C-n")
               ("<C-left>" . "M-b") ("<C-right>" . "M-f") ("<C-up>" . "M-{")
               ("<C-down>" . "M-}")
               ("<M-left>" . "M-b") ("<M-right>" . "M-f") ("<M-up>" . "M-{")
               ("<M-down>" . "M-}")
               ("<delete>" . "C-d") ("<C-delete>" . "M-d")
               ("<M-delete>" . "M-d") ("<next>" . "C-v") ("<C-next>" . "M-x <")
               ("<prior>" . "M-v") ("<C-prior>" . "M-x >")
               ("<home>" . "C-a") ("<C-home>" . "M->")
               ("<C-home>" . "M-<") ("<end>" . "C-e") ("<C-end>" . "M->")))
    (define-key nc-minor-mode-map
      (read-kbd-macro (car l)) (funcall f (cdr l)))))
(define-minor-mode nc-minor-mode
  "A minor mode that disables the arrow-keys, pg-up/down, delete
  and backspace."  t " nc"
  'nc-minor-mode-map :global t)
(nc-minor-mode 0)


;; this uses `forward-whitespace' from `thingatpt.el'
(defun fg/backward-whitespace ()
  (interactive)
  (forward-whitespace -1))

;;----------------------------------------------------------------------------
;; Finally my bindings :)
;;----------------------------------------------------------------------------
(global-unset-key "\C-l")
(defvar ctl-l-map (make-keymap)
     "Keymap for local bindings and functions, prefixed by (^L)")
(define-key global-map "\C-l" 'Control-L-prefix)
(fset 'Control-L-prefix ctl-l-map)

(define-key ctl-l-map "aa"		'fg/anything-jump)
(define-key ctl-l-map "ab"		'anything-browse-code)
(define-key ctl-l-map "af"		'anything-find-files)
(define-key ctl-l-map "ag"		'fg/anything-rgrep)
(define-key ctl-l-map "ai"		'fg/anything-info-pages)
(define-key ctl-l-map "al"		'anything-locate)
(define-key ctl-l-map "am"		'fg/anything-man-pages)
(define-key ctl-l-map "at"		'anything-top)
(define-key ctl-l-map "ax"		'anything-M-x)
(define-key ctl-l-map "caa"		'align)
(define-key ctl-l-map "car"		'align-regexp)
(define-key ctl-l-map "csb"		'hs-show-block)
(define-key ctl-l-map "csa"		'hs-show-all)
(define-key ctl-l-map "chb"		'hs-hide-block)
(define-key ctl-l-map "cha"		'hs-hide-all)
(define-key ctl-l-map "cr"		'recompile)
(define-key ctl-l-map "cc"		'compile)
(define-key ctl-l-map "d"		'duplicate-line)
(define-key ctl-l-map "eb"		'ediff-buffers)
(define-key ctl-l-map "el"		'ediff-regions-linewise)
(define-key ctl-l-map "ew"		'ediff-regions-wordwise)
(define-key ctl-l-map "ef"		'ediff-files)
(define-key ctl-l-map "fn"		'find-name-dired)
(define-key ctl-l-map "G"		'rgrep)
(define-key ctl-l-map "gfn"		'flymake-goto-next-error)
(define-key ctl-l-map "gfp"		'flymake-goto-prev-error)
(define-key ctl-l-map "ha"		'apropos-command)
(define-key ctl-l-map "hb"		'describe-bindings)
(define-key ctl-l-map "hc"		'describe-key-briefly)
(define-key ctl-l-map "hd"		'apropos-documentation)
(define-key ctl-l-map "hf"		'describe-function)
(define-key ctl-l-map "hh"		'help-for-help)
(define-key ctl-l-map "hi"		'info)
(define-key ctl-l-map "hk"		'describe-key)
(define-key ctl-l-map "hl"		'view-lossage)
(define-key ctl-l-map "hm"		'describe-mode)
(define-key ctl-l-map "ho"		'icicle-describe-option-of-type)
(define-key ctl-l-map "hp"		'finder-by-keyword)
(define-key ctl-l-map "hq"		'help-quit)
(define-key ctl-l-map "hr"		'info-emacs-manual)
(define-key ctl-l-map "hs"		'describe-syntax)
(define-key ctl-l-map "hv"		'describe-variable)
(define-key ctl-l-map "hw"		'where-is)
(define-key ctl-l-map "h\S-tab" 'icicle-complete-keys)
(define-key ctl-l-map "k"		'kill-whole-line)
(define-key ctl-l-map "l"		'goto-line)
(define-key ctl-l-map "vg"		'magit-status)
(define-key ctl-l-map "vh"		'monky-status)
(define-key ctl-l-map "n"		'notmuch)
(define-key ctl-l-map "ma"		'auto-complete-mode)
(define-key ctl-l-map "mf"		'flymake-mode)
(define-key ctl-l-map "mr"		'auto-revert-mode)
(define-key ctl-l-map "mw"		'whitespace-mode)
(define-key ctl-l-map "of"		'org-footnote-action)
(define-key ctl-l-map "q"		'query-replace)
(define-key ctl-l-map "Q"		'query-replace-regexp)
(define-key ctl-l-map "r"		'revert-buffer)
(define-key ctl-l-map "ui"		'ucs-insert)
(define-key ctl-l-map "U"		'browse-url)
(define-key ctl-l-map "xx"		'execute-extended-command)
(define-key ctl-l-map "xb"		'eval-buffer)
(define-key ctl-l-map "xe"		'eval-last-sexp)
(define-key ctl-l-map "xr"		'eval-region)
(define-key ctl-l-map "!"		'shell-command)
(define-key ctl-l-map "^"		'join-line)
(define-key ctl-l-map "%"		'goto-match-paren)
(define-key ctl-l-map "<"		'beginning-of-buffer)
(define-key ctl-l-map ">"		'end-of-buffer)
(define-key ctl-l-map "\C-c"	'calendar)
(define-key ctl-l-map "\C-l"	'redraw-display)
(define-key ctl-l-map "\C-n"	'linum-mode)
(define-key ctl-l-map "\C-q"	'fill-paragraph)
(define-key ctl-l-map "\C-r"	'isearch-backward-regexp)
(define-key ctl-l-map "\C-s"	'isearch-forward-regexp)

(global-set-key (kbd "<S-iso-lefttab>") 'indent-relative)
(global-set-key (kbd "<S-tab>") 'indent-relative)
(global-set-key (kbd "<backtab>") 'indent-relative)

(global-set-key (kbd "M-F") 'forward-whitespace)
(global-set-key (kbd "M-B") 'fg/backward-whitespace)

(global-set-key (kbd "C-`") 'flymake-goto-next-error)

(provide 'init-editing-utils)
