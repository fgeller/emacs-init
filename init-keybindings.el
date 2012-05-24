;; Helper library to find unbound key combinations.
(require 'unbound)
(require 'repeat)

(defun fg/eshell-with-prefix ()
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'eshell))

;; From http://groups.google.com/group/gnu.emacs.help/browse_thread/thread/44728fda08f1ec8f?hl=en&tvc=2
(defun make-repeatable-command (cmd)
  "Returns a new command that is a repeatable version of CMD.
The new command is named CMD-repeat.  CMD should be a quoted
command.

This allows you to bind the command to a compound keystroke and
repeat it with just the final key.  For example:

  (global-set-key (kbd \"C-c a\") (make-repeatable-command 'foo))

will create a new command called foo-repeat.  Typing C-c a will
just invoke foo.  Typing C-c a a a will invoke foo three times,
and so on."
  (fset (intern (concat (symbol-name cmd) "-repeat"))
        `(lambda ,(help-function-arglist cmd) ;; arg list
           ,(format "A repeatable version of `%s'." (symbol-name cmd)) ;; doc string
           ,(interactive-form cmd) ;; interactive form
           ;; see also repeat-message-function
           (setq last-repeatable-command ',cmd)
           (repeat nil)))
  (intern (concat (symbol-name cmd) "-repeat")))

;; Swap "C-t" and "C-x", so it's easier to type on Dvorak layout
(keyboard-translate ?\C-t ?\C-x)
(keyboard-translate ?\C-x ?\C-t)

(require 'key-chord)
(require 'space-chord)
(key-chord-mode 1)
(space-chord-define-global "a" 'ace-jump-mode)

(defvar fg-map (make-keymap)
  "Keymap for local bindings and functions")
(key-chord-define-global "uh" 'FG-prefix)
(fset 'FG-prefix fg-map)

(define-key fg-map "A"		'ack)
(define-key fg-map "aa"		'fg/helm-jump)
(define-key fg-map "ab"		'helm-browse-code)
(define-key fg-map "ac"		'fg/helm-contact)
(define-key fg-map "ad"		'helm-c-apropos)
(define-key fg-map "af"		'helm-find-files)
(define-key fg-map "ag"		'fg/helm-rgrep)
(define-key fg-map "ai"		'fg/helm-help)
(define-key fg-map "al"		'helm-locate)
(define-key fg-map "am"		'fg/helm-man-pages)
(define-key fg-map "ar"		'helm-regexp)
(define-key fg-map "at"		'fg/helm-tag)
(define-key fg-map "au"		'helm-ucs)
(define-key fg-map "ay"		'helm-show-kill-ring)
(define-key fg-map "ax"		'helm-M-x)
(define-key fg-map "br"		'rename-buffer)
(define-key fg-map "bb"		'helm-buffers+)
(define-key fg-map "caa"		'align)
(define-key fg-map "car"		'align-regexp)
(define-key fg-map "csb"		'hs-show-block)
(define-key fg-map "csa"		'hs-show-all)
(define-key fg-map "chb"		'hs-hide-block)
(define-key fg-map "cha"		'hs-hide-all)
(define-key fg-map "cr"		'recompile)
(define-key fg-map "cc"		'compile)
(define-key fg-map "cl"		'pylookup-lookup)
(define-key fg-map "db"		'ediff-buffers)
(define-key fg-map "dd"		'ediff-directories)
(define-key fg-map "df"		'ediff-files)
(define-key fg-map "dr"		'ediff-regions-wordwise)
(define-key fg-map "fn"		'find-name-dired)
(define-key fg-map "G"		'rgrep)
(define-key fg-map "gfn"		'flymake-goto-next-error)
(define-key fg-map "gfp"		'flymake-goto-prev-error)
(define-key fg-map "if"              'fg/connect-to-freenode)
(define-key fg-map "ii"              'fg/connect-to-iptego)
(define-key fg-map "il"              'fg/connect-to-bitlbee)
(define-key fg-map "k"		'kill-whole-line)
(define-key fg-map "ld"		'duplicate-line)
(define-key fg-map "ll"		'goto-line)
(define-key fg-map "n"		'notmuch)
(define-key fg-map "ma"		'auto-complete-mode)
(define-key fg-map "mc"		'company-mode)
(define-key fg-map "mf"		'flymake-mode)
(define-key fg-map "ml"		'lighthouse-mode)
(define-key fg-map "mr"		'auto-revert-mode)
(define-key fg-map "mw"		'whitespace-mode)
(define-key fg-map "of"		'org-footnote-action)
(define-key fg-map "q"		'query-replace)
(define-key fg-map "Q"		'query-replace-regexp)
(define-key fg-map "r"		'revert-buffer)
(define-key fg-map "ss"		'eshell)
(define-key fg-map "sn"		'fg/eshell-with-prefix)
(define-key fg-map "tn"		'multi-term-next)
(define-key fg-map "tp"		'multi-term-prev)
(define-key fg-map "tt"		'multi-term)
(define-key fg-map "ui"		'ucs-insert)
(define-key fg-map "U"		'browse-url-default-macosx-browser)
(define-key fg-map "v="		'vc-diff)
(define-key fg-map "vd"		'vc-dir)
(define-key fg-map "vD"		'vc-delete-file)
(define-key fg-map "vF"		'vc-pull)
(define-key fg-map "vg"		'vc-annotate)
(define-key fg-map "vl"		'vc-print-log)
(define-key fg-map "vu"		'vc-revert)
(define-key fg-map "vv"		'vc-next-action)
(define-key fg-map "vh"		'monky-status)
(define-key fg-map "vm"		'magit-status)
(define-key fg-map "wb"		'winring-submit-bug-report)
(define-key fg-map "wc"		'winring-new-configuration)
(define-key fg-map "wd"		'winring-duplicate-configuration)
(define-key fg-map "wj"		'winring-jump-to-configuration)
(define-key fg-map "wk"		'winring-delete-configuration)
(define-key fg-map "wn"		'winring-next-configuration)
(define-key fg-map "wp"		'winring-prev-configuration)
(define-key fg-map "wr"		'winring-rename-configuration)
(define-key fg-map "wt"		'fg/rotate-windows)
(define-key fg-map "wv"		'winring-version)
(define-key fg-map "xx"		'execute-extended-command)
(define-key fg-map "xb"		'eval-buffer)
(define-key fg-map "xe"		'eval-last-sexp)
(define-key fg-map "xr"		'eval-region)
(define-key fg-map "!"		'shell-command)
(define-key fg-map "^"		'join-line)
(define-key fg-map "%"		'goto-match-paren)
(define-key fg-map "<"		'beginning-of-buffer)
(define-key fg-map ">"		'end-of-buffer)


(global-set-key (kbd "M-U") 'upcase-word)
(global-unset-key (kbd "M-u"))
(let ((binding-char-pairs `((,(kbd "M-u a") . "ä")
                            (,(kbd "M-u o") . "ö")
                            (,(kbd "M-u u") . "ü")
                            (,(kbd "M-u A") . "Ä")
                            (,(kbd "M-u O") . "Ö")
                            (,(kbd "M-u U") . "Ü"))))
  (dolist (pair binding-char-pairs)
    (define-key global-map (car pair)
      (lexical-let ((char (cdr pair))) (lambda () (interactive) (insert char))))))

(global-set-key (kbd "M-F") 'forward-whitespace)
(global-set-key (kbd "M-B") 'fg/backward-whitespace)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

(global-set-key (kbd "C-`") 'other-window)

(global-set-key "\C-x2" (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key "\C-x3" (split-window-func-with-other-buffer 'split-window-horizontally))
(global-set-key "\C-x|" 'split-window-horizontally-instead)
(global-set-key "\C-x_" 'split-window-vertically-instead)

(global-set-key (kbd "M-`") 'flymake-goto-next-error)
(global-set-key (kbd "C-c w") (make-repeatable-command 'er/expand-region))
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(global-set-key (kbd "s-<up>") 'backward-up-sexp)
(global-set-key (kbd "s-<down>") 'down-list)
(global-set-key (kbd "s-f") 'forward-sexp)
(global-set-key (kbd "s-b") 'backward-sexp)
(global-set-key (kbd "s-n") 'forward-sentence)
(global-set-key (kbd "s-p") 'backward-sentence)
(global-set-key (kbd "s-u") 'backward-up-list)
(global-set-key (kbd "s-a") 'beginning-of-defun)
(global-set-key (kbd "s-e") 'end-of-defun)
(global-set-key (kbd "s-h") 'mark-defun)
(global-set-key (kbd "s-k") 'kill-sexp)
(global-set-key (kbd "s-t") 'transpose-sexps)

(global-set-key (kbd "C-h t") 'describe-face)

(when *is-a-mac*
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta)
  (setq default-input-method "MacOSX")
  (setq mouse-wheel-scroll-amount '(0.0001)))

(when *is-cocoa-emacs*
  (global-set-key (kbd "S-`") 'ns-next-frame)
  (global-set-key (kbd "S-h") 'ns-do-hide-others))


(add-hook 'outline-minor-mode-hook
          (lambda () (local-set-key (kbd "M-o")
                               outline-mode-prefix-map)))

(global-set-key (kbd "C-M-=") 'increase-default-font-height)
(global-set-key (kbd "C-M--") 'decrease-default-font-height)

(define-key dired-mode-map "o" 'fg/dired-open-mac)

(global-set-key (kbd "M-z") 'fg/zap-to-char)
(global-set-key (kbd "C-z") 'fg/jump-to-next-char)

(global-set-key [remap kill-word] 'fg/kill-word)
(global-set-key [remap backward-kill-word] 'fg/backward-kill-word)

(define-key js2-mode-map (kbd "C-c s") 'fg/install-reload-browser-on-save-hook)

(define-key message-mode-map (kbd "C-c C-b") 'fg/goto-message-body)
(define-key notmuch-search-mode-map (kbd "Q") 'fg/notmuch-archive-all-and-quit)
(define-key notmuch-search-mode-map (kbd "a") 'fg/notmuch-archive)
(define-key notmuch-search-mode-map (kbd "g") 'notmuch-search-refresh-view)
(define-key notmuch-hello-mode-map "g" 'notmuch-hello-update)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cb" 'org-iswitchb)
(define-key global-map "\C-c\C-xf" 'org-footnote-action)
(define-key org-mode-map (kbd "C-c t") 'org-todo)
(define-key org-agenda-mode-map (kbd "C-c t") 'org-agenda-todo)

(eval-after-load 'paredit
  '(progn
     ;; These are handy everywhere, not just in lisp modes
     (global-set-key (kbd "M-(") 'paredit-wrap-round)
     (global-set-key (kbd "M-[") 'paredit-wrap-square)
     (global-set-key (kbd "M-{") 'paredit-wrap-curly)

     (global-set-key (kbd "M-)") 'paredit-close-round-and-newline)
     (global-set-key (kbd "M-]") 'paredit-close-square-and-newline)
     (global-set-key (kbd "M-}") 'paredit-close-curly-and-newline)

     (dolist (binding (list (kbd "C-<left>") (kbd "C-<right>")
                            (kbd "C-M-<left>") (kbd "C-M-<right>")))
       (define-key paredit-mode-map binding nil))

     ;; Disable kill-sentence, which is easily confused with the kill-sexp
     ;; binding, but doesn't preserve sexp structure
     (define-key paredit-mode-map [remap kill-sentence] nil)
     (define-key paredit-mode-map [remap backward-kill-sentence] nil)))


(define-key emacs-lisp-mode-map (kbd "C-x C-a") 'pp-macroexpand-last-sexp)


(provide 'init-keybindings)
