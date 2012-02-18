;; Helper library to find unbound key combinations.
(require 'unbound)

(global-unset-key "\C-l")
(defvar ctl-l-map (make-keymap)
  "Keymap for local bindings and functions, prefixed by (^L)")
(define-key global-map "\C-l" 'Control-L-prefix)
(fset 'Control-L-prefix ctl-l-map)

(define-key ctl-l-map "aa"		'fg/anything-jump)
(define-key ctl-l-map "ab"		'anything-browse-code)
(define-key ctl-l-map "ad"		'anything-c-apropos)
(define-key ctl-l-map "af"		'anything-find-files)
(define-key ctl-l-map "ag"		'fg/anything-rgrep)
(define-key ctl-l-map "ai"		'anything-info-emacs)
(define-key ctl-l-map "al"		'anything-locate)
(define-key ctl-l-map "am"		'fg/anything-man-pages)
(define-key ctl-l-map "at"		'fg/anything-tag)
(define-key ctl-l-map "au"		'anything-ucs)
(define-key ctl-l-map "ay"		'anything-show-kill-ring)
(define-key ctl-l-map "ax"		'anything-M-x)
(define-key ctl-l-map "bs"		'bookmark-set)
(define-key ctl-l-map "bj"		'bookmark-jump)
(define-key ctl-l-map "caa"		'align)
(define-key ctl-l-map "car"		'align-regexp)
(define-key ctl-l-map "csb"		'hs-show-block)
(define-key ctl-l-map "csa"		'hs-show-all)
(define-key ctl-l-map "chb"		'hs-hide-block)
(define-key ctl-l-map "cha"		'hs-hide-all)
(define-key ctl-l-map "cr"		'recompile)
(define-key ctl-l-map "cc"		'compile)
(define-key ctl-l-map "cl"		'pylookup-lookup)
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
(define-key ctl-l-map "n"		'notmuch)
(define-key ctl-l-map "ma"		'auto-complete-mode)
(define-key ctl-l-map "mc"		'company-mode)
(define-key ctl-l-map "mf"		'flymake-mode)
(define-key ctl-l-map "ml"		'lighthouse-mode)
(define-key ctl-l-map "mr"		'auto-revert-mode)
(define-key ctl-l-map "mw"		'whitespace-mode)
(define-key ctl-l-map "of"		'org-footnote-action)
(define-key ctl-l-map "q"		'query-replace)
(define-key ctl-l-map "Q"		'query-replace-regexp)
(define-key ctl-l-map "r"		'revert-buffer)
(define-key ctl-l-map "tn"		'multi-term-next)
(define-key ctl-l-map "tp"		'multi-term-prev)
(define-key ctl-l-map "tt"		'multi-term)
(define-key ctl-l-map "ui"		'ucs-insert)
(define-key ctl-l-map "U"		'browse-url)
(define-key ctl-l-map "v="		'vc-diff)
(define-key ctl-l-map "vd"		'vc-dir)
(define-key ctl-l-map "vD"		'vc-delete-file)
(define-key ctl-l-map "vg"		'vc-annotate)
(define-key ctl-l-map "vl"		'vc-print-log)
(define-key ctl-l-map "vu"		'vc-revert)
(define-key ctl-l-map "vv"		'vc-next-action)
(define-key ctl-l-map "vh"		'monky-status)
(define-key ctl-l-map "vm"		'magit-status)
(define-key ctl-l-map "wc"		'winring-new-configuration)
(define-key ctl-l-map "wd"		'winring-duplicate-configuration)
(define-key ctl-l-map "wk"		'winring-delete-configuration)
(define-key ctl-l-map "wj"		'winring-jump-to-configuration)
(define-key ctl-l-map "wn"		'winring-next-configuration)
(define-key ctl-l-map "wp"		'winring-prev-configuration)
(define-key ctl-l-map "wr"		'winring-rename-configuration)
(define-key ctl-l-map "wb"		'winring-submit-bug-report)
(define-key ctl-l-map "wv"		'winring-version)
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

(global-set-key (kbd "M-F") 'forward-whitespace)
(global-set-key (kbd "M-B") 'fg/backward-whitespace)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

(global-set-key (kbd "C-`") 'other-window)
(global-set-key (kbd "M-`") 'flymake-goto-next-error)
(global-set-key (kbd "M-RET") 'er/expand-region)

(global-set-key (kbd "s-<up>") 'backward-up-sexp)
(global-set-key (kbd "s-<down>") 'down-list)

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


(provide 'init-keybindings)
