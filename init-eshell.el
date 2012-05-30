;; http://www.masteringemacs.org/articles/2010/12/13/complete-guide-mastering-eshell/
(require 'eshell)
(require 'em-term)
(eval-after-load 'eshell
  '(add-to-list 'eshell-visual-commands "htop"))

(require 'vc-git)
(defun fg/eshell-git-info ()
  (let* ((branch (vc-git-working-revision (eshell/pwd))))
    (if (not (string-equal "" branch))
        (concat branch " ")
      "")))


(defun fg/eshell-replace-prompt-prefixes ()
  (let ((absolute-path (eshell/pwd)))
    (cond ((string-match (getenv "HOME") absolute-path)
           (replace-match "~" nil nil absolute-path))
          ((string-match "/ssh:\\(.+\\):" absolute-path)
           (replace-match (concat "@" (match-string 1 absolute-path) " ")  nil nil absolute-path))
          (t
           absolute-path))))

(defun fg/eshell-prompt-function ()
  (concat
   (fg/eshell-git-info)
   (fg/eshell-replace-prompt-prefixes)
   "/ "))

(defun fg/eshell-with-prefix ()
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'eshell))


(setq eshell-prompt-function #'fg/eshell-prompt-function)
(setq eshell-prompt-regexp "^[^\n]*/ ")


(provide 'init-eshell)
