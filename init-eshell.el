;; http://www.masteringemacs.org/articles/2010/12/13/complete-guide-mastering-eshell/
(require 'eshell)
(require 'em-term)
(eval-after-load 'eshell
  '(add-to-list 'eshell-visual-commands "htop"))

(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(provide 'init-eshell)
