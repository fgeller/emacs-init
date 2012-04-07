(require 'bookmark+)

;; automatically bookmark line and column
(setq bmkp-auto-idle-bookmark-mode-set-function 'bmkp-set-autonamed-bookmark)
;; seconds before automatically bookmark point
(setq bmkp-auto-idle-bookmark-mode-delay 15)
;; min number of chars between automatically set bookmarks.
;; nil causes infinite loop.
(setq bmkp-auto-idle-bookmark-min-distance 200)

;; enable automatic bookmarking globally
(bmkp-global-auto-idle-bookmark-mode 1)

;; styling.
(setq bmkp-light-style-autonamed 'lfringe)
(setq bmkp-light-style-non-autonamed 'lfringe)
(setq bmkp-light-left-fringe-bitmap 'right-triangle)
(setq bmkp-auto-light-when-set 'any-bookmark)
(setq bmkp-auto-light-when-jump 'any-bookmark)


(provide 'init-bookmark)
