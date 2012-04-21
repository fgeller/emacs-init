(setq vc-annotate-background "#fcf4dc")
(setq vc-annotate-color-map
      '((20 . "#a57705")
        (40 . "#bd3612")
        (50 . "#c60007")
        (60 . "#c61b6e")
        (80 . "#5859b7")
        (100 ."#2075c7")
        (120 ."#259185")
        (140 ."#728a05")))

(setq vc-annotate-very-old-color "#042028")

;; http://www.gnu.org/software/tramp/#Frequently-Asked-Questions
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp
              ))

(provide 'init-vc)
