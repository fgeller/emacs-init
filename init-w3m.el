(setq w3m-command "/usr/local/bin/w3m")
(require 'w3m-load)

(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

(provide 'init-w3m)
