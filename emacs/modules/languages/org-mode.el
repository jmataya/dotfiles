(use-package org :ensure t)

;; A more detailed sequence of keywords for tasks.
(setq org-todo-keywords '((sequence "TODO"
                                    "IN-PROGRESS"
                                    "BLOCKED"
                                    "|"
                                    "DONE"
                                    "ABANDONED")))

;; Add a timestamp when completing items.
(setq org-log-done 'time)

;; Use pretty bullets
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

;; Allow export of GitHub Flavored Markdown.
(use-package ox-gfm :ensure t)
(eval-after-load "org"
  '(require 'ox-gfm nil t))

(setq org-html-head "<meta http-equiv='X-UA-Compatible' content='IE=edge'><meta content='width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no' name='viewport'><style>html{touch-action:manipulation;-webkit-text-size-adjust:100%}body{font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji;font-size:14px;line-height:1.5;color:#24292e}h1,h2,h3,h4,h5,h6{margin-top:0;margin-bottom:0}h1,h2{font-weight:600}h3,h4{font-weight:600}h1{font-size:2em;margin: .67em 0}h2{font-size:24px}h3{font-size:20px}#content{font-size:16px;line-height:1.5;margin:0 auto;padding:0;width:100%;max-width:780px;word-wrap:break-word}#content h1, #content h2, #content h3, #content h4, #content h5, #content h6{margin-top:24px;margin-bottom:16px;font-weight:600;line-height:1.25}#content h1, #content h2{padding-bottom: .3em;border-bottom:1px solid #eaecef}#content h1{font-size:2em}#content h2{font-size:1.5em}#content h3{font-size:1.25em}</style>")
