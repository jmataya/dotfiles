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
