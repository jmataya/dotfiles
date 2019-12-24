(use-package magit :ensure t)
(global-set-key (kbd "C-c C-g") 'magit-status)

(use-package evil :ensure t)
(use-package evil-leader :ensure t :after evil)
(evil-leader/set-key "g" 'magit-status)
