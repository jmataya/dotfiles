;; Vim-style code commenting

(use-package evil-commentary :ensure t)
(evil-commentary-mode)

;; Company mode for autocompletion

(use-package company :ensure t)
(add-hook 'after-init-hook 'global-company-mode)
