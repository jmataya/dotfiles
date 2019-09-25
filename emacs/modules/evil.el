(setq evil-want-C-i-jump nil)

(use-package evil :ensure t)
(use-package evil-leader :ensure t :after evil)
(use-package key-chord :ensure t)

(evil-mode 1)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

(key-chord-mode 1)
(setq
 key-chord-two-key-delay 1.0
 key-chord-one-key-delay 1.0)
(key-chord-define
 evil-insert-state-map "jk" 'evil-normal-state)
