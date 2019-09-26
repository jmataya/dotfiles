(setq evil-want-C-i-jump nil)

(use-package evil :ensure t)
(use-package evil-leader :ensure t :after evil)
(use-package key-chord :ensure t)

(evil-mode 1)

(key-chord-mode 1)
(setq
 key-chord-two-key-delay 1.0
 key-chord-one-key-delay 1.0)
(key-chord-define
 evil-insert-state-map "jk" 'evil-normal-state)

(key-chord-define evil-normal-state-map "vv" 'split-window-horizontally)
(key-chord-define evil-normal-state-map "ss" 'split-window-vertically)

(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key "m" 'toggle-frame-maximized)
(evil-leader/set-key "f" 'toggle-frame-fullscreen)
(evil-leader/set-key "v" 'delete-other-windows-vertically)
(evil-leader/set-key "1" 'delete-other-windows)
(evil-leader/set-key "q" 'evil-save-and-close)
