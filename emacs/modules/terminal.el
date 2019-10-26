(defun nolinum ()
  (linum-mode 0))

(add-hook 'org-mode-hook 'nolinum)
(add-hook 'shell-mode-hook 'nolinum)
(add-hook 'ansi-term-hook 'nolinum)
(add-hook 'term-mode-hook 'nolinum)

(delete 'term-mode evil-insert-state-modes)
(add-to-list 'evil-emacs-state-modes 'term-mode)
