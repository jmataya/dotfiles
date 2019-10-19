(use-package web-mode :ensure t)
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(defun web-mode-hook-init ()
  "Hooks for web-mode."
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-indent-style 2))

(add-hook 'web-mode-hook 'web-mode-hook-init)
