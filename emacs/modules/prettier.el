(use-package prettier-js :ensure t)
(use-package add-node-modules-path :ensure t)

(defun web-mode-init-prettier-hook ()
  (add-node-modules-path)
  (prettier-js-mode))

(add-hook 'web-mode-hook 'web-mode-init-prettier-hook)
