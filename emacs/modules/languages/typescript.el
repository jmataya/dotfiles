;; Use tide for typescript support

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save-mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
;;  (company-mode +1)
  (add-node-modules-path)
  (prettier-js-mode)
  (evil-add-command-properties #'tide-jump-to-definition :jump t)
  (key-chord-define evil-normal-state-map "gd" 'tide-jump-to-definition))


(add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))

(defun tsx-prettier-mode ()
  (enable-minor-mode
   '("\\.tsx\\'" . init-prettier-mode)))

;; (add-hook 'typescript-mode-hook #'setup-tide-mode)
;(add-hook 'typescript-mode-hook 'init-prettier-mode)
(add-hook 'web-mode-hook 'tsx-prettier-mode)
(add-hook 'typescript-mode-hook 'setup-tide-mode)
(add-hook 'typescript-mode-hook 'init-prettier-mode)
