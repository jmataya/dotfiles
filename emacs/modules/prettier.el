(use-package prettier-js :ensure t)
(use-package add-node-modules-path :ensure t)

(defun init-prettier-mode ()
  (add-node-modules-path)
  (prettier-js-mode))

(defun enable-minor-mode (my-pair)
  "Enable minor mode if the filename matches the regexp.
MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))

(defun tsx-prettier-mode ()
  (enable-minor-mode
   '("\\.tsx\\'" . init-prettier-mode)))

(defun js-prettier-mode ()
  (enable-minor-mode
   '("\\.jsx?\\'" . init-prettier-mode)))

;(add-hook 'typescript-mode-hook 'init-prettier-mode)
(add-hook 'web-mode-hook 'tsx-prettier-mode)
