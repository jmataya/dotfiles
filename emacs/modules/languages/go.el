(use-package go-mode :ensure t)
(use-package gotest :ensure t)

;; Run goimports to format and import packages on save
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

(defun go-compile-hook ()
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go vet")))

(add-hook 'go-mode-hook 'go-compile-hook)
