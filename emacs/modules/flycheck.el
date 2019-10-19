(use-package flycheck :ensure t)
(require 'flycheck)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))

(flycheck-add-mode 'javascript-eslint 'web-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)
