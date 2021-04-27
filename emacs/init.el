(package-initialize)

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(add-to-list 'load-path "~/.emacs.d/gzy/colors/")
(add-to-list 'load-path "~/.emacs.d/gzy/fonts/")
(add-to-list 'load-path "~/.emacs.d/gzy/modeline/")

(load-file "~/.emacs.d/modules/baseline.el")
(load-file "~/.emacs.d/modules/coding.el")
(load-file "~/.emacs.d/modules/company.el")
(load-file "~/.emacs.d/modules/evil.el")
(load-file "~/.emacs.d/modules/flycheck.el")
(load-file "~/.emacs.d/modules/global-term.el")
(load-file "~/.emacs.d/modules/linux.el")
(load-file "~/.emacs.d/modules/macos.el")
(load-file "~/.emacs.d/modules/magit.el")
(load-file "~/.emacs.d/modules/navigation.el")
(load-file "~/.emacs.d/modules/prettier.el")
(load-file "~/.emacs.d/modules/terminal.el")
(load-file "~/.emacs.d/modules/theme.el")
(load-file "~/.emacs.d/modules/writing.el")
;; (load-file "~/.emacs.d/modules/modeline.el")

(load-file "~/.emacs.d/modules/languages/clojure.el")
(load-file "~/.emacs.d/modules/languages/go.el")
(load-file "~/.emacs.d/modules/languages/javascript.el")
(load-file "~/.emacs.d/modules/languages/lisp.el")
(load-file "~/.emacs.d/modules/languages/markdown.el")
(load-file "~/.emacs.d/modules/languages/org-mode.el")
(load-file "~/.emacs.d/modules/languages/php.el")
(load-file "~/.emacs.d/modules/languages/typescript.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("81c3de64d684e23455236abde277cda4b66509ef2c28f66e059aa925b8b12534" "b8929cff63ffc759e436b0f0575d15a8ad7658932f4b2c99415f3dde09b32e97" "fede08d0f23fc0612a8354e0cf800c9ecae47ec8f32c5f29da841fe090dfc450" "99c86852decaeb0c6f51ce8bd46e4906a4f28ab4c5b201bdc3fdf85b24f88518" "350dc341799fbbb81e59d1e6fff2b2c8772d7000e352a5c070aa4317127eee94" "196df8815910c1a3422b5f7c1f45a72edfa851f6a1d672b7b727d9551bb7c7ba" "c614d2423075491e6b7f38a4b7ea1c68f31764b9b815e35c9741e9490119efc0" "6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" default))
 '(package-selected-packages
   '(clojure-mode treeview ox-gfm writeroom-mode markdown-mode+ ztree treemacs prettier-js vala-mode yaml-mode web-mode vue-mode use-package toc-org spaceline solarized-theme scss-mode rjsx-mode rainbow-delimiters paredit org-bullets neotree markdown-mode magit key-chord helm-projectile helm-ag gotest ggtags exec-path-from-shell evil-leader evil-commentary dracula-theme dockerfile-mode diminish counsel-projectile clojure-mode-extra-font-locking cider base16-theme all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
