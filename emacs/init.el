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
;; (load-file "~/.emacs.d/modules/modeline.el")

(load-file "~/.emacs.d/modules/languages/javascript.el")
(load-file "~/.emacs.d/modules/languages/lisp.el")
(load-file "~/.emacs.d/modules/languages/org-mode.el")

(require 'gzy-modeline)
(gzy-modeline)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (treemacs prettier-js vala-mode yaml-mode web-mode vue-mode use-package toc-org spaceline solarized-theme scss-mode rjsx-mode rainbow-delimiters paredit org-bullets neotree markdown-mode magit key-chord helm-projectile helm-ag gotest ggtags exec-path-from-shell evil-leader evil-commentary dracula-theme dockerfile-mode diminish counsel-projectile clojure-mode-extra-font-locking cider base16-theme all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
