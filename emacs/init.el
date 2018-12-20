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

(org-babel-load-file "~/.emacs.d/configuration-v2.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ggtags diminish yaml-mode web-mode vue-mode use-package toc-org terraform-mode spaceline solarized-theme scss-mode org-bullets neotree markdown-mode magit key-chord js2-mode helm-projectile helm-ag gotest git-gutter exec-path-from-shell evil-org evil-leader evil-commentary dracula-theme dotenv-mode dockerfile-mode command-log-mode buffer-move base16-theme all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
