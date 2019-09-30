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

; (org-babel-load-file "~/.emacs.d/configuration-v2.org")
; (load-file "~/.emacs.d/modules/modeline.el")
					; (org-babel-load-file "~/.emacs.d/mac.org")
(load-file "~/.emacs.d/modules/baseline.el")
(load-file "~/.emacs.d/modules/coding.el")
(load-file "~/.emacs.d/modules/evil.el")
(load-file "~/.emacs.d/modules/global-term.el")
(load-file "~/.emacs.d/modules/magit.el")
(load-file "~/.emacs.d/modules/navigation.el")
(load-file "~/.emacs.d/modules/theme.el")
(load-file "~/.emacs.d/modules/modeline.el")

(load-file "~/.emacs.d/modules/languages/lisp.el")
(load-file "~/.emacs.d/modules/languages/org-mode.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#fafafa" "#ca1243" "#50a14f" "#c18401" "#4078f2" "#a626a4" "#4078f2" "#383a42"])
 '(ansi-term-color-vector
   [unspecified "#fafafa" "#ca1243" "#50a14f" "#c18401" "#4078f2" "#a626a4" "#4078f2" "#383a42"])
 '(custom-safe-themes
   (quote
    ("c9f102cf31165896631747fd20a0ca0b9c64ecae019ce5c2786713a5b7d6315e" default)))
 '(package-selected-packages
   (quote
    (base16-theme counsel-projectile counsel ivy yaml-mode web-mode vue-mode vimrc-mode use-package toc-org spaceline scss-mode rainbow-delimiters paredit org-bullets neotree markdown-mode magit key-chord js2-mode helm-projectile helm-ag gotest ggtags exec-path-from-shell evil-leader evil-commentary dracula-theme dockerfile-mode diminish clojure-mode-extra-font-locking cider all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
