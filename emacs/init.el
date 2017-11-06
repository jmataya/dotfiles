(package-initialize)

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

; TODO: Load better than this.
; (load "~/.emacs.d/modules/evil-mode-line/config.el")
(org-babel-load-file "~/.emacs.d/modules/monkey-mode-line.org")

(setq package-enable-at-startup nil)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(org-babel-load-file "~/.emacs.d/configuration.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "25c242b3c808f38b0389879b9cba325fb1fa81a0a5e61ac7cae8da9a32e2811b" "ef04dd1e33f7cbd5aa3187981b18652b8d5ac9e680997b45dc5d00443e6a46e3" default)))
 '(package-selected-packages
   (quote
    (ensime evil-leader yaml-mode web-mode use-package solarized-theme smart-mode-line-powerline-theme org-bullets neotree monokai-theme markdown-preview-mode magit load-theme-buffer-local key-chord helm-projectile go-mode exec-path-from-shell evil-visual-mark-mode evil-commentary color-theme-buffer-local base16-theme all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:slant italic))))
 '(org-document-title ((t (:family "Operator")))))
