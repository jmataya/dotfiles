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
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    (quote
;;     (toc-org evil-org scss-mode key-chord evil-leader use-package evil))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("80930c775cef2a97f2305bae6737a1c736079fdcc62a6fdf7b55de669fbbcd13" "7a2ac0611ded83cdd60fc4de55ba57d36600eae261f55a551b380606345ee922" "304c39b190267e9b863c0cf9c989da76dcfbb0649cbcb89592e7c5c08348fce9" "542e6fee85eea8e47243a5647358c344111aa9c04510394720a3108803c8ddd1" "808b47c5c5583b5e439d8532da736b5e6b0552f6e89f8dafaab5631aace601dd" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "446cc97923e30dec43f10573ac085e384975d8a0c55159464ea6ef001f4a16ba" "5a39d2a29906ab273f7900a2ae843e9aa29ed5d205873e1199af4c9ec921aaab" "44961a9303c92926740fc4121829c32abca38ba3a91897a4eab2aa3b7634bed4" "aaffceb9b0f539b6ad6becb8e96a04f2140c8faa1de8039a343a4f1e009174fb" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(package-selected-packages
   (quote
    (base16-theme terraform-mode helm-ag buffer-move solarized-theme dotenv-mode web-mode vue-mode helm-ebdb command-log-mode go-test go-mode use-package toc-org scss-mode key-chord evil-org evil-leader dracula-theme dash))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((((min-colors 16777216)) (:background "#282a36" :foreground "#f8f8f2")) (t (:background "#000000" :foreground "#f8f8f2"))))
;;  '(mode-line ((t (:foreground: "#282a36" :background "#565761" :box nil))))
;;  '(powerline-active1 ((t (:foreground: "#282a36" :background "#ff79c6" :box nil))))
;;  '(powerline-active2 ((t (:foreground: "#282a36" :background "#ff79c6" :box nil)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
