;; Only do this when not in a terminal.

(when window-system

  ;; Set a nice base theme.
  ;; I like light themes - sue me.

  (use-package base16-theme :ensure t)
  (setq base16-distinct-fringe-background nil)

  (require 'gzy-colors)
  (setq gzy-light-theme 'base16-one-light
        gzy-dark-theme 'base16-onedark)

  (gzy-colors)

  ;; Set the font face.

  (require 'gzy-fonts)

  (setq gzy-font-faces "Operator Mono SSm,Fira Code")
  (setq gzy-line-spacing 0.4)

  (if (memq window-system '(mac ns))
      (setq gzy-font-size 14)
    (setq gzy-font-size 12))

  (gzy-font-face)
  (add-hook 'after-make-frame-functions 'gzy-font-face t))
