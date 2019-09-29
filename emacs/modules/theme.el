;; Set a nice base theme.
;; I like light themes - sue me.

(when window-system
  (use-package base16-theme :ensure t)
  (setq base16-distinct-fringe-background nil)
  (setq base16-highlight-mode-line 'contrast)
  (load-theme 'base16-one-light t)

  (defun gzy/theme-fringe ()
    "Make sure the fringe is styled properly."
    (set-face-attribute 'fringe nil
                        :foreground (face-foreground 'default)
                        :background (face-background 'default)))

  (gzy/theme-fringe)

  (defun gzy/light-theme ()
    "Loads a light theme for the editor."
    (interactive)
    (load-theme 'base16-one-light t)
    (gzy/theme-fringe))

  (defun gzy/dark-theme ()
    "Loads a dark theme for the editor."
    (interactive)
    (load-theme 'base16-onedark t)
    (gzy/theme-fringe))

  (global-set-key (kbd "C-c t l") 'gzy/light-theme)
  (global-set-key (kbd "C-c t d") 'gzy/dark-theme))

;; Set the font face.
;; Only do this when we're not in a terminal.

(when window-system
  (require 'gzy-fonts)

  (setq gzy-font-faces "Operator Mono SSm,Fira Code")
  (setq gzy-line-spacing 0.4)

  (if (memq window-system '(mac ns))
      (setq gzy-font-size 14)
    (setq gzy-font-size 12))

  (gzy-font-face)
  (add-hook 'after-make-frame-functions 'gzy-font-face t))
