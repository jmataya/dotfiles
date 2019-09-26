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
(defun gzy/format-font (face size)
  "Format a string describing a font face and size."
  (format "%s-%d" face size))

(defvar +gzy/font-faces (list "Operator Mono SSm"
                              "Fira Code"))

(defvar +gzy/font-size 14)

(defun gzy/font-candidate (fonts font-size)
  "Searches through a list of fonts and returns the first one installed on the system."
  (let* ((font-name (format "%s-%d" (car fonts) font-size))
         (font-face (find-font (font-spec :name font-name)))
         (rest-fonts (cdr fonts)))
    (if font-face
        font-name
      (if rest-fonts (gzy/font-candidate rest-fonts font-size)))))

(defun gzy/font-face ()
  "Return the correct font specs."
  (when window-system
    (set-frame-font (gzy/font-candidate +gzy/font-faces +gzy/font-size))
    (setq-default line-spacing 0.40)))

(when window-system (gzy/font-face))
(add-hook 'after-make-frame-functions 'gzy/font-face t)
