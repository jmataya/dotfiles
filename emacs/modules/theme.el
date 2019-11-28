
;; Set a nice base theme.
;; I like light themes - sue me.

(defun set-theme (&optional frame)
  "Set a custom color and font settings."
  (with-selected-frame (or frame (selected-frame))
    (set-color-scheme)
    (set-font-face)))

(defun set-color-scheme ()
  "Set a custom color scheme."
  (use-package base16-theme :ensure t)
  (setq base16-distinct-fringe-background nil)

  (require 'gzy-colors)
  (setq gzy-light-theme 'base16-one-light
        gzy-dark-theme 'base16-onedark)

  (gzy-colors))

(defun set-font-face ()
  "Set font face in graphical displays."

  (when window-system

    (require 'gzy-fonts)

    (setq gzy-font-faces "Operator Mono SSm,Fira Code")
    (setq gzy-line-spacing 0.4)

    (if (memq window-system '(mac ns))
        (setq gzy-font-size 14)
      (setq gzy-font-size 12))

    (gzy-font-face)))
;; (add-hook 'after-make-frame-functions 'gzy-font-face t)))

(add-hook 'after-make-frame-functions 'set-theme)
(add-hook 'after-init-hook 'set-theme)

;; (use-package base16-theme :ensure t)
;; (setq base16-distinct-fringe-background nil)

;; (require 'gzy-colors)
;; (setq gzy-light-theme 'base16-one-light
;;       gzy-dark-theme 'base16-onedark)

;; (gzy-colors)

;; ;; Set the font face.
;; ;; Only do this when not in a terminal.

;; (add-hook 'after-make-frame-functions (lambda (aframe)
;;                                         (use-package base16-theme :ensure t)
;;                                         (setq base16-distinct-fringe-background nil)

;;                                         (require 'gzy-colors)
;;                                         (setq gzy-light-theme 'base16-one-light
;;                                               gzy-dark-theme 'base16-onedark)

;;                                         (gzy-colors)

;;                                         (when window-system

;;                                           (require 'gzy-fonts)

;;                                           (setq gzy-font-faces "Operator Mono SSm,Fira Code")
;;                                           (setq gzy-line-spacing 0.4)

;;                                           (if (memq window-system '(mac ns))
;;                                               (setq gzy-font-size 14)
;;                                             (setq gzy-font-size 12))

;;                                           (gzy-font-face)
;;                                           (add-hook 'after-make-frame-functions 'gzy-font-face t))))
