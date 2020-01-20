(use-package writeroom-mode :ensure t)

(setq-default writing-mode-enabled nil)

(setq-default writing-font-size 18)
(setq-default default-font-size 12)

(defun writing-mode--writing-font ()
  (setq gzy-font-size writing-font-size)
  (gzy-font-face))

(defun writing-mode--default-font ()
  (setq gzy-font-size default-font-size)
  (gzy-font-face))

(defun writing-mode--enable ()
  (interactive)
  (setq writeroom-restore-window-config t)
  (visual-line-mode)
  (toggle-word-wrap)
  (writing-mode--writing-font)
  (writeroom-mode)
  (setq writing-mode-enabled t))

(defun writing-mode--disable ()
  (interactive)
  (visual-line-mode)
  (toggle-word-wrap)
  (writing-mode--default-font)
  (writeroom--disable)
  (setq writing-mode-enabled nil))

(defun toggle-writing-mode ()
  "Toggles between writing mode and normal mode."
  (interactive)
  (if writing-mode-enabled (writing-mode--disable) (writing-mode--enable)))

(define-key global-map (kbd "C-c C-w") 'toggle-writing-mode)

