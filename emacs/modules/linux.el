;; Linux-specific configuration

(defun is-linux-or-term ()
  "Determines if the current frame is running on Linux or a terminal."
  (or (memq window-system '(x))
      (not (display-graphic-p))))

(defun setup-linux-or-term (&optional frame)
  "Runs initial configuration that we want in Linux or the terminal."
  (with-selected-frame (or frame (selected-frame))
    (when (is-linux-or-term)
      (menu-bar-mode 0)
      (use-package exec-path-from-shell :ensure t)
      (exec-path-from-shell-initialize)
      (require 'gzy-colors)
      (setq gzy-default-theme 'dark)
      (gzy-colors))))

(add-hook 'after-init-hook 'setup-linux-or-term)
(add-hook 'after-make-frame-functions 'setup-linux-or-term)
