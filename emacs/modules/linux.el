;; Linux-specific configuration

(defun is-linux-or-term ()
  "Determines if the current frame is running on Linux or a terminal."
  (or (memq window-system '(x))
      (not (display-graphic-p))))

(defun setup-linux-or-term ()
  "Runs initial configuration that we want in Linux or the terminal."
  (when (is-linux-or-term)
    (require 'gzy-colors)
    (menu-bar-mode 0)
    (setq gzy-default-theme 'dark)
    (gzy-colors)))

(add-hook 'after-init-hook 'setup-linux-or-term)
(add-hook 'after-make-frame-functions 'setup-linux-or-term)
