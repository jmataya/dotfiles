;; MacOS-specific configuration

(defun is-mac ()
  "Determines if the current frame is running on a Mac."
  (memq window-system '(mac ns)))

(defun setup-mac (&optional frame)
  "Runs initial configuration that we want in Linux or the terminal."
  (with-selected-frame (or frame (selected-frame))
    (when (is-mac)
      (setq gzy-default-theme 'dark)
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (use-package exec-path-from-shell :ensure t)
      (exec-path-from-shell-initialize))))

(add-hook 'after-init-hook 'setup-mac)
(add-hook 'after-make-frame-functions 'setup-mac)
