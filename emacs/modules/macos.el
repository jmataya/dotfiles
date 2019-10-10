;; MacOS-specific configuration

(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  ;; (add-to-list 'default-frame-alist '(ns-appearance . light))

  (use-package exec-path-from-shell :ensure t)
  (exec-path-from-shell-initialize))
