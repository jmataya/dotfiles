(defvar +gzy/global-term-name "*global-term*")

(defun gzy/global-term ()
  "Opens an ansi-term in bottom window that spreads the entire frame.
   Create the window if it doesn't exist, otherwise show it."
  (interactive)

  (let ((location 'below)
        (size 30))
    (if (get-buffer +gzy/global-term-name)
        "TODO: Add opening the global term"
      (gzy/new-global-term location size))))

  ;; (let* ((direction 'below)
  ;;        (size 30)
  ;;        (term-name "*global-term*")
  ;;        (current-buf (get-buffer-window (current-buffer)))
  ;;        (existing-buf (get-buffer term-name))
  ;;        (new-buf (generate-new-buffer-name term-name))
  ;;        (term-win (split-window (frame-root-window)
  ;;                                (and size (prefix-numeric-value size))
  ;;                                direction)))
  ;;   (select-window term-win)
  ;;   (if existing-buf
  ;;       ((set-window-dedicated-p current-buf nil)
  ;;        (display-buffer-same-window existing-buf nil))
  ;;     ((generate-new-buffer new-buf)
  ;;      (set-window-dedicated-p current-buf nil)
  ;;      (ansi-term "/bin/zsh" new-buf)
  ;;      (evil-emacs-state)))))

  (defun gzy/new-global-term (location size)
    "Opens a new global-term instance with the appropriate LOCATION and SIZE."
    (let* ((current-buf (get-buffer-window (current-buffer)))
           (new-buf (generate-new-buffer-name +gzy/global-term-name))
           (term-win (split-window (frame-root-window)
                                   (and size (prefix-numeric-value size))
                                   location)))
      (select-window term-win)
      (generate-new-buffer new-buf)
      (set-window-dedicated-p current-buf nil)
      (ansi-term "/bin/zsh" new-buf)
      (evil-emacs-state)))

  (define-key global-map (kbd "C-`") 'gzy/global-term)
