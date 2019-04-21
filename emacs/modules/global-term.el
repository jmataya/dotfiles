(defvar +gzy/global-term-base "global-term")
(defvar +gzy/global-term-name (format "*%s*" +gzy/global-term-base))

(defun gzy/global-term ()
  "Opens an ansi-term in bottom window that spreads the entire frame.
   Create the window if it doesn't exist, otherwise show it."
  (interactive)

  (let* ((location 'below)
         (size 30)
         (existing-buf (get-buffer +gzy/global-term-name))
         (existing-win (get-buffer-window existing-buf)))
    (if existing-win
        (delete-window existing-win)
      (if existing-buf
          (gzy/open-global-term location size)
        (gzy/new-global-term location size)))))

(defun gzy/open-global-term (location size)
  "Opens an existing instance of the global-term with the appropriate LOCATION and SIZE."
  (let ((existing-buf (get-buffer +gzy/global-term-name))
        (term-win (split-window (frame-root-window)
                                (and size (prefix-numeric-value size))
                                location)))
    (select-window term-win)
    (display-buffer-same-window existing-buf nil)))

(defun gzy/new-global-term (location size)
  "Opens a new global-term instance with the appropriate LOCATION and SIZE."
  (let* ((current-buf (get-buffer-window (current-buffer)))
         (new-buf (generate-new-buffer-name +gzy/global-term-base))
         (term-win (split-window (frame-root-window)
                                 (and size (prefix-numeric-value size))
                                 location)))
    (select-window term-win)
    (set-window-dedicated-p current-buf nil)
    (ansi-term "/bin/zsh" new-buf)
    (evil-emacs-state)))

(define-key global-map (kbd "C-`") 'gzy/global-term)
