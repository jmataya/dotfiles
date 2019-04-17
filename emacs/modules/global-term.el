(defun gzy/global-term ()
  "Opens an ansi-term in bottom window that spreads the entire frame.
   Create the window if it doesn't exist, otherwise show it."
  (interactive)

  (let* ((direction 'below)
         (size 30)
         (current-buf (get-buffer-window (current-buffer)))
         (new-buf (generate-new-buffer-name "*global-term*"))
         (term-win (split-window (frame-root-window)
                                 (and size (prefix-numeric-value size))
                                 direction)))
    (select-window term-win)
    (generate-new-buffer new-buf)
    (set-window-dedicated-p current-buf nil)
    (ansi-term "/bin/zsh" new-buf)
    (evil-emacs-state)))

(define-key global-map (kbd "C-`") 'gzy/global-term)
