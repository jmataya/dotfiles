(require 'all-the-icons)
(require 'cl-lib)
(require 'evil)

;;
;; Determine which window is active.
;; Based off the amazing Doom: https://github.com/hlissner/doom-emacs
;;

(defsubst active ()
  (eq (selected-window) -gzy-current-window))

(defvar -gzy-current-window (frame-selected-window))
(defun gzy-set-selected-window (&rest _)
  "Sets -gzy-current-window with the appropriately selected window."
  (when-let* ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq -gzy-current-window win))))

(add-hook 'window-configuration-change-hook #'gzy-set-selected-window)
(add-hook 'focus-in-hook #'gzy-set-selected-window)
(advice-add #'handle-switch-frame :after #'gzy-set-selected-window)
(advice-add #'select-window :after #'gzy-set-selected-window)

;;
;; Colors
;;

(defvar -gzy-foreground "#f8f8f2")
(defvar -gzy-dark-foreground "#282a36")
(defvar -gzy-inactive-background "#282a36")
(defvar -gzy-height 0.85)
(defvar -gzy-current-accent -gzy-inactive-background)

(set-face-attribute 'mode-line nil
                    :foreground -gzy-foreground
                    :box nil
                    :height -gzy-height)

(set-face-attribute 'mode-line-inactive nil
                    :background -gzy-inactive-background
                    :foreground "#44475a"
                    :box nil
                    :height -gzy-height)

(defun gzy-has-substr (test str)
  "Checks to see if the string TEST is in the STR."
  (string-match-p (regexp-quote test) str))

(defun gzy-trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
  White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun gzy-spacer ()
  "Inserts a spacer between segments."
   (propertize " "
               'face `(:height 1.5 :box nil)
               'display '(raise -0.15)))

(defun gzy-segment/spacer ()
  (propertize " • "))

(defun gzy-segment/project-name ()
  "Segment for the modeline that displays the current project name."
  (let ((name (projectile-project-name)))
    (if (> (length name) 1)
        (concat
         (propertize name)
         (gzy-segment/spacer)))))

(defun gzy-propertize-filename ()
  "Gets the current filename with its corresponding icon."
  (let ((color (if (not (active))
                   "#44475a"
                 (if (buffer-modified-p)
                     "#50fa7b"
                   "#f8f8f2"))))
    (concat
     (if (stringp buffer-file-name)
         (let
             ((buffer-icon (all-the-icons-icon-for-file (buffer-file-name))))
           (if buffer-icon
               (propertize (format "%s" (all-the-icons-icon-for-file (buffer-file-name)))
                           'face `(:foreground ,color :height 1.2 :box nil :family ,(all-the-icons-fileicon-family))
                           'display '(raise -0.17)))))
     (propertize (format " %s" (buffer-name))
                 'face `(:foreground ,color)))))

(defvar -gzy-evil-names '((" <N> " . "NORMAL")
                          (" <I> " . "INSERT")
                          (" <V> " . "VISUAL")
                          (" <R> " . "REPLACE")
                          (" <O> " . "OPERATOR-PENDING")
                          (" <M> " . "MOTION")
                          (" <E> " . "EMACS")))

(defvar -gzy-evil-colors '(("NORMAL" . "#bd93f9")
                           ("INSERT" . "#50fa7b")
                           ("VISUAL" . "#8be9fd")
                           ("REPLACE" . "#ff79c6")
                           ("OPERATOR-PENDING" . "#ff5555")
                           ("MOTION" . "#f1fa8c")))

(defun gzy-propertize-evil-mode ()
  "Formats the Evil mode for the modeline."
  (let* ((name (cdr (assoc evil-mode-line-tag -gzy-evil-names)))
         (color (cdr (assoc name -gzy-evil-colors)))
         (face (if (active)
                   `(:background ,color :foreground ,-gzy-dark-foreground))))
    (if color (setq -gzy-current-accent color))
    (if name (propertize (format " %s " name) 'face face))))

(defun gzy-propertize-git-branch ()
  "Reads the current git branch and formats it for the modeline."
  (if (active)
      (let ((branch (gzy-trim-string
                     (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))))
        (if (not (or (eq branch "")
                     (gzy-has-substr "not a git repository" branch)))
            (concat
             (propertize (format " • %s" (all-the-icons-octicon "git-branch"))
                         'face `(:height 1 :family ,(all-the-icons-faicon-family))
                         'display '(raise 0))
             (propertize (format " %s" branch)))))))

(defun gzy-extract-git-changes ()
  "Extracts the current number of file changes, insertions, and deletions
   from inside a git repository."
  (let ((summary (split-string
                  (shell-command-to-string "git diff --stat | grep changed")
                  ", ")))
    (fset 'new-alist (lambda (key val)
                       (cons (cons key val) ())))
    (fset 'extract-num (lambda (str)
                         (car (split-string str " "))))
    (cl-reduce (lambda (&optional acc &optional elt)
                 (append
                  (if (equal acc nil) '() acc)
                  (cond
                   ((gzy-has-substr "changed" elt)
                    (new-alist "ch" (extract-num elt)))
                   ((gzy-has-substr "insertion" elt)
                    (new-alist "up" (extract-num elt)))
                   ((gzy-has-substr "deletion" elt)
                    (new-alist "down" (extract-num elt))))))
               summary
               :initial-value '())))

(defun gzy-propertize-git-changes ()
  "Reads git changes and formats them for the modeline."
  (if (active)
      (let* ((change-details (gzy-extract-git-changes))
             (ch (assoc "ch" change-details))
             (up (assoc "up" change-details))
             (dn (assoc "down" change-details)))
        (if ch
            (concat
             (propertize (format " •"))
             (if up
                 (concat
                  (propertize (format " %s" (all-the-icons-faicon "arrow-circle-o-up"))
                              'face `(:height 1 :family ,(all-the-icons-faicon-family))
                              'display '(raise 0))
                  (propertize (format " %s" (cdr up)))))
             (if dn
                 (concat
                  (propertize (format " %s" (all-the-icons-faicon "arrow-circle-o-down"))
                              'face `(:height 1 :family ,(all-the-icons-faicon-family))
                              'display '(raise 0))
                  (propertize (format " %s" (cdr dn))))))))))

(defun custom-modeline-region-info ()
  (when mark-active
    (let ((words (count-lines (region-beginning) (region-end)))
          (chars (count-words (region-end) (region-beginning))))
      (concat
       (propertize (format "   %s" (all-the-icons-octicon "pencil") words chars)
                   'face `(:family ,(all-the-icons-octicon-family))
                   'display '(raise -0.0))
       (propertize (format " (%s, %s)" words chars)
                   'face `(:height 0.9))))))

(defun beginning ()
  (concat
   (gzy-propertize-evil-mode)
   (gzy-propertize-filename)
   (gzy-propertize-git-branch)
   (gzy-propertize-git-changes)))

(defun end ()
  (let ((bg (if (active)
                -gzy-current-accent
              -gzy-inactive-background))
        (fg (if (active)
                -gzy-dark-foreground
              "#44475a")))
    (propertize (format-mode-line " %l:%c ")
                'face `(:background ,bg :foreground ,fg))))

(defun mode-line-fill (reserve)
  "Return empty space leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (if (and window-system (eq 'right (get-scroll-bar-mode)))
      (setq reserve (- reserve 3))
    (setq reserve (- reserve 0.85)))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))))

(setq-default mode-line-format (list
                                '(:eval (gzy-propertize-evil-mode))
                                '(:eval (gzy-spacer))
                                '(:eval (gzy-segment/project-name))
                                '(:eval (gzy-propertize-filename))
                                '(:propertize (:eval (concat
                                                      (gzy-propertize-git-branch)
                                                      (gzy-propertize-git-changes)))
                                              face mode-line-directory)
                                '(:propertize (:eval (mode-line-fill (length (end))))
                                              face mode-line-directory)
                                '(:eval (end))))
