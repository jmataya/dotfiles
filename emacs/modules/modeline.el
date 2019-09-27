(require 'all-the-icons)
(require 'cl-lib)
(require 'evil)

;;
;; Colors
;; By default, the modeline is based off base16 themes.
;;

(defvar +gzy/color-scheme base16-one-light-colors)
(defvar +gzy/modeline-height 0.85)

(defun gzy/set-face-mode-line (color-palette)
  "Sets the font style for the base mode-line style."
  (let ((foreground-color (plist-get color-palette :base04))
        (background-color (plist-get color-palette :base01)))
    (set-face-attribute 'mode-line nil
                        :foreground foreground-color
                        :background background-color
                        :height +gzy/modeline-height
                        :box nil)))

(defun gzy/set-face-mode-line-inactive (color-palette)
  "Sets the font style for the inactive mode-line style."
  (let ((foreground-color (plist-get color-palette :base03))
        (background-color (plist-get color-palette :base00)))
    (set-face-attribute 'mode-line-inactive nil
                        :foreground foreground-color
                        :background background-color
                        :height +gzy/modeline-height
                        :box nil)))
;;
;; Determine which window is active.
;; Based off the amazing Doom: https://github.com/hlissner/doom-emacs
;;

(defsubst active ()
  (eq (selected-window) -gzy-current-window))

(defvar -gzy-current-window (frame-selected-window))
(defun gzy-set-selected-window (&rest _)
  "Sets -gzy-current-window with the appropriately selected window."
  (let ((win (frame-selected-window)))
    (when win
      (unless (minibuffer-window-active-p win)
        (setq -gzy-current-window win)))))

(add-hook 'window-configuration-change-hook #'gzy-set-selected-window)
(add-hook 'focus-in-hook #'gzy-set-selected-window)
(advice-add #'handle-switch-frame :after #'gzy-set-selected-window)
(advice-add #'select-window :after #'gzy-set-selected-window)

;;
;; Colors
;;

(defvar -gzy-dark-foreground "#282a36")
(defvar -gzy-inactive-background "#282a36")
(defvar -gzy-height 0.85)
(defvar -gzy-current-accent -gzy-inactive-background)

(gzy/set-face-mode-line +gzy/color-scheme)
(gzy/set-face-mode-line-inactive +gzy/color-scheme)

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
               'face `(:height 1.5)
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
     (if (and (stringp buffer-file-name) (display-graphic-p))
         (let
             ((buffer-icon (all-the-icons-icon-for-file (buffer-file-name))))
           (if buffer-icon
               (concat 
                (propertize (format "%s" (all-the-icons-icon-for-file (buffer-file-name)))
                            'face `(:foreground ,color :height 1.2 :family ,(all-the-icons-fileicon-family))
                            'display '(raise -0.17))
                (propertize " "
                            'face `(:foreground ,color))))))
     (propertize (format "%s" (buffer-name))
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
                           ("MOTION" . "#f1fa8c")
                           ("EMACS" . "#ffb86c")))

;; (defun gzy/evil-colors ()
;;   "Returns a list of colors to use for evil mode in the modeline."
;;   '(("NORMAL" . `(plist-get +gzy/color-scheme :base0B))
;;     ("INSERT" . ,(plist-get +gzy/color-scheme :base0D))
;;     ("VISUAL" . ,(plist-get +gzy/color-scheme :base09))
;;     ("REPLACE" . ,(plist-get +gzy/color-scheme :base08))
;;     ("OPERATOR-PENDING" . ,(plist-get +gzy/color-scheme :base0E))
;;     ("MOTION" . ,(plist-get +gzy/color-scheme :base0E))
;;     ("EMACS" . ,(plist-get +gzy/color-scheme :base0D))))
(defun gzy/evil-colors (mode)
  "Returns a list of colors to use for evil in the modeline."
  (let ((color (cond
                ((eq mode 'normal) :base0B)
                ((eq mode 'insert) :base0D)
                ((eq mode 'visual) :base09)
                ((eq mode 'replace) :base08)
                ((eq mode 'operator-pending) :base0E)
                ((eq mode 'motion) :base0E)
                ((eq mode 'emacs) :base0E)
                (t :base0B))))
    (plist-get +gzy/color-scheme color)))

(defun gzy-propertize-evil-mode ()
  "Formats the Evil mode for the modeline."
  (let* ((name (cdr (assoc evil-mode-line-tag -gzy-evil-names)))
         (color (cdr (assoc name (gzy/evil-colors))))
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
             (if (display-graphic-p)
                 (propertize (format " • %s" (all-the-icons-octicon "git-branch"))
                             'face `(:height 1 :family ,(all-the-icons-faicon-family))
                             'display '(raise 0)))
             (propertize (format " %s" branch)))))))

(defun gzy-extract-git-changes ()
  "Extracts the current number of file changes, insertions, and deletions
   from inside a git repository."
  (let* ((git-diff (shell-command-to-string "git diff --stat | grep changed"))
         (summary (split-string (gzy-trim-string git-diff) ", ")))
    (fset 'new-alist (lambda (key val)
                       (cons (cons key val) ())))
    (fset 'extract-num (lambda (str)
                         (car (split-string str " "))))
    (cl-reduce (lambda (acc elt)
                 (append
                  (if (equal acc nil) '() acc)
                  (let ((num (extract-num elt))
                        (key (cond
                              ((gzy-has-substr "changed" elt) 'ch)
                              ((gzy-has-substr "insertion" elt) 'up)
                              ((gzy-has-substr "deletion" elt) 'down))))
                    (if (and key num)
                        `((,key . ,num))
                      acc))))
               summary
               :initial-value '())))

(defun gzy-propertize-git-changes ()
  "Reads git changes and formats them for the modeline."
  (if (active)
      (let* ((change-details (gzy-extract-git-changes))
             (ch (assoc 'ch change-details))
             (up (assoc 'up change-details))
             (dn (assoc 'down change-details)))
        (if ch
            (concat
             (propertize (format " •"))
             (if up
                 (concat
                  (if (display-graphic-p)
                      (propertize (format " %s " (all-the-icons-faicon "arrow-circle-o-up"))
                                  'face `(:height 1 :family ,(all-the-icons-faicon-family))
                                  'display '(raise 0))
                    " +")
                  (propertize (format "%s" (cdr up)))))
             (if dn
                 (concat
                  (if (display-graphic-p)
                      (propertize (format " %s " (all-the-icons-faicon "arrow-circle-o-down"))
                                  'face `(:height 1 :family ,(all-the-icons-faicon-family))
                                  'display '(raise 0))
                    " -")
                  (propertize (format "%s" (cdr dn))))))))))

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
      (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))))

;; (setq-default mode-line-format (list
;;                                 '(:eval (gzy-propertize-evil-mode))
;;                                 '(:eval (gzy-spacer))
;;                                 '(:eval (gzy-segment/project-name))
;;                                 '(:eval (gzy-propertize-filename))
;;                                 ;; '(:propertize (:eval (concat
;;                                 ;;                       (gzy-propertize-git-branch)
;;                                 ;;                       (gzy-propertize-git-changes)))
;;                                 ;;               face mode-line-directory)
;;                                 '(:propertize (:eval (mode-line-fill (length (end))))
;;                                               face mode-line-directory)
;;                                 '(:eval (end))))

(defun gzy/render-modeline ()
  (setq-default mode-line-format (list
                                  '(:eval (gzy-propertize-filename)))))

(gzy/render-modeline)
