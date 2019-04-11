(require 'all-the-icons)
(require 'cl-lib)
(require 'evil)

(defun gzy-has-substr (test str)
  "Checks to see if the string TEST is in the STR."
  (string-match-p (regexp-quote test) str))

(defun gzy-trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
  White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun gzy-propertize-filename ()
  "Gets the current filename with its corresponding icon."
  (concat
   (if (stringp buffer-file-name)
       (let
           ((buffer-icon (all-the-icons-icon-for-file (buffer-file-name))))
         (if buffer-icon
             (propertize (format " %s" (all-the-icons-icon-for-file (buffer-file-name)))
                         'face `(:height 1 :family ,(all-the-icons-fileicon-family))
                         'display '(raise 0)))))
   (propertize (format " %s" (buffer-name)))
   (if (buffer-modified-p)
       (propertize (format " (+)")))))

(defun gzy-propertize-evil-mode ()
  "Formats the Evil mode for the modeline."
  (let* ((evil-settings
          '((" <N> " . ((name . "NORMAL")))
            (" <I> " . ((name . "INSERT")))
            (" <V> " . ((name . "VISUAL")))
            (" <R> " . ((name . "REPLACE")))
            (" <O> " . ((name . "OPERATOR-PENDING")))
            (" <M> " . ((name . "MOTION")))
            (" <E> " . ((name . "EMACS")))))
         (current-setting (assoc evil-mode-line-tag evil-settings))
         (evil-name (if current-setting
                        (format " %s " (alist-get 'name (cdr current-setting)))
                      (format "%s" evil-mode-line-tag))))
    (propertize evil-name)))

(defun gzy-propertize-git-branch ()
  "Reads the current git branch and formats it for the modeline."
  (setq branch (gzy-trim-string
                (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
  (if (or (equal branch "") (gzy-has-substr "not a git repository" branch))
      (propertize (format ""))
    (concat
     (propertize (format " • %s" (all-the-icons-octicon "git-branch"))
                 'face `(:height 1 :family ,(all-the-icons-faicon-family))
                 'display '(raise 0))
     (propertize (format " %s" branch)))))


(defun gzy-extract-git-changes ()
  "Extracts the current number of file changes, insertions, and deletions
   from inside a git repository."
  (setq summary (split-string
                 (shell-command-to-string "git diff --stat | grep changed")
                 ", "))
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
             :initial-value '()))

(defun gzy-propertize-git-changes ()
  "Reads git changes and formats them for the modeline."
  (setq change-details (gzy-extract-git-changes))
  (if (assoc "ch" change-details)
      (concat
       (propertize (format " •"))
       (if (setq a-up (assoc "up" change-details)) 
           (concat
            (propertize (format " %s" (all-the-icons-faicon "arrow-circle-o-up"))
                        'face `(:height 1 :family ,(all-the-icons-faicon-family))
                        'display '(raise 0))
            (propertize (format " %s" (cdr a-up)))))
       (if (setq a-down (assoc "down" change-details))
           (concat
            (propertize (format " %s" (all-the-icons-faicon "arrow-circle-o-down"))
                        'face `(:height 1 :family ,(all-the-icons-faicon-family))
                        'display '(raise 0))
            (propertize (format " %s" (cdr a-down))))))
    (propertize (format ""))))

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
  (format-mode-line "Ln %l, Col %c "))

(defun mode-line-fill (reserve)
  "Return empty space leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))))

(setq-default mode-line-format (list
                                '(:propertize (:eval (beginning))
                                              face mode-line-directory)
                                '(:propertize (:eval (mode-line-fill (length (end))))
                                              face mode-line-directory)
                                '(:propertize (:eval (end))
                                              face mode-line-directory)))
