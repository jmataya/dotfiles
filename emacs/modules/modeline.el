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
   (propertize (format " •" ))
   (if (stringp buffer-file-name)
       (let ((buffer-icon (all-the-icons-icon-for-file (buffer-file-name))))
         (if buffer-icon
             (propertize (format " %s" (all-the-icons-icon-for-file (buffer-file-name)))
                         'face `(:height 1 :family ,(all-the-icons-fileicon-family))
                         'display '(raise 0)))))
   (propertize (format " %s" (buffer-name)))
   (if (buffer-modified-p)
       (propertize (format " (+)")))))

(defun gzy-propertize-evil-mode ()
  "Formats the Evil mode for the modeline."
  (when evil-mode-line-tag
    (cond
     ((string-match "<N>" evil-mode-line-tag)
      (propertize (format " %s" "NORMAL")))
     ((string-match "<I>" evil-mode-line-tag)
      (propertize (format " %s" "INSERT")))
     ((string-match "<V>" evil-mode-line-tag)
      (propertize (format " %s" "VISUAL")))
     ((string-match "<R>" evil-mode-line-tag)
      (propertize (format " %s" "REPLACE")))
     ((string-match "<O>" evil-mode-line-tag)
      (propertize (format " %s" "OPERATOR-PENDING")))
     ((string-match "<M>" evil-mode-line-tag)
      (propertize (format " %s" "MOTION")))
     ((string-match "<E>" evil-mode-line-tag)
      (propertize (format " %s" "EMACS")))
     (t
      (propertize (format "%s" evil-mode-line-tag))))))

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

(setq-default mode-line-format '(:propertize
                                 (:eval
                                  (concat
                                   (gzy-propertize-evil-mode)
                                   (gzy-propertize-filename)
                                   (gzy-propertize-git-branch)
                                   (gzy-propertize-git-changes)))
                                 face mode-line-directory))
