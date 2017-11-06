
(defgroup monkey-mode-line nil
  "Custom mode line configuration.")

(require 'evil)

(defgroup evil-mode-line nil
  "Mode line configuration for Evil."
  :group 'monkey-mode-line)

(defcustom evil-state-msgs
  `((normal  . " NORMAL ")
    (insert  . " INSERT ")
    (replace . " REPLACE ")
    (emacs   . " EMACS "))
  "Mode line messages for non-visual Evil states."
  :type '(list (cons symbol string))
  :group 'evil-mode-line)

(defcustom evil-visual-state-msgs
  `((normal . " VISUAL ")
    (line   . " V-LINE ")
    (block  . " V-BLOCK "))
  "Mode line messages for visual Evil states."
  :type '(list (cons symbol string))
  :group 'evil-mode-line)

(defun evil-mode-line-state-msg (&optional state)
  "Find the message that should represent the STATE in the mode line."
  (unless state (setq state evil-state))
  (cond
   ((evil-visual-state-p)
    (or (cdr (assq (evil-visual-type) evil-visual-state-msgs))
        (cdr (assq 'normal evil-visual-state-msgs))))
   (t (or (cdr (assq state evil-state-msgs))
          ""))))

(defvar evil-mode-line-msg (evil-mode-line-state-msg 'emacs-state))

(defun update-evil-mode-line-state-msg ()
  "Update `evil-mode-line-msg'."
  (condition-case ()
      (progn
        (set (make-local-variable 'evil-mode-line-msg)
             (evil-mode-line-state-msg)))
    (error nil)))

(defadvice evil-refresh-mode-line (after update-evil-mode-line-state-msg activate)
  "Update our own mode state by `update-evil-mode-line-state-msg'."
  (update-evil-mode-line-state-msg))

(use-package all-the-icons
  :ensure t
  :pin melpa-stable)

(require 'all-the-icons)

(defgroup monkey-vc-mode-line nil
  "Mode line configuration for Git and other version control."
  :group 'monkey-mode-line)

(defun monkey-vc-mode-line-msg ()
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
      (propertize (format " %s" (all-the-icons-octicon "git-branch"))
                  'face `(:height 1 :family ,(all-the-icons-octicon-family))
                  'display '(raise 0))
      (propertize (format " %s" branch)))))

(defvar vc-mode-line-msg
  '(:propertize
    (:eval (when vc-mode
    (cond
      ((string-match "Git[:-]" vc-mode) (monkey-vc-mode-line-msg))
      (t (format "%s" vc-mode)))))
     face mode-line-directory)
   "Formats the current director.")

(defun mode-line-file-info ()
  (if (stringp buffer-file-name)
      (file-name-nondirectory buffer-file-name)
    (buffer-name)))

(defvar mode-line-file-info-msg (mode-line-file-info))

(defun update-file-name-mode-line-msg ()
  "Update `mode-line-file-info-msg'."
  (condition-case ()
      (progn
        (set (make-local-variable 'mode-line-file-info-msg)
             (mode-line-file-info)))
    (error nil)))

(add-hook 'find-file-hook 'update-file-name-mode-line-msg)

(setq-default mode-line-format
              (list
               '("" evil-mode-line-msg)
               '("" mode-line-file-info-msg)
               " "
               mode-line-mule-info
               mode-line-modified
               mode-line-frame-identification
               " "
               mode-line-position
               vc-mode-line-msg
               "  "
               mode-line-modes))

(provide 'monkey-mode-line)
