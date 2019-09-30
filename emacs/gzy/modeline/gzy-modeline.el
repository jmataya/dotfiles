;;; gzy-modeline.el --- A simple and clean modeline.

;; Copyright (C) 2019 jmataya

;; Author: jmataya <hi@jeffmataya.com>
;; URL: https://github.com/jmataya/dotfiles
;; Version: 0.0.1

;;; Commentary:

;; To use this file, put something like the following in your
;; ~/.emacs:
;;
;; (add-to-list 'load-path "/directory/containing/modeline/")
;; (require 'gzy-modeline)
;;
;; Type M-x gzy-modeline
;;

;;; Code:

;;
;; Customization
;;

(defgroup gzy-modeline nil
  "Options for gzy-modeline."
  :prefix "gzy-"
  :group 'faces)

;;
;; Variables
;;

(defvar gzy-evil-settings '(("<N>" . ((text . "NORMAL")
                                      (color . :base0B)))
                            ("<I>" . ((text . "INSERT")
                                      (color . :base0D)))
                            ("<V>" . ((text . "VISUAL")
                                      (color . :base09)))
                            ("<R>" . ((text . "REPLACE")
                                      (color . :base08)))
                            ("<O>" . ((text . "OPERATOR PENDING")
                                      (color . :base0E)))))

;;
;; Global methods
;;

(defun gzy-modeline--filename ()
  "Gets the current filename with its corresponding icon."
  (propertize (format "%s" (buffer-name))))

(defun gzy-test ()
  (propertize (format "--%s--" evil-mode-line-tag)))

;;;###autoload
(defun gzy-modeline ()
  "Renders the modeline."
  (setq mode-line-format
        '(:eval (list (gzy-modeline--filename)
                      (gzy-test)))))

(provide 'gzy-modeline)
;;; gzy-modeline.el ends here

