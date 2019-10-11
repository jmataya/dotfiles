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

(defvar gzy-evil-normal-color "#50a14f")
(defvar gzy-evil-insert-color "#4078f2")
(defvar gzy-evil-visual-color "#d75f00")
(defvar gzy-evil-replace-color "#ca1243")
(defvar gzy-evil-operator-pending-color "#a626a4")
(defvar gzy-evil-motion-color "#a626a4")
(defvar gzy-evil-emacs-color "#a626a4")

(defvar gzy-evil-settings `((" <N> " . ((text . "NORMAL")
                                        (color . ,gzy-evil-normal-color)))
                            (" <I> " . ((text . "INSERT")
                                        (color . ,gzy-evil-insert-color)))
                            (" <V> " . ((text . "VISUAL")
                                        (color . ,gzy-evil-visual-color)))
                            (" <R> " . ((text . "REPLACE")
                                        (color . ,gzy-evil-replace-color)))
                            (" <O> " . ((text . "OPERATOR PENDING")
                                        (color . ,gzy-evil-operator-pending-color)))
                            (" <M> " . ((text . "MOTION")
                                        (color . ,gzy-evil-motion-color)))
                            (" <E> " . ((text . "EMACS")
                                        (color . ,gzy-evil-emacs-color)))))

;;
;; Faces
;;

(defface gzy-highlight
  '((t (:foreground "White" :background "DarkMagenta" :height 1.85)))
  "The highlight to show based on evil status."
  :group 'gzy-modeline :group 'font-lock-highlighting-faces)
(defvar gzy-highlight 'gzy-highlight)  

(set-face-attribute 'mode-line nil
                    :height 0.85)
  

  ;;
  ;; Global methods
  ;;


(defun gzy-modeline--evil-settings (mode-line-tag)
  "Returns the modeline settings for an evil tag."
  (cdr (assoc mode-line-tag gzy-evil-settings)))

(defun gzy-modeline--evil-color (mode-line-tag)
  "Inspects the current modeline tag and returns the evil color to display."
  (cdr (assoc 'color (gzy-modeline--evil-settings mode-line-tag))))

(defun gzy-modeline--evil-text (mode-line-tag)
  "Inspects the current modeline tag and returns the evil text to display."
  (cdr (assoc 'text (gzy-modeline--evil-settings mode-line-tag))))

(defun gzy-modeline--filename ()
  "Gets the current filename with its corresponding icon."
  (propertize (format "%s" (buffer-name))
              'face 'mode-line))

(defun gzy-modeline--evil ()
  (propertize (format "%s" (gzy-modeline--evil-text evil-mode-line-tag))
              'face gzy-highlight))

;;;###autoload
(defun gzy-modeline ()
  "Renders the modeline."
  (setq mode-line-format
        '(:eval (list (gzy-modeline--filename)
                      (gzy-modeline--evil)))))

(provide 'gzy-modeline)
;;; gzy-modeline.el ends here

