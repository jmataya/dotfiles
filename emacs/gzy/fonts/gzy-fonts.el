;;; gzy-fonts.el --- A series of methods to better manage fonts.

;; Copyright (C) 2019 jmataya

;; Author: jmataya <hi@jeffmataya.com>
;; URL: https://github.com/jmataya/dotfiles
;; Version: 0.0.1

;;; Commentary:

;; To use this file, put something like the following in your
;; ~/.emacs:
;;
;; (add-to-list 'load-path "/directory/containing/fonts/")
;; (require 'gzy-fonts)
;;
;; Type M-x gzy-font-face
;;

;;; Code:

;;
;; Customization
;;

(defgroup gzy-fonts nil
  "Options for gzy-fonts."
  :prefix "gzy-"
  :group 'faces)

(defcustom gzy-font-size 14
  "The default font size to use."
  :type 'integer
  :group 'gzy-fonts)

(defcustom gzy-line-spacing 0.40
  "The default line spacing to use."
  :type 'float
  :group 'gzy-fonts)

(defcustom gzy-font-faces nil
  "List of font faces to try to use."
  :type 'string
  :group 'gzy-fonts)

;;
;; Global methods
;;
(defun gzy-fonts--search-fonts (fonts font-size)
  "Searches through a list of fonts and returns the first one installed on the system."
  (if (and fonts font-size)
      (let* ((font-name (format "%s-%d" (car fonts) font-size))
             (font-face (find-font (font-spec :name font-name)))
             (rest-fonts (cdr fonts)))
        (if font-face
            font-name
          (if rest-fonts (gzy-fonts--search-fonts rest-fonts font-size))))))

(defun gzy-fonts--split-font-string (font-str)
  "Splits font-str into a list. Returns nil if empty."
  (if font-str
      (delete "" (split-string font-str ","))))

(defun gzy-fonts--find-font (fonts font-size)
  "Find the first matching font in fonts."
  (let ((faces (gzy-fonts--split-font-string fonts)))
    (if faces
        (gzy-fonts--search-fonts faces font-size))))

;;;###autoload
(defun gzy-font-face ()
  "Returns the correct font specs."
  (interactive)
  (let ((font (gzy-fonts--find-font gzy-font-faces gzy-font-size))
        (size (if gzy-font-size gzy-font-size)))
    (cond (font (set-frame-font font))
          (size (set-face-attribute 'default nil
                                    :height (* size 10)))))
  (setq-default line-spacing gzy-line-spacing))

(provide 'gzy-fonts)
;;; gzy-fonts.el ends here

