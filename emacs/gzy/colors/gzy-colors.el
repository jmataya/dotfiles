;;; gzy-colors.el --- A series of methods to allow for on-the-fly color settings.

;; Copyright (C) 2019 jmataya

;; Author: jmataya <hi@jeffmataya.com>
;; URL: https://github.com/jmataya/dotfiles
;; Version: 0.0.1

;;; Commentary:

;; To use this file, put something like the following in your
;; ~/.emacs:
;;
;; (add-to-list 'load-path "/directory/containing/colors/")
;; (require 'gzy-colors)
;;
;; Type M-x gzy-colors
;;

;;; Code:

;;
;; Customization
;;

(defgroup gzy-colors nil
  "Options for gzy-colors."
  :prefix "gzy-"
  :group 'faces)

(defcustom gzy-default-theme 'light
  "Whether the light or dark theme should be the default."
  :group 'gzy-colors
  :type '(choice (const light)
                 (const dark)))

(defcustom gzy-light-theme nil
  "The light theme to use."
  :type 'symbol
  :group 'gzy-colors)

(defcustom gzy-dark-theme nil
  "The dark theme to use."
  :type 'symbol
  :group 'gzy-colors)

(defcustom gzy-colors-kbd-prefix "C-x t"
  "The prefix to use for key bindings."
  :type 'string
  :group 'gzy-colors)

(defcustom gzy-blend-fringe t
  "If true, the fringe will use the default text foreground and background."
  :group 'gzy-colors
  :type 'boolean)
;;
;; Global methods
;;

(defun gzy-colors--theme-fringe ()
  "Sets the fringe's theme."
  (if gzy-blend-fringe
      (set-face-attribute 'fringe nil
                          :foreground (face-foreground 'default)
                          :background (face-background 'default))))

(defun gzy-colors--set-default-theme ()
  "Sets the default color scheme."
  (let ((theme (if (eq gzy-default-theme 'light)
                   gzy-light-theme
                 gzy-dark-theme)))
    (gzy-colors--set-theme theme gzy-blend-fringe)))

(defun gzy-colors--set-theme (theme blend-fringe)
  "Sets theme and decides whether to blend the fringe in."
  (when theme
    (let ((ansi-term-backup (if (boundp 'ansi-term-color-vector)
                                ansi-term-color-vector)))
      (load-theme theme t)
      (if (boundp 'ansi-term-color-vector)
          (setq ansi-term-color-vector ansi-term-backup))))
  (gzy-colors--theme-fringe))

(defun gzy-colors--set-light-theme ()
  (interactive)
  (gzy-colors--set-theme gzy-light-theme gzy-blend-fringe))

(defun gzy-colors--set-dark-theme ()
  (interactive)
  (gzy-colors--set-theme gzy-dark-theme gzy-blend-fringe))

;;;###autoload
(defun gzy-colors ()
  "Load everything up and set the default theme."
  (interactive)
  (gzy-colors--set-default-theme)
  (global-set-key (kbd "C-c t l") 'gzy-colors--set-light-theme)
  (global-set-key (kbd "C-c t d") 'gzy-colors--set-dark-theme))

(provide 'gzy-colors)
;;; gzy-colors.el ends here

