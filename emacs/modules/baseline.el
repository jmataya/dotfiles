;; Modify default UI elements to be less invasive.

(blink-cursor-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)

;; Set defaults so that we start with a sane system and build on top.

(setq-default auto-window-vscroll nil
	      column-number-mode t
	      confirm-kill-emacs 'yes-or-no-p
	      cursor-in-non-selected-windows t
	      delete-by-moving-to-trash t
	      display-time-default-load-average nil
	      display-time-format "%H:%M"
	      help-window-select t
	      indent-tabs-mode nil
	      initial-scratch-message ""
	      inhibit-startup-message t
	      line-number-mode t
	      scroll-conservatively most-positive-fixnum
	      select-enable-clipboard t
	      sentence-end-double-space nil
	      show-trailing-whitespace nil
	      split-height-threshold nil
	      split-width-threshold nil
	      tab-width 4
	      uniquity-buffer-name-style 'forward
	      visual-bell t
	      window-combination-resize t
	      x-stretch-cursor t)

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(global-linum-mode t)
(setq linum-format "%4d ")
(global-subword-mode 1)

;; Enable garbage collection on focus-out

(add-hook 'focus-out-mode #'garbage-collect)

;; Make sure the path is being pulled in correctly

(setq-default exec-path (append exec-path '("/usr/local/bin")))

