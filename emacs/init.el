(package-initialize)

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

; (org-babel-load-file "~/.emacs.d/configuration-v2.org")
; (load-file "~/.emacs.d/modules/modeline.el")
					; (org-babel-load-file "~/.emacs.d/mac.org")
(load-file "~/.emacs.d/modules/baseline.el")
(load-file "~/.emacs.d/modules/evil.el")
(load-file "~/.emacs.d/modules/global-term.el")
(load-file "~/.emacs.d/modules/magit.el")
(load-file "~/.emacs.d/modules/theme.el")

(load-file "~/.emacs.d/modules/languages/lisp.el")
