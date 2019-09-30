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

(add-to-list 'load-path "~/.emacs.d/gzy/colors/")
(add-to-list 'load-path "~/.emacs.d/gzy/fonts/")
(add-to-list 'load-path "~/.emacs.d/gzy/modeline/")

(load-file "~/.emacs.d/modules/baseline.el")
(load-file "~/.emacs.d/modules/coding.el")
(load-file "~/.emacs.d/modules/evil.el")
(load-file "~/.emacs.d/modules/global-term.el")
(load-file "~/.emacs.d/modules/magit.el")
(load-file "~/.emacs.d/modules/navigation.el")
(load-file "~/.emacs.d/modules/theme.el")
;; (load-file "~/.emacs.d/modules/modeline.el")

(load-file "~/.emacs.d/modules/languages/lisp.el")

(require 'gzy-modeline)
(gzy-modeline)
