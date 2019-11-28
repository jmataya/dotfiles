;; Configure projectile for navigating projects.

(use-package projectile :ensure t)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Ivy, Counsel, and Swiper for enhanced minibuffer functionality.

(use-package ivy :ensure t)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(global-set-key "\C-s" 'swiper)

;; Tie projectile and counsel together

(use-package counsel-projectile :ensure t)
(counsel-projectile-mode +1)

;; Window navigation

(global-set-key (kbd "C-c h") 'windmove-left)          ; move to left window
(global-set-key (kbd "C-c l") 'windmove-right)        ; move to right window
(global-set-key (kbd "C-c k") 'windmove-up)              ; move to upper window
(global-set-key (kbd "C-c j") 'windmove-down)          ; move to lower window

(defun ansi-term-char-mode ()
  (if (string= (buffer-name) "*ansi-term*")
    (term-char-mode)))

(defadvice windmove-left (before windmove-left-ansi-term (&optional arg))
  (ansi-term-char-mode))

(defadvice windmove-right (before windmove-right-ansi-term (&optional arg))
  (ansi-term-char-mode))

(defadvice windmove-up (before windmove-up-ansi-term (&optional arg))
  (ansi-term-char-mode))

(defadvice windmove-down (before windmove-down-ansi-term (&optional arg))
  (ansi-term-char-mode))

(add-hook 'term-load-hook
  (lambda ()
    (define-key term-raw-map (kbd "C-c h") 'windmove-left)
    (ad-activate 'windmove-left)
    (define-key term-raw-map (kbd "C-c l") 'windmove-right)
    (ad-activate 'windmove-right)
    (define-key term-raw-map (kbd "C-c k") 'windmove-up)
    (ad-activate 'windmove-up)
    (define-key term-raw-map (kbd "C-c j") 'windmove-down)
    (ad-activate 'windmove-down)))