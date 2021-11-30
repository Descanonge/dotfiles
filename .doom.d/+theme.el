;;; +theme.el -*- lexical-binding: t; -*-

;;; Frames
;; Set first frame to maximised
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(add-hook 'before-make-frame-hook
          #'(lambda ()
              (add-to-list 'default-frame-alist '(left   . 0))
              (add-to-list 'default-frame-alist '(top    . 0))
              (add-to-list 'default-frame-alist '(height . 30))
              (add-to-list 'default-frame-alist '(width  . 90))))

;;; Theme
(load-theme 'doom-one-light t)
(doom-themes-org-config)

(after! doom-modeline
  (setq size-indication-mode nil
        doom-modeline-buffer-encoding nil
        doom-modeline-checker-simple-format nil
        doom-modeline-vcs-max-length 30
        doom-modeline-percent-position nil)

  (remove-hook! 'doom-modeline-mode-hook 'size-indication-mode)
  )

;;; Fill column
(after! display-fill-column-indicator
  ;; (setq-default display-fill-column-indicator-character ?│)
  (setq-default display-fill-column-indicator-character ?|)
  (defun set-face-fci ()
    ""
    (let* ((bk (face-background 'default nil 'default))
          (fg (color-name-to-rgb (face-foreground 'default nil 'default)))
          (bg (color-name-to-rgb bk))
          mod fl bl)
      (setq fl (nth 2 (apply 'color-rgb-to-hsl fg)))
      (setq bl (nth 2 (apply 'color-rgb-to-hsl bg)))
      (setq mod (cond ((< fl bl) -1) ((> fl bl) 1) ((< 0.5 bl) -1) (t 1)))
      (set-face-foreground 'fill-column-indicator (color-lighten-name bk (* mod 10))))
    )

  (custom-set-faces
  '(fill-column-indicator ((t (:inherit default)))))
  (set-face-fci)
  )

(add-hook! 'python-mode-hook #'display-fill-column-indicator-mode)
(add-hook! 'rst-mode-hook #'display-fill-column-indicator-mode)
(add-hook! 'mail-mode-hook #'display-fill-column-indicator-mode)

;;; Line numbers
(setq display-line-numbers-type 'relative)

;;; Parrot
(use-package! parrot
  :config
  (map! :map evil-normal-state-map
        "!" #'parrot-rotate-next-word-at-point)
  (parrot-mode)
  (setq parrot-directory (concat doom-private-dir "parrot/"))
  (parrot-set-parrot-type 'default)
  (setq parrot-rotate-highlight-after-rotation nil
        parrot-animation-frame-interval 0.030)

  (dolist (entry '((:rot ("frt" "bkg") :caps t :upcase t)
                   (:rot (">" "<") :caps t :upcase t)
                   (:rot ("lon" "lat") :caps t :upcase t)))
    (add-to-list 'parrot-rotate-dict entry))

  (defun parrot-start-animation-advice (old-function &rest arguments)
    (parrot-start-animation))
  (advice-add 'evil-ex-substitute :after #'parrot-start-animation-advice))

;;; Info-mode dedicated window
(set-popup-rules!
  '(("^\\*info" :slot 2 :side bottom :heigth 110 :quit nil)))
