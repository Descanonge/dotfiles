;;; +theme.el -*- lexical-binding: t; -*-

;;; Frames
;; Set first frame to maximised
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(add-hook 'before-make-frame-hook
          (lambda ()
              (add-to-list 'default-frame-alist '(left   . 0))
              (add-to-list 'default-frame-alist '(top    . 0))
              (add-to-list 'default-frame-alist '(height . 30))
              (add-to-list 'default-frame-alist '(width  . 90))))

;;; Theme
(use-package! doom-themes
  :ensure t
  :config
  (load-theme 'doom-one-light t)
  (doom-themes-org-config)
  )

(use-package! doom-modeline
  :ensure t
  :config
  (setq doom-modeline-buffer-encoding nil
        doom-modeline-modal nil
        doom-modeline-enable-word-count t
        doom-modeline-env-version nil
        doom-modeline-checker-simple-format nil
        doom-modeline-vcs-max-length 30
        doom-modeline-percent-position nil)
  (remove-hook! 'doom-modeline-mode-hook 'size-indication-mode)

  (add-to-list 'mode-line-misc-info
               '(auto-fill-function "⏎" ""))

  (add-to-list 'mode-line-misc-info
               '(:eval (when (featurep 'lispyville)
                         (lispyville-mode-line-string))))

  (doom-modeline-def-segment buffer-position
    "The buffer position information.

Rewritten from doom-modeline-segments.el.
Changed line display to have total number of lines and removed
what I did not need.
"
    (let ((active (doom-modeline--active))
          (lc '("%l/"
                (:eval (int-to-string (count-lines (point-min) (point-max))))
                ":%c"))
          (mouse-face 'doom-modeline-highlight)
          (local-map mode-line-column-line-number-mode-map))
      (concat
       doom-modeline-wspc

       ;; Line and column
       (propertize (format-mode-line lc)
                   'help-echo "Buffer position\n\
mouse-1: Display Line and Column Mode Menu"
                   'mouse-face mouse-face
                   'local-map local-map)
       doom-modeline-spc)))
  )

;;; Fill column
(use-package! display-fill-column-indicator
  :demand t
  :config (setq display-fill-column-indicator-character ?|)
  :hook ((python-mode rst-mode mail-mode) . display-fill-column-indicator-mode)
  )

;;; Line numbers
(setq display-line-numbers-type 'relative)

;;; Parrot
(use-package! parrot
  :demand t
  :config
  (map! :n "!" #'parrot-rotate-next-word-at-point)
  (setq parrot-type 'default) ; so that first activation of parrot mode is silent
  (parrot-mode)
  (setq parrot-rotate-highlight-after-rotation nil
        parrot-animation-frame-interval 0.030)

  ;; Load all images
  (defcustom me/parrot-types '(confused default emacs nyan rotating science thumbsup)
    "Parrot types available."
    :type '(repeat symbol)
    :group 'me)
  (defvar me/parrot-frames
    (mapcar
     (lambda (parrot)
       (setq parrot-frame-list (number-sequence 1 (parrot-sequence-length parrot)))
       (parrot-load-frames parrot)
       (cons parrot parrot-animation-frames))
     me/parrot-types))

  (setq parrot-static-image
        (create-image (concat doom-user-dir "parrot/img/transparent.xpm")
                      'xpm nil :ascent 'center))

  (defvar me/parrot-lengths
    '((confused . 38)
      (default . 10)
      (emacs . 10)
      (nyan . 10)
      (rotating . 13)
      (science . 10)
      (thumbsup . 12)))
  (defun parrot-sequence-length (parrot)
    (cond ((alist-get parrot me/parrot-lengths))
          (t (error (format "Invalid parrot %s" parrot)))))

  (defun me/parrot-type-random ()
    (let ((parrot (seq-random-elt me/parrot-types)))
      (setq parrot-type parrot)
      (setq parrot-frame-list (number-sequence 1 (parrot-sequence-length parrot)))
      (setq parrot-animation-frames (alist-get parrot me/parrot-frames))
      (if (eq parrot 'confused)
          (setq parrot-num-rotations 1)
        (setq parrot-num-rotations 3))
      parrot))
  (advice-add #'parrot-start-animation :before #'me/parrot-type-random)

  (defun parrot-start-animation-advice (&rest _)
    (parrot-start-animation))
  (advice-add #'evil-ex-substitute :after #'parrot-start-animation-advice))

;;; Info-mode dedicated window
(set-popup-rules!
  '(("^\\*info" :slot 2 :side bottom :heigth 110 :quit nil)))

;; ;;; Emoji
;; (use-package! emojify
;;   :defer t
;;   :config
;;   (setq emojify-emojis-dir (concat doom-user-dir "emojis/openmoji-72x72-color")
;;         emojify-display-style 'image
;;         emojify-emoji-set "openmoji-v14.0-72")

;;   (setq emojify-user-emojis
;;         '(("🏳️‍⚧️" . (("name" . "Transgender Flag")
;;                    ("image" . "1F3F3-FE0F-200D-26A7-FE0F.png")
;;                    ("style" . "github")))))
;;   )
