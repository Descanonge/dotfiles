;;; +spell.el -*- lexical-binding: t; -*-

;; Lazy load built-in flyspell config at startup
(use-package! flyspell
  :disabled
  :preface
  (defer-feature! flypsell flyspell-mode flyspell-prog-mode)
  :config
  (setq flyspell-mark-duplications-flag nil
        flyspell-duplicate-distance 0)
  )


(after! ispell
  ;; Don't spellcheck org blocks
  (pushnew! ispell-skip-region-alist
            '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
            '("#\\+BEGIN_SRC" . "#\\+END_SRC")
            '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

  ;; Use hunspell
  (setq ispell-program-name "hunspell")
  )

;;; Grammalecte
(use-package! flycheck-grammalecte
  :defer t
  :config
  (setq flycheck-grammalecte-report-grammar t
        flycheck-grammalecte-report-spellcheck nil
        flycheck-grammalecte-report-apos nil)
  (setq flycheck-grammalecte-enabled-modes
        '(latex-mode
          mail-mode
          message-mode))
  (flycheck-grammalecte-setup)

  (defun me/flycheck-grammalecte-p ()
    "Check if grammalecte should be used in current buffer.
Current implementation looks at ispell-local-dictionary."
    (and (boundp 'ispell-local-dictionary)
         (member ispell-local-dictionary
                 '("fr" "fr_CUS" "fr_PERS" "fr_FR" "francais7" "francais-tex"))))
  (setq flycheck-grammalecte-predicate #'me/flycheck-grammalecte-p)

  (setq flycheck-grammalecte-filters-by-mode nil)
  (add-to-list 'flycheck-grammalecte-filters-by-mode
               '(latex-mode
                 "(?s)\\\\begin{(equation|verbatim|luacode*)}.*?\\\\end{\\1}"
                 "\\\\\\w+(?:\\[[^]]+\\])?(?:{[^}]*}(?:\\[[^]]*\\])?)?"
                 "\\\\(?:title|(?:sub)*section){([^}]+)}"
                 "}{"))

  (defun me/flycheck-grammalecte-setup-latex ()
    (setq flycheck-grammalecte-report-esp nil
          flycheck-grammalecte-report-nbsp nil)
    (flycheck-grammalecte-setup))
  (add-hook 'LaTeX-mode-hook 'me/flycheck-grammalecte-setup-latex)
  )

;;; Spelling
(use-package! ispell
  :defer nil
  :init
  (defun me/ispell ()
    "Resume an ispell session or start a new one."
    (interactive)
    (if (not (marker-position ispell-region-end))
        (ispell-buffer)
      (ispell-continue)))
  (map! :map 'evil-normal-state-map
        :n :desc "Start/continue iSpell" "zi" #'me/ispell)

  :config
  (setq ispell-personal-dictionary "~/.config/hunspell/pers_wordlist.txt"
        ispell-dictionary "fr_CUS")
  (ispell-set-spellchecker-params)
  (ispell-change-dictionary "fr_CUS" t)

  (defun ispell-display-buffer (buffer)
    "Show BUFFER in new window above selected one.
Also position fit window to BUFFER and select it.

Redefined from textmodes/ispell.el so that the \"choices\" buffer opens
at bottom of frame.
"
    (let* ((unsplittable
            (cdr (assq 'unsplittable (frame-parameters (selected-frame)))))
           (window
            (or (get-buffer-window buffer)
                (and unsplittable
                     ;; If frame is unsplittable, temporarily disable that...
                     (let ((frame (selected-frame)))
                       (modify-frame-parameters frame '((unsplittable . nil)))
                       (prog1
                           (condition-case nil
                               (split-window
                                ;; Chose the last of a window group, since
                                ;; otherwise, the lowering of another window's
                                ;; TL corner would cause the logical order of
                                ;; the windows to be changed.
                                (car (last (selected-window-group)))
                                (- ispell-choices-win-default-height) 'below)
                             (error nil))
                         (modify-frame-parameters frame '((unsplittable . t))))))
                (and (not unsplittable)
                     (condition-case nil
                         (split-window
                          ;; See comment above.
                          (car (last (selected-window-group)))
                          (- ispell-choices-win-default-height) 'below)
                       (error nil)))
                (display-buffer buffer))))
      (if (not window)
          (error "Couldn't make window for *Choices*")
        (select-window window)
        (set-window-buffer window buffer)
        (set-window-point window (point-min))
        (fit-window-to-buffer window nil nil nil nil t))))
)
