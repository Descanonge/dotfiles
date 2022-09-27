;;; +evil.el -*- lexical-binding: t; -*-

(setq avy-keys '(?n ?r ?t ?d ?e ?a ?i ?u))

;;; EVIL

(after! evil
  (setq evil-kill-on-visual-paste nil)
  ;; Scrolling
  (setq scroll-step 5)
  (evil-define-motion scroll-n-lines-up (count)
    "Scroll `scroll-step' up"
    (evil-scroll-line-up scroll-step))
  (evil-define-motion scroll-n-lines-down (count)
    "Scroll `scroll-step' down"
    (evil-scroll-line-down scroll-step))

  (map! :n "M-l" #'drag-stuff-up
        :n "M-a" #'drag-stuff-down

        :n "l" #'evil-insert-char
        :n "L" #'evil-append-char

        :nv "gs <up>" #'evilem-motion-previous-line
        :nv "gs <down>" #'evilem-motion-next-line

        :nv "gC" #'evilnc-copy-and-comment-operator

        :map (override evil-motion-state-map)

        :map doom-leader-toggle-map
        :desc "Visual line mode" "v" #'visual-line-mode)

  (map! :map override
        "<M-up>" #'scroll-n-lines-up
        "<M-down>" #'scroll-n-lines-down

        :i "C-a" #'+default/newline

        :n "u" #'undo
        :n "C-r" #'redo

        "M-t" #'evil-window-right
        "M-n" #'evil-window-left
        "M-g" #'evil-window-up
        "M-r" #'evil-window-down

        :map evil-window-map
        "N" #'+evil/window-move-left
        "T" #'+evil/window-move-right
        "G" #'+evil/window-move-up
        "R" #'+evil/window-move-down

        :map evil-motion-state-map
        "<up>" #'evil-previous-visual-line
        "<down>" #'evil-next-visual-line
        "<home>" #'evil-beginning-of-visual-line
        "<end>" #'evil-end-of-visual-line
        "é" #'forward-symbol
        "É" #'sp-backward-symbol
        :map evil-inner-text-objects-map
        "é" #'evil-inner-symbol

        :map doom-leader-workspace-map
        "[" :desc "Swap left" #'+workspace/swap-left
        "]" :desc "Swap right" #'+workspace/swap-right
        "(" #'+workspace/switch-left
        ")" #'+workspace/switch-right
        )

  ;; Moving by paragraphs does not add to the jump list
  (evil-define-motion evil-forward-paragraph (count)
    "Move to the end of the COUNT-th next paragraph."
    :type exclusive
    (evil-signal-at-bob-or-eob count)
    (evil-forward-end 'evil-paragraph count)
    (unless (eobp) (forward-line)))

  ;; Insert a single character
  (evil-define-command evil-insert-char (&optional count char)
    "Insert COUNT times character CHAR."
    (interactive "pc")
    (insert (make-string count char))
    )

  ;; Append a single character
  (evil-define-command evil-append-char (&optional count char)
    "Append COUNT times character CHAR."
    (interactive "pc")
    (when (not (eolp))
      (forward-char))
    (insert (make-string count char))
    (backward-char))
  )

;; Increment number at point
(defun increment-number-at-point (&optional count)
  "Increment number at point by COUNT."
  (interactive "p")
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (+ count
                                      (string-to-number (match-string 0)))))
  (left-char))


(map! :map override
      :niv "M-L" #'increment-number-at-point
      :niv "M-A" (lambda () (interactive) (increment-number-at-point -1)))

;; Unmap for some conflicting keymaps
(map! (:map evil-markdown-mode-map
       "M-n" nil))

(use-package! markdown-mode
  :config
  (map! :map markdown-mode-map
        :niv "M-l" nil
        :niv "M-a" nil))


;;; Multiple cursors
(after! evil-mc
  (map! :prefix "gz"
        :nv "j" nil
        :desc "Make, move next line" :nv "<down>" #'evil-mc-make-cursor-move-next-line
        :nv "k" nil
        :desc "Make, move prev line" :nv "<up>" #'evil-mc-make-cursor-move-prev-line)

  (nconc evil-mc-custom-known-commands
         '((forward-symbol . ((:default . evil-mc-execute-default-call-with-count)
                              (visual . evil-mc-execute-visual-call-with-count)))
           (sp-backward-symbol . ((:default . evil-mc-execute-default-call-with-count)
                                  (visual . evil-mc-execute-visual-call-with-count))))))

(map! :map doom-leader-open-map
      "c" #'=calendar)
