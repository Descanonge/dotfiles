;;; +evil.el -*- lexical-binding: t; -*-


(use-package! avy
  :config
  (setq avy-keys '(?n ?r ?t ?d ?e ?a ?i ?u ?o ?s ?v ?l ?c ?h ?g ?f ?m ?b))
  (setq avy-style 'at-full)
  (setq avy-background nil)
  )

(use-package! evil
  :init
  (setq evil-respect-visual-line-mode nil)
  :config
  (setq evil-kill-on-visual-paste nil
        evil-in-single-undo t)

  ;; Scrolling
  (defcustom me/scroll-step 5
    "Scroll step in lines."
    :group 'me
    :type 'integer)
  (evil-define-motion me/scroll-n-lines-up (count)
    "Scroll `scroll-step' up"
    :jump nil
    (evil-scroll-line-up me/scroll-step))
  (evil-define-motion me/scroll-n-lines-down (count)
    "Scroll `scroll-step' down"
    :jump nil
    (evil-scroll-line-down me/scroll-step))

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

  ;; Define motions for symbol
  (evil-define-motion evil-forward-symbol-begin (count)
    "Move the cursor to the beginning of the COUNT-th next symbol.

If this command is called in operator-pending state it behaves
differently. If point reaches the beginning of a symbol on a new
line point is moved back to the end of the previous line.

If called after a change operator, i.e. cé or cÉ,
`evil-want-change-word-to-end' is non-nil and point is on a word,
then both behave like cè or cÈ.

If point is at the end of the buffer and cannot be moved signal
'end-of-buffer is raised.
"
    :type exclusive
    (let ((thing 'evil-symbol)
          (orig (point))
          (count (or count 1)))
      (evil-signal-at-bob-or-eob count)
      (cond
       ;; default motion, beginning of next word
       ((not (evil-operator-state-p))
        (evil-forward-beginning thing count))
       ;; the evil-change operator, maybe behave like ce or cE
       ((and evil-want-change-word-to-end
             (memq evil-this-operator evil-change-commands)
             (< orig (or (cdr-safe (bounds-of-thing-at-point thing)) orig)))
        ;; forward-thing moves point to the correct position because
        ;; this is an exclusive motion
        (forward-thing thing count))
       ;; operator state
       (t
        (prog1 (evil-forward-beginning thing count)
          ;; if we reached the beginning of a word on a new line in
          ;; Operator-Pending state, go back to the end of the previous
          ;; line
          (when (and (> (line-beginning-position) orig)
                     (looking-back "^[[:space:]]*" (line-beginning-position)))
            ;; move cursor back as long as the line contains only
            ;; whitespaces and is non-empty
            (evil-move-end-of-line 0)
            ;; skip non-empty lines containing only spaces
            (while (and (looking-back "^[[:space:]]+$" (line-beginning-position))
                        (not (<= (line-beginning-position) orig)))
              (evil-move-end-of-line 0))
            ;; but if the previous line is empty, delete this line
            (when (bolp) (forward-char))))))))

  (evil-define-motion evil-forward-symbol-end (count)
    "Move the cursor to the end of the COUNT-th next symbol."
    :type inclusive
    (let ((thing 'evil-symbol)
          (count (or count 1)))
      (evil-signal-at-bob-or-eob count)
      ;; Evil special behaviour: e or E on a one-character word in
      ;; operator state does not move point
      (unless (and (evil-operator-state-p)
                   (= 1 count)
                   (let ((bnd (bounds-of-thing-at-point thing)))
                     (and bnd
                          (= (car bnd) (point))
                          (= (cdr bnd) (1+ (point)))))
                   (looking-at "[[:word:]]"))
        (evil-forward-end thing count))))

  (evil-define-motion evil-backward-symbol-begin (count)
    "Move the cursor to the beginning of the COUNT-th previous symbol."
    :type exclusive
    (evil-signal-at-bob-or-eob (- (or count 1)))
    (evil-backward-beginning 'evil-symbol count))

  (evil-define-motion evil-backward-symbol-end (count)
    "Move the cursor to the end of the COUNT-th previous symbol."
    :type inclusive
    (evil-signal-at-bob-or-eob (- (or count 1)))
    (evil-backward-end 'evil-symbol count))
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


;; Redefine evil-easymotions to use 'line scope instead where needed
;; (contrary to 'visible as defined in modules/editor/evil/config.el)
;; Add symbol to motions
(use-package! evil-easymotion
  :after-call doom-first-input-hook
  :commands evilem-create evilem-default-keybindings
  :config
  ;; Rebind scope of w/W/e/E/ge/gE evil-easymotion motions to the visible
  ;; buffer, rather than just the current line.
  (evilem-make-motion evilem-motion-forward-word-begin #'evil-forward-word-begin :scope 'line)
  (evilem-make-motion evilem-motion-forward-WORD-begin #'evil-forward-WORD-begin :scope 'line)
  (evilem-make-motion evilem-motion-forward-symbol-begin #'evil-forward-symbol-begin :scope 'line)
  (evilem-make-motion evilem-motion-forward-word-end #'evil-forward-word-end :scope 'line)
  (evilem-make-motion evilem-motion-forward-WORD-end #'evil-forward-WORD-end :scope 'line)
  (evilem-make-motion evilem-motion-forward-symbol-end #'evil-forward-symbol-end :scope 'line)
  (evilem-make-motion evilem-motion-backward-word-begin #'evil-backward-word-begin :scope 'line)
  (evilem-make-motion evilem-motion-backward-WORD-begin #'evil-backward-WORD-begin :scope 'line)
  (evilem-make-motion evilem-motion-backward-symbol-begin #'evil-backward-symbol-begin :scope 'line)
  (evilem-make-motion evilem-motion-backward-word-end #'evil-backward-word-end :scope 'line)
  (evilem-make-motion evilem-motion-backward-WORD-end #'evil-backward-WORD-end :scope 'line)
  (evilem-make-motion evilem-motion-backward-symbol-end #'evil-backward-symbol-end :scope 'line)

  (map! :map evilem-map
        "é" #'evilem-motion-forward-symbol-begin
        "É" #'evilem-motion-backward-symbol-begin
        "è" #'evilem-motion-forward-symbol-end
        "È" #'evilem-motion-backward-symbol-end)

)


;;; Multiple cursors
(use-package! evil-mc
  :defer t
  :config
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

(use-package! lispy
    :disabled)

(use-package! lispyville
  :defer t
  :init
  (setq lispyville-key-theme
        '((operators normal)
          c-w
          (prettify insert)
          (atom-movement t)
          slurp/barf-lispy
          additional-insert))
  :config
  (lispyville--define-key 'normal
    (kbd "M-k") #'lispyville-drag-backward
    (kbd "M-k") #'lispyville-drag-backward)
  )
