;;; +bindings.el -*- lexical-binding: t; -*-


(map! :after evil
      ;; Override always
      (:map (local-intercept override)
      :desc "Scroll up" "<M-up>" #'me/scroll-n-lines-up
      :desc "Scroll down" "<M-down>" #'me/scroll-n-lines-down

      "M-t" #'evil-window-right
      "M-n" #'evil-window-left
      "M-r" #'evil-window-down
      "M-g" #'evil-window-up

      "M-l" #'drag-stuff-up
      "M-a" #'drag-stuff-down)

      ;; Rebinds for dealing with hjkl keys on my Neo layout
      :m "j" #'evil-ex-search-next
      :m "," #'evil-ex-search-previous
      :nm ";" #'evil-snipe-repeat
      :nm "\"" #'evil-snipe-repeat-reverse

      :nvo "l" #'evil-replace
      :no "L" #'evil-enter-replace-state
      :n "k" #'evil-insert-char
      :n "K" #'evil-append-char

      :nm "n" #'evil-previous-line
      :nm "r" #'evil-next-line
      ;; --

      :desc "Increment number at point" :niv "M-L" #'increment-number-at-point
      :desc "Decrement number at point" :niv "M-L" #'increment-number-at-point

      :map global
      :m "\\" nil

      :m "<up>" #'evil-previous-visual-line
      :m "<down>" #'evil-next-visual-line
      :m "<home>" #'evil-beginning-of-visual-line
      :m "<end>" #'evil-end-of-visual-line

      :m "é" #'evil-forward-symbol-begin
      :m "É" #'evil-backward-symbol-begin
      :m "è" #'evil-forward-symbol-end
      :m "È" #'evil-backward-symbol-end

      ;; :desc "Undo" :n "u" #'undo
      ;; :desc "Redo" :n "C-r" #'redo

      :n "gC" #'evilnc-copy-and-comment-operator

      :map in
      "é" #'evil-inner-symbol

      :map doom-leader-workspace-map
      :desc "Swap left" "[" #'+workspace/swap-left
      :desc "Swap right" "]" #'+workspace/swap-right
      :desc "Switch left" "(" #'+workspace/switch-left
      :desc "Switch right" ")" #'+workspace/switch-right

      :map doom-leader-file-map
      :desc "Find file in new window" "." #'find-file-other-window
      )


(map! :after evil-easymotion
      :map evilem-map
      "<up>" #'evilem-motion-previous-visual-line
      "n" #'evilem-motion-previous-line
      "<down>" #'evilem-motion-next-visual-line
      "r" #'evilem-motion-next-line)

;; Unclutter window bindings
(map! :map evil-window-map
      ;; All of those are redundant
      "C-<down>" nil "C-<up>" nil
      "C-<left>" nil "C-<right>" nil
      "C-h" nil "C-j" nil "C-k" nil "C-l" nil
      "h" nil "j" nil "k" nil "l" nil

      ;; Redundant with simpler keys
      "C-S-h" nil "C-S-j" nil "C-S-k" nil "C-S-l" nil
      "C-S-r" nil "C-S-s" nil "C-S-w" nil "C-_" nil
      "C-x" nil "C-s" nil "C-t" nil "C-v" nil "C-n" nil

      ;; No use
      "q" nil "C-q" nil

      ;; Right hand keys for moving
      "N" #'+evil/window-move-left
      "T" #'+evil/window-move-right
      "G" #'+evil/window-move-up
      "R" #'+evil/window-move-down
      ;; Correct stuff that is shadowed
      "C-r" #'evil-window-rotate-upwards
      )

(map! :map doom-leader-open-map
      "c" #'=calendar

      :map doom-leader-toggle-map
      :desc "Autofill" "a" #'auto-fill-mode
      :desc "Center mode" "C" #'centered-window-mode
      )


;; Run stuff, by default Elisp code
(map! :localleader
      :map emacs-lisp-mode-map
      (:prefix ("r" . "run")
       :desc "Run line/region" :n "l" #'+eval/line-or-region
       :desc "Run" :n "r" #'+eval:region
       :desc "Run buffer" :n "b" #'eval-buffer
       :desc "Run string" :n "s" #'pp-eval-expression))
