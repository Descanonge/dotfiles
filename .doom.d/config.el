;;; .doom.d/config.el --- Config

;;; Frames
;; Set first frame to maximised
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(add-hook 'before-make-frame-hook
          #'(lambda ()
              (add-to-list 'default-frame-alist '(left   . 0))
              (add-to-list 'default-frame-alist '(top    . 0))
              (add-to-list 'default-frame-alist '(height . 30))
              (add-to-list 'default-frame-alist '(width  . 90))))

;;; General info
(setq user-full-name "Clément Haëck"
      user-mail-address "clement.haeck@posteo.net"

      avy-keys '(?n ?r ?t ?d ?e ?a ?i ?u)
      display-line-numbers-type 'relative
      )

(map! :map doom-leader-open-map
      "c" #'calendar)

;;; EVIL
(setq evil-respect-visual-line-mode t)
(after! evil
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
        :niv "<up>" #'evil-previous-visual-line
        :niv "<down>" #'evil-next-visual-line

        :map doom-leader-toggle-map
        :desc "Centered window" "c" #'centered-window-mode-toggle
        :map doom-leader-toggle-map
        :desc "Visual line mode" "v" #'visual-line-mode)

  (map! :map override
        "<M-up>" #'scroll-n-lines-up
        "<M-down>" #'scroll-n-lines-down

        :i "C-a" #'+default/newline

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
        "é" #'forward-symbol
        "É" #'sp-backward-symbol
        :map evil-inner-text-objects-map
        "é" #'evil-inner-symbol

        :map doom-leader-workspace-map
        "[" :desc "Swap left" #'+workspace/swap-left
        "]" :desc "Swap right" #'+workspace/swap-right
        "(" #'+workspace/switch-left
        ")" #'+workspace/switch-right

        :map doom-leader-toggle-map
        "V" #'visual-fill-column-mode
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


(after! ivy
 (setq ivy-extra-directories nil))

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

;;; Preferred viewers
(use-package! mailcap
  :config
  (add-to-list 'mailcap-user-mime-data
        '((viewer . "evince %s")
          (type . "application/pdf")))
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

;;; Info-mode
;; Dedicated window
(set-popup-rules!
  '(("^\\*info" :slot 2 :side bottom :heigth 110 :quit nil)))

;;; Projectile
(use-package! projectile
  :init
  (setq projectile-track-known-projects-automatically nil)
  (setq projectile-globally-ignored-file-suffixes
        '(".swp" ".png" ".jpg" ".avi" ".mp4" ".svg" ".mkv" ".xcf"
               ".pdf" ".dvi"
               ".pyc" ".pyo" ".pyd"
               ".o" ".so" ".a" ".exe" ".o.d")
        projectile-globally-ignored-files
        '()
        projectile-globally-ignored-directories
        '("__pycache__" "*.egg-info" ".git"
          ".jekyll-cache" "_build")
        projectile-sort-order 'default
        projectile-projects
        '("~/.scripts"
          "~/Documents/Libraries/Python/Tomate"
          "~/Documents/Libraries/Python/MyPack"
          "~/Documents/Libraries/Python/Xarray-regex"
          "~/Documents/Libraries/Web/VisibleEarthHome"
          "~/Documents/Work/Fronts"
          "~/Documents/Websites/pinako"
          "~/Documents/Applications/dateloop"
          "~/Documents/Applications/CarGame"
          "~/Documents/Applications/notmuch-notifier@cinnamon.org"
          "~/Documents/Applications/JS/Bot-ulus"
          "~/Documents/Work/Enseignement/3P002_Phy_Num/TPs/TP2"
          "/sshx:ciclad:/home/chaeck/Fronts"))

  :config
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-known-projects projectile-projects)
  (setq projectile-generic-command
        (lambda (_)
          (let (bin)
            (cond
             ((setq bin (cl-find-if (doom-rpartial #'executable-find t)
                                    '("fdfind" "fd")))
              (concat (format "%s . -0 -H -E .git --color=never --type file --type symlink --follow"
                              bin)
                      (cl-loop for dir in projectile-globally-ignored-directories
                               concat " -E "
                               concat (shell-quote-argument dir))))
             ((concat "find . -type f"
                      (cl-loop for dir in projectile-globally-ignored-directories
                               concat " -not -path "
                               concat (shell-quote-argument (format "*/%s/*" dir)))
                      " -printf '%P\\0'"))))))

  (defun projectile-project-name-function-remote (project-root)
    (let* ((dir (directory-file-name project-root))
           (name (file-name-nondirectory dir))
           (remote-p (file-remote-p dir 'host))
           (remote (if remote-p (format "@%s" remote-p))))
      (concat name remote)
      ))

  (setq projectile-project-name-function #'projectile-project-name-function-remote)
  )

;;; Magit
;; Scroll in magit buffers
(after! magit
  ;; Find git rather than prescribe its location. Useful for tramp
  (setq magit-git-executable "git")
  (map! (:map magit-mode-map
          :prefix "z"
          :nv "t" #'evil-scroll-line-to-top)

        (:map magit-mode-map
          "M-n" nil)

        :leader
        :desc "Diff" "gd" #'magit-diff))

(map! (:map evil-markdown-mode-map
       "M-n" nil))


(use-package! markdown-mode
  :config
  (map! :map markdown-mode-map
        :niv "M-l" nil
        :niv "M-a" nil))


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

(use-package! parrot
  :config
  (map! :map evil-normal-state-map
        "!" #'parrot-rotate-next-word-at-point)
  (parrot-mode)
  (setq parrot-directory (concat doom-private-dir "parrot/"))
  (parrot-set-parrot-type 'default)
  (setq parrot-rotate-highlight-after-rotation nil
        parrot-animation-frame-interval 0.030)

  (defun parrot-start-animation-advice (old-function &rest arguments)
    (parrot-start-animation))
  (advice-add 'evil-ex-substitute :after #'parrot-start-animation-advice))

;;; Flycheck
(use-package! flycheck
  :init
  ;; Set flycheck to check at save
  (setq! flycheck-check-syntax-automatically '(mode-enabled save))
  ;; Set flake8 config file
  (setq flycheck-flake8rc "~/.config/flake8")
  ;; Set mypy config file
  (setq! flycheck-python-mypy-ini "~/.config/mypy/config")
  (setq-default flycheck-disabled-checkers '(python-mypy python-pycompile python-pylint
                                                         emacs-lisp-checkdoc))

  :config
  (setq flycheck-gfortran-language-standard "f2008")
  (add-to-list 'flycheck-gfortran-include-path "/usr/include"))

;;; Direnv
(after! direnv
  (setq direnv-always-show-summary nil)
  )

;;; RST
(use-package! rst
  :config
  (map! :map rst-mode-map
        "]g" #'rst-forward-section
        "[g" #'rst-backward-section))

(use-package! liquid-mode)

(load! "+mail.el")
(load! "+org.el")
(load! "+python.el")
(load! "+dashboard.el")


;;; TRAMP
;; Add path for git on @ciclad

(after! tramp
  ;; (setq tramp-sh-extra-args '(("/bash\\'" . "-norc")))
  (setenv "SHELL" "/bin/bash")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; (add-to-list 'tramp-remote-path "/opt/git/2.7.4/bin")
  (add-to-list 'tramp-methods
               '("yadm"
                 (tramp-login-program "yadm")
                 (tramp-login-args (("enter")))
                 (tramp-login-env (("SHELL") ("/bin/sh")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-c"))))
  (map! :leader "g." (cmd! (magit-status  "/yadm::")))

  (map! :leader
        (:prefix ("g," . "set exec")
         :desc "Local" "l" #'(lambda () (interactive) (setq magit-git-executable "git"))
         :desc "Ciclad" "c" #'(lambda () (interactive) (setq magit-git-executable "/opt/git/2.7.4/bin/git"))
         )))

;;; Server
(server-start)
