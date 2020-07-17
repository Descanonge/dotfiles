;;; .doom.d/config.el --- Config

;; Set first frame to maximised
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq user-full-name "Clément Haëck"
      user-mail-address "clement.haeck@posteo.net"

      avy-keys '(?n ?r ?t ?d ?e ?a ?i ?u)
      display-line-numbers-type 'relative
      )


;;; EVIL
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
    (insert (make-string count char)))

  ;; Append a single character
  (evil-define-command evil-append-char (&optional count char)
    "Append COUNT times character CHAR."
    (interactive "pc")
    (when (not (eolp))
      (forward-char))
    (insert (make-string count char))
    (backward-char))
  )


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
        '("~/Documents/Libraries/Python/Tomate"
          "~/Documents/Libraries/Python/MyPack"
          "~/Documents/Libraries/Web/VisibleEarthHome"
          "~/Documents/Work/Fronts"
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


;;; Org
(map! :map evil-org-mode-map
      :after evil-org
      :niv "M-l" #'org-metaup
      :niv "M-a" #'org-metadown
      :niv "M-i" #'org-metaleft
      :niv "M-e" #'org-metaright
      :niv "M-L" #'org-shiftmetaup
      :niv "M-A" #'org-shiftmetadown
      :niv "M-I" #'org-shiftmetaleft
      :niv "M-E" #'org-shiftmetaright

      :localleader
      :desc "Sparse" "m" #'org-sparse-tree
      )

(use-package! markdown-mode
  :config
  (map! :map markdown-mode-map
        :niv "M-l" nil
        :niv "M-a" nil))

(use-package! org
  :init
  (setq org-cycle-separator-lines 1)
  (setq org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . t)))
  (setq thunderbird-program "/usr/bin/thunderbird")

  :config
  (defun org-capture-project-relative () ""
         (let ((file (car (projectile-make-relative-to-root (list (buffer-file-name)))))
               (text (string-trim (org-current-line-string))))
           (format "* TODO %%?\n[[file:%s::%s]]\n\n" file text)))

  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* %?\n" :prepend t)
          ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
          ;; {todo,notes,changelog}.org file is found in a parent directory.
          ;; Uses the basename from `+org-capture-todo-file',
          ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
          ("p" "Templates for projects")
          ;; ("pt" "Project-local todo" entry  ; {project-root}/todo.org
          ;;  (file+headline +org-capture-project-todo-file "General")
          ;;  "* TODO %?\n%a\n\n" :prepend t)
          ("pt" "Project-local todo" entry  ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "General")
           (function org-capture-project-relative) :prepend t)
          ("pn" "Project-local notes" entry  ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "General")
           "* %U %?\n%a\n\n" :prepend t)
          ("pc" "Project-local changelog" plain  ; {project-root}/changelog.org
           (file +org-capture-project-changelog-file)
           "- [%(format-time-string \"%Y-%m-%d\")] %?\n" :prepend t)
          ;; Will use {org-directory}/{+org-capture-projects-file} and store
          ;; these under {ProjectName}/{Tasks,Notes,Changelog} headings. They
          ;; support `:parents' to specify what headings to put them under, e.g.
          ;; :parents ("Projects")
          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry
           (function +org-capture-central-project-todo-file)
           "* TODO %?\n %i\n %a\n"
           :heading "Tasks"
           :prepend nil)
          ("on" "Project notes" entry
           (function +org-capture-central-project-notes-file)
           "* %U %?\n %i\n %a\n"
           :heading "Notes"
           :prepend t)
          ("oc" "Project changelog" entry
           (function +org-capture-central-project-changelog-file)
           "* %U %?\n %i\n %a\n"
           :heading "Changelog"
           :prepend t))
        +org-capture-changelog-file "changelog.md")

  (defun org-message-thunderlink-open (slash-message-id)
    "Handler for  org-link-set-parameters that converts a standard message://
from SLASH-MESSAGE-ID link into a thunderlink and then invokes thunderbird."
    ;; remove any / at the start of slash-message-id)
    (let ((message-id
           (replace-regexp-in-string (rx bos (* "/"))
                                     ""
                                     slash-message-id)))
      (start-process(concat "thunderlink: " message-id)
                    nil
                    thunderbird-program
                    "-thunderlink"
                    (concat "thunderlink://messageid=" message-id)
                    )))
  (org-link-set-parameters "message" :follow #'org-message-thunderlink-open)
  )

(use-package! org-agenda
  :init
  (map! :map doom-leader-map
        :leader
        :desc "Agenda" "A" (lambda () (interactive) (org-agenda nil "n")))

  :config
  (org-add-agenda-custom-command
        '("n" "Agenda and all TODOs"
          ((agenda "" ((org-agenda-show-all-dates nil)
                       (org-agenda-start-day "today")
                       (org-agenda-span 7)))
           (todo "TODO" ((org-agenda-todo-ignore-scheduled "future")))
           )))
  )

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
                                                         emacs-lisp-checkdoc)))


;;; Direnv
(use-package! direnv
  :init
  (setq direnv-always-show-summary nil)
  )

(use-package! rst
  :config
  (map! :map rst-mode-map
        "]g" #'rst-forward-section
        "[g" #'rst-backward-section))

;;; Python
(use-package! python
  :init
  (setq! python-shell-interpreter "ipython"
         python-shell-interpreter-args "console --simple-prompt"
         python-shell-prompt-detect-failure-warning nil))

;; Python-cells
(use-package! python-cell
  :init
  ;; Make python cell mode default
  (add-hook! 'python-mode-hook #'python-cell-mode)

  :config
  (defun python-cell-previous-cell ()
    "Move to beginning of cell or previous cell")

  ;; Move cells
  (map! :map python-cell-mode-map
        :nv "]g" #'python-cell-forward-cell
        :nv "[g" #'python-cell-backward-cell)

  ;; Add ipython sections to imenu
  (add-hook 'python-mode-hook
            (lambda ()
              (add-to-list 'imenu-generic-expression
               (list "Sections" python-cell-cellbreak-regexp 1))
              (imenu-add-to-menubar "Position")
              (setq imenu-create-index-function 'python-merge-imenu)))
  (defun python-merge-imenu ()
    (interactive)
    (let ((mode-imenu (python-imenu-create-index))
          (custom-imenu (imenu--generic-function imenu-generic-expression)))
      (append mode-imenu custom-imenu)))

  ;; Set cell mode highlight
  (defun python-cell-range-function ()
    "Function to call to return highlight range.
  Highlight only the cell title. Return nil if the title
  is off screen."
    (save-match-data
      (save-excursion
        (progn (end-of-line)
               (if (re-search-backward python-cell-cellbreak-regexp nil t)
                   (let ((start (goto-char (match-beginning 0)))
                         (end (goto-char (match-end 0))))
                     `(,start . ,end))
                 nil))))
    )
  )


(use-package! zeal-at-point
  :init
  (add-hook 'python-mode-hook
            (lambda () (setq zeal-at-point-docset '("python" "numpy" "matplotlib" "scipy"))))
  (map! :map doom-leader-code-map
        "z" :desc "Zeal at point" #'zeal-at-point)
  :config
  (defun zeal-search (search)
    "Search SEARCH in Zeal."
    (interactive (list (completing-read "Zeal: " zeal-at-point-docsets)))
    (zeal-at-point-run-search (concat "dash-plugin://query=%s" search))
    )
  (map! :map doom-leader-search-map
   "z" :desc "Zeal" #'zeal-search)
  )

(use-package! counsel-dash
  :init
  (setq dash-docs-docsets-path "/home/clement/.local/share/Zeal/Zeal/docsets"))

(use-package! anaconda-mode
  :init
  (defun inhibit-anaconda-remote ()
    (when (file-remote-p (buffer-file-name))
      (anaconda-mode -1)
      (anaconda-eldoc-mode -1)))
  (add-hook! 'find-file-hook #'inhibit-anaconda-remote)
  )


;; Jupyter
(use-package! jupyter
  :config
  (defun jupyter-connect-name (filename)
    "Connect to a jupyter kernel by its FILENAME."
    (interactive (list (ivy-read "Connection file name: "
                                 (mapcar #'car
                                         (reverse (cl-sort
                                                   (seq-subseq (directory-files-and-attributes "~/.local/share/jupyter/runtime/") 2)
                                                   #'time-less-p :key #'(lambda (x) (nth 6 x))))))))
    (setq client (jupyter-connect-repl (concat "~/.local/share/jupyter/runtime/" filename) filename))
    (jupyter-repl-associate-buffer client))

  ;; Send cell to jupyter
  (defun jupyter-eval-cell ()
    "Eval current IPython cell."
    (interactive)
    (let (
          (start (save-excursion (python-cell-beginning-of-cell)
                                 (point)))
          (end (save-excursion (python-cell-end-of-cell)
                               (point))))
      (jupyter-eval-region start end)))

  ;; Jupyter kb
  (map! :map jupyter-repl-interaction-mode-map "M-i" nil)
  (map! :leader
        (:prefix ("r" . "run")
         :desc "Connect to kernel" "k" #'jupyter-connect-name
         :desc "Send line or region" "l" #'jupyter-eval-line-or-region
         :desc "Send string" "s" #'jupyter-eval-string-command
         :desc "Send cell" "c" #'jupyter-eval-cell
         :desc "Send buffer" "b" #'jupyter-eval-buffer
         :desc "Interrupt kernel" "i" #'jupyter-interrupt-kernel
         )))

(defface font-lock-ds-arguments-face
  '((t :inherit font-lock-doc-face
       :slant normal)) "Face for docstring arguments.")

(font-lock-add-keywords 'python-mode
                        '(("[ ^:]*:param \\([a-zA-Z0-9_^:]*\\):" 1 "font-lock-ds-arguments-face" t)))


;;; TRAMP
;; Add path for git on @ciclad
(setenv "SHELL" "/bin/bash")
(add-to-list 'tramp-remote-path "/opt/git/2.7.4/bin")
(add-to-list 'tramp-methods
             '("yadm"
               (tramp-login-program "yadm")
               (tramp-login-args (("enter")))
               (tramp-login-env (("SHELL") ("/bin/sh")))
               (tramp-remote-shell "/bin/sh")
               (tramp-remote-shell-args ("-c"))))
(map! :leader "g." (cmd! (magit-status  "/yadm::")))

(use-package! liquid-mode)

(load! "+dashboard.el")

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
