;;; .doom.d/config.el --- Config

;; Set first frame to maximised
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(add-hook 'before-make-frame-hook
          #'(lambda ()
              (add-to-list 'default-frame-alist '(left   . 0))
              (add-to-list 'default-frame-alist '(top    . 0))
              (add-to-list 'default-frame-alist '(height . 30))
              (add-to-list 'default-frame-alist '(width  . 90))))

(setq user-full-name "Clément Haëck"
      user-mail-address "clement.haeck@posteo.net"

      avy-keys '(?n ?r ?t ?d ?e ?a ?i ?u)
      display-line-numbers-type 'relative
      )

(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; org-agenda source
    ;; (cfw:cal-create-source "Gray") ; diary source
    (cfw:ical-create-source "Events" "~/events.ics" "Orange")  ; ICS source1
    )))

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

;;; Major modes
(add-to-list 'auto-mode-alist '("\\.eml\\'" . mail-mode))


(setq evil-respect-visual-line-mode t)
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
        '("~/.scripts"
          "~/Documents/Libraries/Python/Tomate"
          "~/Documents/Libraries/Python/MyPack"
          "~/Documents/Libraries/Web/VisibleEarthHome"
          "~/Documents/Work/Fronts"
          "~/Documents/Websites/pinako"
          "~/Documents/Applications/dateloop"
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

;;; Org
(map! :map evil-org-mode-map
      :after evil-org
      :niv "S-<up>" #'org-previous-visible-heading
      :niv "S-<down>" #'org-next-visible-heading
      :niv "S-<left>" #'org-backward-heading-same-or-up-level
      :niv "S-<right>" #'org-forward-heading-same-or-up-level
      :niv "S-M-<up>" #'org-metaup
      :niv "S-M-<down>" #'org-metadown
      :niv "S-M-<left>" #'org-shiftmetaleft
      :niv "S-M-<right>" #'org-shiftmetaright
      :niv "M-L" #'org-shiftup
      :niv "M-A" #'org-shiftdown
      :niv "M-I" #'org-shiftleft
      :niv "M-E" #'org-shiftright
      :niv "M-l" #'drag-stuff-up

      :localleader
      :desc "Sparse" "m" #'org-sparse-tree
      )

(use-package! markdown-mode
  :config
  (map! :map markdown-mode-map
        :niv "M-l" nil
        :niv "M-a" nil))

(setq org-roam-directory "~/Nextcloud/org/notes"
      org-directory "~/org/todos")

(use-package! org
  :init
  (setq org-cycle-separator-lines 1
        org-blank-before-new-entry nil
        thunderbird-program "/usr/bin/thunderbird"
        org-startup-folded 'content)

  :config
  (set-face-attribute 'org-drawer nil :height 0.9 :weight 'semi-light :foreground "grey")
  (org-link-set-parameters "zotero" :follow
                           (lambda (zpath)
                             (browse-url (format "zotero:%s" zpath))))

  (defun org-forward-heading-same-or-up-level (arg &optional invisible-ok)
    "Move forward to the ARG'th subheading at same level as this one. Stop at
        the first and last subheadings of a superior heading. Normally this only
        looks at visible headings, but when INVISIBLE-OK is non-nil it will also
        look at invisible ones."
    (interactive "p")
    (let ((backward? (and arg (< arg 0))))
      (if (org-before-first-heading-p)
          (if backward? (goto-char (point-min)) (outline-next-heading))
        (org-back-to-heading invisible-ok)
        (unless backward? (end-of-line))	;do not match current headline
        (let ((level (- (match-end 0) (match-beginning 0) 1))
              (f (if backward? #'re-search-backward #'re-search-forward))
              (count (if arg (abs arg) 1))
              (result (point)))
          (while (and (> count 0)
                      (funcall f org-outline-regexp-bol nil 'move))
            (let ((l (- (match-end 0) (match-beginning 0) 1)))
              (cond ((and (<= l level)
                          (or invisible-ok
                              (not (org-invisible-p
                                    (line-beginning-position)))))
                     (cl-decf count)
                     (setq result (point))))))
          (goto-char result))
        (beginning-of-line))))

  (defun org-backward-heading-same-or-up-level (arg &optional invisible-ok)
    "Move backward to the ARG'th subheading at same level as this one. Stop at
     the first and last subheadings of a superior heading."
    (interactive "p")
    (org-forward-heading-same-or-up-level (if arg (- arg) -1) invisible-ok))

  ;; (defun org-capture-project-relative () ""
  ;;        (let ((file (car (projectile-make-relative-to-root (list (buffer-file-name)))))
  ;;              (text (string-trim (org-current-line-string))))
  ;;          (format "* TODO %%?\n[[file:%s::%s]]\n\n" file text)))

  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* %?\n%i\n%a" :prepend t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t)
          ("j" "Journal" entry
           (file+olp+datetree +org-capture-journal-file)
           "* %U %?\n%i\n%a" :prepend t)

          ("w" "Capture protocol" entry
           (file+headline +org-capture-notes-file "Web")
           "* [[%:link][%:description]]\n%:initial"
           :immediate-finish t :prepend t)

          ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
          ;; {todo,notes,changelog}.org file is found in a parent directory.
          ;; Uses the basename from `+org-capture-todo-file',
          ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry  ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("pn" "Project-local notes" entry  ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)
          ("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
           (file+headline +org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i\n%a" :prepend t)

          ;; Will use {org-directory}/{+org-capture-projects-file} and store
          ;; these under {ProjectName}/{Tasks,Notes,Changelog} headings. They
          ;; support `:parents' to specify what headings to put them under, e.g.
          ;; :parents ("Projects")
          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry
           (function +org-capture-central-project-todo-file)
           "* TODO %?\n %i\n %a"
           :heading "Tasks"
           :prepend nil)
          ("on" "Project notes" entry
           (function +org-capture-central-project-notes-file)
           "* %U %?\n %i\n %a"
           :heading "Notes"
           :prepend t)
          ("oc" "Project changelog" entry
           (function +org-capture-central-project-changelog-file)
           "* %U %?\n %i\n %a"
           :heading "Changelog"
           :prepend t)))

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
  (setq org-agenda-files (apply 'append
                                (mapcar
                                 (lambda (directory)
                                   (directory-files-recursively
                                    directory org-agenda-file-regexp))
                                   '("~/org/todos"))))
  (setq calendar-week-start-day 1)
  (org-add-agenda-custom-command
        '("n" "Agenda and all TODOs"
          ((agenda "" ((org-agenda-show-all-dates nil)
                       (org-agenda-start-day "today")
                       (org-agenda-span 7)
                       (org-agenda-skip-scheduled-if-deadline-is-shown t)
                       (org-agenda-scheduled-leaders '("S." "S.%2dx"))))
           (todo "TODO" ((org-agenda-todo-ignore-scheduled 'future)))
           )))
  )

;;; JOURNAL
(use-package! org-journal
  :config
  (setq org-journal-file-type 'daily
        org-journal-file-format "%Y-%m-%d"
        org-journal-time-format ""
        org-journal-dir "~/org/journal/lab-notebook")
  (map! :map doom-leader-notes-map
        (:prefix-map ("j" . "journal")
        :desc "Diary new entry"        "d" #'journal-new-entry
        :desc "Searc lab-notebook"     "s" #'org-journal-search
        :desc "Lab-notebook new entry" "l" #'org-journal-new-entry
        :desc "Lab-notebook latest entry" "L" #'(lambda () (interactive)
                                                  (org-journal-new-entry 1))))

  (setq journal-diary-dir "~/org/journal/diary"
        journal-diary-date-format "%A, %x"
        journal-file-type 'weekly)

  (defun journal-get-today-file ()
    "Get today file."
    (let* ((org-journal-file-type journal-file-type)
           (file (file-truename
                  (expand-file-name
                   (format-time-string "%Y-%m-%d"
                                       (org-journal--convert-time-to-file-type-time nil))
                   journal-diary-dir))))
      file))

  (defun journal-new-entry ()
    "Open today entry. If it does not exist, create it."
    (interactive)
    (let ((oetu-active-p)
          (time (current-time))) ;; org-extend-today-until-active-p
      (let ((now (decode-time nil)))
        (if (and (< (nth 2 now)
                    org-extend-today-until))
            (setq oetu-active-p t
                  time (encode-time (nth 0 now)      ; second
                                    (nth 1 now)      ; minute
                                    (nth 2 now)      ; hour
                                    (1- (nth 3 now)) ; day
                                    (nth 4 now)      ; month
                                    (nth 5 now)      ; year
                                    (nth 8 now)))))  ; timezone

      (let* ((entry-path (journal-get-today-file))
             match)
        (unless (string= entry-path (buffer-file-name))
          (find-file entry-path))
        (view-mode -1)
        (org-mode)
        (let ((entry-header (concat "* " (format-time-string journal-diary-date-format time))))
          (goto-char (point-min))
          (unless (search-forward entry-header nil t)
            (goto-char (point-max))
            (forward-line)
            (when (looking-back "[^\t ]" (point-at-bol))
              (insert "\n"))
            (beginning-of-line)
            (insert entry-header)))

          (outline-end-of-subtree)
          (outline-hide-other)
          (goto-char (point-max))
          (unless (eq (current-column) 0) (insert "\n"))
          )))
  )


(use-package! bibtex-completion
  :config
  (setq bibtex-completion-bibliography '("~/zotero/library.bib")
        bibtex-completion-notes-path "~/Nextcloud/org/notes/articles"
        bibtex-completion-pdf-field "File"
        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-notes-template-multiple-files
        (concat
         "${=key=}: ${title}\n"
         "#+ROAM_KEY: cite:${=key=}\n"
         "#+ROAM_TAGS: article\n\n"
         "- keywords :: ${keywords}\n\n"
         ":PROPERTIES:\n"
         ":Custom_ID: ${=key=}\n"
         ":AUTHOR: ${author-abbrev}\n"
         ":JOURNAL: ${journaltitle}\n"
         ":DATE: ${date}\n"
         ":YEAR: ${year}\n"
         ":DOI: ${doi}\n"
         ":URL: ${url}\n"
         ":END:\n\n"
         )))

(use-package! org-ref
    :config
    (org-ref-ivy-cite-completion)
    (setq org-ref-completion-library 'org-ref-ivy-cite
          org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
          org-ref-default-bibliography '("~/zotero/library.bib")
          org-ref-bibliography-notes "~/Nextcloud/org/notes/bibnotes.org"
          org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
          org-ref-notes-directory "~/Nextcloud/org/notes/"
          org-ref-notes-function 'orb-edit-notes
          ))

(use-package! deft
  :init
  (setq deft-directory "~/org/notes"
        deft-use-filename-as-title t
        deft-auto-save-interval 0
        deft-recursive t))

(use-package! org-roam
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain #'org-roam-capture--get-point "%?"
          :file-name "${slug}" :head "#+title: ${title}\n" :unarrowed t))
        ))

(use-package org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-note-actions-frontend 'ivy)
  (setq org-roam-bibtex-preformat-keywords
        '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "articles/${slug}"
           :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}\n#+ROAM_TAGS: article

- tags ::
- keywords :: ${keywords}

\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"

           :unnarrowed t))))

(use-package! company-bibtex
  :config
  (add-to-list 'company-backends 'company-bibtex)
  (setq company-bibtex-bibliography '("~/zotero/library.bib")))

;; (use-package! zotxt
;;   :config
;;   (add-hook! 'org-mode-hook '(lambda () (org-zotxt-mode t)))
;;   (setq zotxt-default-bibliography-style "mkbehr-short")
;;   (map! :map org-zotxt-mode-map
;;         :localleader
;;         :prefix ("z" . "Zotxt")
;;         :desc "Insert link" :niv "i" #'org-zotxt-insert-reference-link
;;         :desc "Update links" :niv "r" #'org-zotxt-update-all-reference-links
;;         :desc "Open atch" :niv "a" #'org-zotxt-open-attachment))

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
(after! direnv
  (setq direnv-always-show-summary nil)
  )


;;; RST
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

(use-package! liquid-mode)

(load! "+dashboard.el")
(load! "+mail.el")


;;; TRAMP
;; Add path for git on @ciclad
(after! tramp
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
  )

(server-start)
