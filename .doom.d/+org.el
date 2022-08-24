;;; +org.el -*- lexical-binding: t; -*-

;;; General
(setq org-roam-directory "~/Nextcloud/org/notes"
      org-directory "~/org/todos")

;;; Org main
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
      :niv "C-S-l" #'org-shiftright
      :niv "C-S-a" #'org-shiftleft

      :localleader
      :desc "Sparse" "m" #'org-sparse-tree
      :desc "Add timed heading" "dn" #'org-insert-timed-heading
      )

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


  (defun org-insert-timed-heading () (interactive)
    "Insert a heading with today's date."
    (org-insert-subheading 1)
    (org-insert-time-stamp (current-time) nil t)
    )

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

          ("e" "Templates for experiments")
          ("c" "Current experiments" entry
           (file +org-capture-experiment-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)

          ("n" "New experiment" entry
           (file +org-capture-journal-file)
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

(after! ol
  ;; Link a commit to magit
  (defun org-magit-open-commit (path)
    (message path)
    (magit-log-setup-buffer (list path) nil nil))

  (defun org-magit-store-commit ()
    (when (derived-mode-p 'magit-mode)
      (let ((link (or (magit-commit-at-point)
                      (magit-rev-parse '("--short" "HEAD")))))
        (message link)
        (org-link-store-props
         :type "commit"
         :link (concat "commit:" link)))))

  (org-link-set-parameters "commit"
                           :follow #'org-magit-open-commit
                           :store #'org-magit-store-commit)

  )

;;; Agenda
(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; org-agenda source
    ;; (cfw:cal-create-source "Gray") ; diary source
    (cfw:ical-create-source "Events" "~/events.ics" "Orange")  ; ICS source1
    )))

(use-package! org-agenda
  :init
  (map! :map doom-leader-map
        :leader
        :desc "Agenda" "A" (lambda () (interactive) (org-agenda nil "n")))

  :config
  (defun org-agenda-refresh-files ()
    (interactive)
    (setq org-agenda-files (apply 'append
                                  (mapcar
                                   (lambda (directory)
                                     (directory-files-recursively
                                      directory org-agenda-file-regexp nil nil t))
                                   '("~/org/todos")))))
  (org-agenda-refresh-files)
  (setq calendar-week-start-day 1)

  (org-add-agenda-custom-command
        '("n" "Agenda and all TODOs"
          (
           (agenda "" ((org-agenda-show-all-dates nil)
                       (org-agenda-start-day "today")
                       (org-agenda-span 7)
                       (org-agenda-skip-scheduled-if-deadline-is-shown t)
                       (org-agenda-scheduled-leaders '("S." "S."))))
           (todo "TODO" ((org-agenda-todo-ignore-scheduled 'future)))
           )))
  )

;;; Journal
(use-package! org-journal
  :config
  (setq org-journal-file-type 'daily
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-time-format ""
        org-journal-dir "~/org/journal/lab-notebook")
  (map! :map doom-leader-notes-map
        (:prefix-map ("j" . "journal")
        :desc "Journal new entry"        "j" #'journal-new-entry
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
                   (format-time-string "%Y-%m-%d.org"
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


;;; Bibtex
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
    ;; (org-ref-ivy-cite-completion)
    (setq org-ref-completion-library 'org-ref-ivy-cite
          org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
          org-ref-default-bibliography '("~/zotero/library.bib")
          org-ref-bibliography-notes "~/Nextcloud/org/notes/bibnotes.org"
          org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
          org-ref-notes-directory "~/Nextcloud/org/notes/"
          org-ref-notes-function 'orb-edit-notes
          ))

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

;;; Notes
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
  (setq orb-preformat-keywords
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

