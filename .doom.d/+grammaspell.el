;;; +grammar.el -*- lexical-binding: t; -*-

;; Does spell checking with ispell and grammar with grammalecte
;; Ispell.el is nice but only available in interactive mode, to have
;; errors underlined you must use flyspell which uses a different api (erk).
;; Also, it does not allow to go back to previous word in session.
;; Grammalecte is also useful but also uses a different API.
;;
;; Ispell (really hunspell) allows for piping sentences incrementally, which
;; allows to deal with filtering within emacs. This is not the case for grammalecte.
;; Also grammalecte is a python program, it is costly to launch repeatedly.
;; I aim to start an ispell process, go through the buffer while removing tex
;; commands, send portions of text between commands to filter out to ispell *and*
;; copy them to a secondary buffer (with the commands replaced by blank text).
;; The secondary buffer is then sent to grammalecte.
;; All errors are collected and sent to flycheck.
;; Mispells and eventual replacements are kept somewhere to be accessible quickly
;; for a correct-word command (alist ? buffer ?).


(defgroup grammalecte nil
  "Grammalecte custom variables"
  :group 'text)

(defcustom grammaspell-min-word-size 4
  "Minimum size of word to be spellchecked."
  :type 'integer
  :group 'grammalecte)

(defcustom grammaspell-borders-by-mode
  '((latex-mode . "^\\\\begin{document}"))
  "Line before which checking is not done, by mode."
  :type '(alist (function :tag "Mode") string)
  :group 'grammalecte)

(defcustom grammaspell-cmd-rgx
  (rx (: "\\" (group (* word))))
  "Regexp for finding the next command."
  :type 'string
  :group 'grammalecte)
(setq grammaspell-cmd-rgx (rx (: "\\" (group (* word)) (? "*"))))

(defcustom grammaspell-cmd-list nil
  "Alist of latex commands which arguments must
be ignored.

The cdr of each element can be:
- an integer (# of mandatory arguments to ghost)
- a list of integers (alternating # of mandatory
  and optional arguments to ignore, the first
  is mandatory). It is passed to TeX-ispell-tex-arg-end.
- same, but the first item is the :verb symbol,
  the rest of integers is passed to TeX-ispell-tex-arg-verb-end.

Elements can be added with `grammaspell-add-to-cmd-list'."
  :type '(repeat (alist :key string))
  :group 'grammaspell)


(defcustom grammaspell-env-list nil
  "Alist of latex env. where arguments must be ignored."
  :type '(repeat (alist :key string))
  :group 'grammaspell)


(setq grammaspell-cmd-list
      '(;; base
        ("addcontentsline" . 2)
        ("graphicspath" . 1)
        ("include" . 1)
        ("label" . 1)
        ("markboth" . 2)
        ("ref" . 1)
        ("setlength" . 2)
        ))

(setq grammaspell-env-list
      '())

(defun grammaspell-add-cmd-list (cmds)
  "Add elements to list of commands with arguments
to ghost."
  (setq grammaspell-cmd-list
        (append grammaspell-cmd-list cmds)))

(defun grammaspell-add-cmd-byarg (arg cmds)
  "Add commands to list of commands with arguments
to ghost."
  (dolist (c cmds)
    (add-to-list 'grammaspell-cmd-list
                 (cons c arg))))

(defun grammaspell-add-env-list (envs)
  "Add elements to list of environments with arguments
to ghost."
  (setq grammaspell-env-list
        (append grammaspell-env-list envs)))


;; babel
;; TODO: a command to check a region
(grammaspell-add-cmd-list
 '(("foreignblockquote" . 2)))

;; biblatex
(grammaspell-add-cmd-byarg 1
 '("cite" "Cite"
   "parencite"
   "foocite" "footcitetext"
   "textcite" "Textcite"
   "citeauthor" "Citeauthor"
   "citeyear"
   "fullcite" "footfullcite"))

;; cleveref.sty
(grammaspell-add-cmd-byarg 1
 '("cref" "Cref"
   "cpageref" "Cpageref"
   "namecref" "nameCref"
   "lcnamecref" "labelcref"))
(grammaspell-add-cmd-byarg 2
 '("crefrange" "Crefrange"
   "cpagerefrange" "Cpagerefrange"))

;; glossaries.sty
(grammaspell-add-cmd-byarg '(1 1 0)
  '("ab" "Ab" "AB" "abp" "Abp" "ABP"
    "as" "As" "AS" "asp" "Asp" "ASP"
    "af" "Af" "AF" "afp" "Afp" "AFP"
    "al" "Al" "AL" "alp" "Alp" "ALP"))
(grammaspell-add-cmd-byarg 1
  '("glsentryshort" "glsentryname" "glsentrylong" "glsentrytext"
    "printglossary"))

;; hyperref.sty
(grammaspell-add-cmd-byarg 1
 '("href" "url" "nolinkurl"))

;; listings.sty
(grammaspell-add-cmd-byarg 1
 '("lstinputlisting" "lstset"))

;; nameref.sty
(grammaspell-add-cmd-byarg 1
 '("nameref" "Nameref"))

;; siunitx.sty
(grammaspell-add-cmd-byarg 1
 '("num" "numlist" "numproduct"
   "ang" "unit"))
(grammaspell-add-cmd-byarg 2
 '("numrange" "qty"
   "qtylist" "qtyproduct"))

(grammaspell-add-cmd-byarg 3 '("qtyrange"))

(setq grammaspell-env-skip
      '("verbatim"
        "filecontents"
        "equation"))

(defun grammaspell-send-line (str)
  "Send STR to hunspell process."
  (if (process-live-p hunspell-process)
      (process-send-string hunspell-process str)))

;;; Custom
(grammaspell-add-cmd-list
 '(("glsurl" . 1)
   ("glshref" . 1)
   ("citesoft" . 1)
   ("reftitle" . 1)
   ("creftitle" . 1)
   ("declareDataset" . 1)
   ("dataname" . 1)
   ("datasect" . 1)
   ("eng" . 1)
   ("engquote" . 1)
   ("nref" . 1)
   ("insertfig" . 1))
 )

(grammaspell-add-env-list
 '(("note" . 0)
   ("flexlabelled" . 6)))


(defvar hunspell-mispells nil
  "List of mispelled words and the point of the line beginning
where they are located.")

(defvar hunspell-process nil
  "Process.")

(defvar hunspell-filter nil
  "Variable holding hunspell output")

(defvar hunspell-filter-continue nil
  "Control variable for Hunspell filter function.")

(defvar hunspell-buffer nil
  "Buffer for Grammalecte / Hunspell input.")

(defun hunspell-filter (_process output)
  "Output filter function for hunspell.

Stolen from ispell.el."
  (let ((start 0)
        (continue t)
        end)
    (while continue
      (setq end (string-search "\n" output start)) ; get text up to the newline.
      ;; If we get out of sync and hunspell-filter-continue is asserted when we
      ;; are not continuing, treat the next item as a separate list.  When
      ;; hunspell-filter-continue is asserted, hunspell-filter *should* always be a
      ;; list!

      ;; Continue with same line (item)?
      (if (and hunspell-filter-continue hunspell-filter (listp hunspell-filter))
          ;; Yes.  Add it to the prev item
          (setcar hunspell-filter
                  (concat (car hunspell-filter) (substring output start end)))
        ;; No. This is a new line and item.
        (setq hunspell-filter
              (cons (substring output start end) hunspell-filter)))
      (if (null end)
          ;; We've completed reading the output, but didn't finish the line.
          (setq hunspell-filter-continue t continue nil)
        ;; skip over newline, this line complete.
        (setq hunspell-filter-continue nil end (1+ end))
        (if (= end (length output))     ; No more lines in output
            (setq continue nil)                 ;  so we can exit the filter.
          (setq start end))))))              ; else move start to next line of input

(defun grammaspell-skip-borders ()
  "Remove borders. Return shift from removing start."
  (goto-char (point-min))
  (if (re-search-forward "^\\\\begin{document}" nil t)
      (grammaspell-ghost (point-min) (match-end 0)))
  (if (re-search-forward "^\\\\end{document}" nil t)
      (delete-region (match-beginning 0) (point-max)))
  )

(defun grammaspell-goto-env-end (env)
  "Go to the matching \\end of ENV."
  (let ((regexp (format "\\\\\\(begin\\|end\\){%s}" env))
        (level 1))
    (while (and (> level 0) (re-search-forward regexp nil t))
      (if (= (match-beginning 1) ?b) ;; begin
          (setq level (1+ level))
        (setq level (1- level))))
    (unless (= level 0)
      (error "Can't locate end of %s environment" env))
    (match-beginning 0)) ;; we could return if there is env inside and we must recurse
  )

(defun grammaspell-ghost (start end)
  "Replace characters between START and END by whitespace."
  ;; I could use evil <R><space> ?
  (let ((nchar (- end start)))
    (delete-region start end)
    (insert-char ?\s nchar)))

(defun grammaspell-filter-buffer ()
  "Filter out stuff not to check."
  ;; Removing environments
  (goto-char (point-min))
  (while (re-search-forward "\\\\begin{\\([^}]+?\\)}" nil t)
    (let* ((start (match-beginning 0))
           (end (match-end 0))
           (env (match-string 1))
           (react (alist-get env grammaspell-env-list nil nil #'equal)))
      (grammaspell-ghost start end)
      ;; simplest stuff, only integer supported
      (when react
        (ispell-tex-arg-end react)
        (grammaspell-ghost end (point)))
      (grammaspell-goto-env-end env)
      (if (member env grammaspell-env-skip)
        (grammaspell-ghost start (point))
        (grammaspell-ghost (match-beginning 0) (point)))
      (goto-char start)))
  ;; Removing math
  (goto-char (point-min))
  (while (re-search-forward "\\\\(.*?\\\\)" nil t)
    (grammaspell-ghost (match-beginning 0) (match-end 0)))
  ;; Removing commands
  (goto-char (point-min))
  (while (re-search-forward grammaspell-cmd-rgx nil t)
    (let* ((start (match-beginning 0))
           (cmd (match-string 1))
           (react (alist-get cmd grammaspell-cmd-list 0 nil #'equal)))
      (ispell-tex-arg-end 0)
      (cond
       ((integerp react)
        (unless (= react 0)
          (ispell-tex-arg-end react)))
       ((equal (car react) 'verb)
        (apply #'TeX-ispell-tex-arg-verb-end (cdr react)))
       (t
        (apply #'TeX-ispell-tex-arg-end react)))
      (grammaspell-ghost start (point))
      ))
  ;; ;; Remove short words
  ;; (goto-char (point-min))
  ;; (while (re-search-forward "\\<\\w\\{1,3\\}\\>" nil t)
  ;;   (grammaspell-ghost (match-beginning 0) (match-end 0)))
  ;; ;; ;; Replacements
  ;; (goto-char (point-min))
  ;; (while (re-search-forward "\"[-~]" nil t)
  ;;   (replace-match "█-" nil nil))
  )

(defun grammaspell-send-buffer ()
  "Send secondary buffer to Hunspell for correction."
  (goto-char (point-min))
  (while (not (eobp))
    (let ((input (buffer-substring-no-properties (point) (eol)))
          (begline (point)))
      (when (and (string-match "[^[:space:]]" input)
                 (not (string-match "[*@#~+-!%`^]" input 0)))
        (setq input (s-replace "\"~" "-" input))
        (setq input (s-replace "\"-" "-" input))
        (grammaspell-send-line (concat input "\n"))
        (accept-process-output hunspell-process 0.001)
        (while hunspell-filter
          (let ((mispell (car hunspell-filter)))
            (when (> (length mispell) grammaspell-min-word-size)
                (push (cons begline mispell) hunspell-mispells)))
          (setq hunspell-filter (cdr hunspell-filter))))
      (beginning-of-line 2)))
  )


(defun grammaspell--flycheck-errors (checker)
  (grammaspell-buffer)
  (mapcar (lambda (err)
            (let ((begline (car err))
                  (word (cdr err))
                  wordpos)
              (save-excursion
                (goto-char begline)
                (setq wordpos
                      (if (search-forward word (eol) t)
                          (match-beginning 0)
                        (bol))))
              (flycheck-error-new-at-pos
               wordpos 'warning (format "%s at %d" word wordpos)
               :checker checker)))
          hunspell-mispells))

(defun grammaspell--flycheck-start (checker callback)
  (condition-case err
      (let ((errors (grammaspell--flycheck-errors checker)))
        (funcall callback 'finished errors))
    (error (funcall callback 'errored (error-message-string err))))
  )

(flycheck-define-generic-checker 'grammaspell
  "Hunspell stuff boi."
  :start #'grammaspell--flycheck-start
  :modes '(latex-mode LaTeX-mode)
  )


(defun grammaspell-buffer ()
  "idk"
  (interactive)
  ;; Those two calls should be rewritten
  ;; ispell.el is a mess concerning dictionnaries
  (ispell-kill-ispell t t)
  (setq hunspell-filter nil
        hunspell-filter-continue nil
        hunspell-mispells nil)
  (setq hunspell-process (apply 'start-process
                                "hunspell" nil "hunspell"
                                '("-l" "-d" "fr"))
        )
  (set-process-filter hunspell-process 'hunspell-filter)
  (accept-process-output hunspell-process 0.5)
  (grammaspell-send-line "!\n") ;; pass in terse mode
  (setq hunspell-filter nil) ;; discard version line and stuff
  (let ((curbuf (current-buffer))
        (secbuf (get-buffer-create "*grammaspell*" t)))
    (set-buffer secbuf)
    (LaTeX-mode)
    (with-current-buffer curbuf
      (copy-to-buffer secbuf (point-min) (point-max)))
    (grammaspell-skip-borders)
    (grammaspell-filter-buffer)
    (grammaspell-send-buffer)
    (set-buffer curbuf))
  (quit-process hunspell-process)
  )


;;
