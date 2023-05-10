;;; +tex.el -*- lexical-binding: t; -*-

(me/add-eager-package "tex" '(latex))
(use-package! latex
  :defer t
  :init
  (setq TeX-style-private
        (list (expand-file-name TeX-style-local
                                (or (concat user-emacs-directory "auctex/")
                                    "~/.emacs.d/auctex/"))
              (expand-file-name (concat doom-user-dir "auctex-styles"))))

  :config
  (remove-hook! '(tex-mode-local-vars-hook
                  latex-mode-local-vars-hook)
    #'lsp!)

  (defun me/add-comma-breakable-char ()
    "Add comma to the breakable category.

(buffer local)"
    (setq word-wrap-by-category t)
    (modify-category-entry ?, ?\|))
  (add-hook! latex-mode-hook #'me/add-comma-breakable-char)

  (after! font-latex
    (setq font-latex-fontify-script nil))

  ;; (after! flycheck
  ;;   (defun add-latex-next-checkers ()
  ;;     (setq flycheck-local-checkers
  ;;           '((lsp . ((next-checkers . (grammalecte)))))))
  ;;   (add-hook 'LaTeX-mode-hook 'add-latex-next-checkers))

  (setq tex-directory "build"
        TeX-electric-sub-and-superscript nil)
  (setq-default TeX-master nil)

  (setenv "TEXINPUTS" ".:src:tex:build:")
  (setenv "LUAINPUTS" ".:src:")
  (setenv "BIBINPUTS" ".:ref:")
  (setq reftex-texpath-environment-variables
        (mapcar (lambda (x)
                  (concat "/home/clement/Documents/Work/Thèse/" x))
                '("." "src" "tex" "build")))
  (setq reftex-bibpath-environment-variables
        '("/home/clement/Documents/Work/Thèse/references"))

  (setq font-latex-deactivated-keyword-classes '("textual"))

  (defcustom me/latex-font-list
    '((nil .
       (("em: Emphasize" . ("emph" nil))
        ("nm: Normal" . ("textnormal" nil))
        ("vb: Verb" . ("verb" nil))))
      ("Family" .
       (("rm: Roman" . ("textrm" nil))
        ("sf: Sans Serif" . ("textsf" nil))
        ("tt: Monospace" . ("texttt" nil))))
      ("Shape" .
       (("up: Upright" . ("textup" nil))
        ("it: Italic" . ("textit" nil))
        ("sl: Slanted" . ("textsl" nil))
        ("sc: SmallCaps" . ("textsc" nil))))
      ("Weight" .
       (("bf: Bold" . ("textbf" "mathbf"))
        ("md: Medium" . ("textmd" nil))
        ("lf: Light" . ("textlf" nil))))
      )
    "Latex font commands."
    :group 'me
    :type '(repeat
            (alist :key-type string
                   :value-type (repeat
                                (alist :key-type string
                                       :value-type (list (choice string nil))))))
    )

  (defun me/consult-latex-fonts ()
    "Choose a latex font command to insert from selection."
    (consult--multi
     (mapcar
      (lambda (typespec)
        (let ((type (car typespec))
              (spec (cdr typespec)))
          (list :name type :category 'unordered
                :items (mapcar #'car spec)
                :annotate (lambda (cand)
                            (mapconcat (lambda (cmd)
                                         (if cmd (format "\\%s" cmd) ""))
                                       (cdr (assoc-string cand spec))
                                       "\t\t")))))
      me/latex-font-list)
     :sort nil))

  (defun me/latex-insert-font (normal math)
    "Insert command NORMAL or MATH (depending on environment) to change font.

If called interactively, choose from `me/latex-font-spec' using consult.
"
    (interactive
     (let ((select (me/consult-latex-fonts)))
       (if (plist-get (cdr select) :match)
           (cdr (assoc-string (car select) (cdr (assoc-string (plist-get (cdr select) :name) me/latex-font-list))))
         (make-list 2 (car select)))))
    (message normal math)
    (when-let* ((cmd (if (texmathp) math normal))
                (before (concat "\\" cmd "{"))
                (after "}"))
      (if (string-equal cmd "verb")
          (setq before "\\verb|" after "|"))
      (cond
       ((TeX-active-mark)
        (save-excursion
          (cond ((> (mark) (point))
                 (insert before)
                 (goto-char (mark))
                 (insert after))
                (t
                 (insert after)
                 (goto-char (mark))
                 (insert before)))))
       (t
        (insert before)
        (save-excursion
          (insert after))))))

  ;; Add manual sections to imenu (useful for navigating preamble)
  (add-hook 'LaTeX-mode-hook
            (defun me/add-tex-manual-section-imenu ()
              (setq lsp-enable-imenu nil)
              (add-to-list 'imenu-generic-expression
                           '("Manual Sections" "^%%% +\\([^%\n]*\\)$" 1))
              (setq imenu-create-index-function
                    (defun me/imenu-create-index-tex () (imenu--generic-function imenu-generic-expression)))))

  ;; Dont fontify footnotes
  (add-hook 'LaTeX-mode-hook
            (defun me/no-footnote-fontify ()
              (setq-default font-latex-match-reference-keywords-local
                            (seq-difference font-latex-match-reference-keywords-local
                                            '("footnote" "footnotetext")
                                            (lambda (a b) (string= a (car b)))))))

  ;; Remove automatic insertion of latex stuff for quotes
  (map! :map TeX-mode-map
        "\"" nil)

  (map! :map (LaTeX-mode-map latex-mode-map)
        :localleader
        :desc "Newline"             "RET" #'TeX-newline
        :desc "Math mode"           "~" #'LaTeX-math-mode
        :desc "Insert font command" "f" #'me/latex-insert-font
        :desc "Insert macro"        "." #'TeX-insert-macro
        :desc "Insert section"      "s" #'LaTeX-section
        :desc "Insert environment"  "e" #'LaTeX-environment

        :desc "Complete symbol"     "*" #'TeX-complete-symbol
        :desc "Close environment"   "}" #'LaTeX-close-environment
        :desc "Find matching begin" "[" #'LaTeX-find-matching-begin
        :desc "Find matching end"   "]" #'LaTeX-find-matching-end

        :desc "Set master file" "_" #'TeX-master-file-ask
        :desc "View"            "v" #'TeX-view
        :desc "Save document"   "d" #'TeX-save-document
        :desc "Up list"         "^" #'up-list
        :desc "Normal mode"     "#" #'TeX-normal-mode

        (:prefix ("i" . "insert")
         :desc "quote"       "\"" #'TeX-insert-quote
         :desc "dollar sign" "$" #'TeX-insert-dollar
         :desc "backslash"   "\\" #'TeX-insert-backslash
         :desc "environment" "e" #'LaTeX-environment
         :desc "macro"       "." #'TeX-insert-macro
         :desc "item"        "i" #'LaTeX-insert-item
         :desc "font"        "f" #'me/latex-insert-font
         :desc "section"     "s" #'LaTeX-section)

        (:prefix ("t" . "toggle")
         :desc "Interactive"               "i" #'TeX-interactive-mode
         :desc "Source correlate"          "s" #'TeX-source-correlate-mode
         :desc "Pin region"                "r" #'TeX-pin-region
         :desc "Bad boxes"                 "b" #'TeX-toggle-debug-bad-boxes
         :desc "Warnings"                  "w" #'TeX-toggle-debug-warnings
         :desc "Suppress ignored warnings" "x" #'TeX-toggle-suppress-ignored-warnings
         :desc "Fold"                      "f" #'TeX-fold-mode)

        (:prefix ("q" . "fill")
         :desc "paragraph"   "p" #'LaTeX-fill-paragraph
         :desc "region"      "r" #'LaTeX-fill-region
         :desc "section"     "s" #'LaTeX-fill-section
         :desc "environment" "e" #'LaTeX-fill-environment)

        "p" nil
        "P" nil
        (:prefix ("p" . "preview")
         :desc "Preview region"         "r" #'preview-region
         :desc "Preview point"          "p" #'preview-at-point
         :desc "Preview buffer"         "b" #'preview-buffer
         :desc "Preview document"       "d" #'preview-document
         :desc "Preview cache preamble" "f" #'preview-cache-preamble
         :desc "Preview environment"    "e" #'preview-environment
         :desc "Preview section"        "s" #'preview-section
         :desc "Clearout region"        "R" #'preview-clearout
         :desc "Clearout at point"      "P" #'preview-clearout-at-point
         :desc "Clearout section"       "S" #'preview-clearout-section
         :desc "Clearout buffer"        "B" #'preview-clearout-buffer
         :desc "Clearout document"      "D" #'preview-clearout-document)

        )

  ; Remove rainbow-delimiters in tex files (it doesn't work perfectly
  ; and color stuff red chaotically)
  (remove-hook 'TeX-update-style-hook #'rainbow-delimiters-mode)

  ;; Verbatim syntax highlighting for lua code
  (add-to-list 'LaTeX-verbatim-environments "luacode*")
  (appendq! LaTeX-verbatim-macros-with-braces '("luaexec" "directlua"))

  (add-to-list 'TeX-view-program-list
               '("Zathura"
                 ("zathura %o"
                  (mode-io-correlate " --synctex-forward %n:0:\"%b\" -x \"emacsclient +%{line} %{input}\""))
                 "zathura"))

  (defun TeX-view ()
    (interactive)
    (let ((output-file (TeX-active-master (TeX-output-extension))))
      (if (file-exists-p (concat tex-directory "/" output-file))
          (TeX-command "View" 'TeX-active-master 0)
        (message "Output file %S does not exist." output-file))))
  )

(use-package! latex
  :after smartparens
  :config
  (sp-with-modes '(tex-mode plain-tex-mode latex-mode LaTeX-mode)
    (sp-local-pair "``" "''" :actions '(rem))))

(me/add-eager-package "tex" 'reftex)
(use-package! reftex
  :defer t
  :config
  (setq reftex-trust-label-prefix t
        reftex-label-menu-flags '(nil nil nil nil t nil nil nil))

  (setq reftex-default-context-regexps
        (assoc-delete-all 'caption reftex-default-context-regexps))
  (add-to-list 'reftex-default-context-regexps
               '(caption .  "\\\\\\(rot\\|bi\\)?\\(sub\\)?captionT?\\(box\\)?\\*?[[{]"))

  (map! :map (LaTeX-mode-map latex-mode-map)
        :localleader
        :desc "Insert ref" "r" #'reftex-reference)
  )

(me/add-eager-package "tex" 'reftex-ref)
(use-package! reftex-ref
  :defer t
  :config
  (map! :map reftex-mode-map
        :localleader
        (:prefix ("g" . "goto")
        :nv :desc "Goto label" "l" #'reftex-goto-label))
)

(use-package! company-reftex
  :defer t
  :config
  ;; Add \nref to completion commands and optional arguments
  (setq company-reftex-labels-regexp
        (rx "\\"
            (or "autoref"
                "autopageref"
                "Cpageref"
                "cpageref"
                "Cref"
                "cref"
                "eqref"
                "nref"
                "pageref"
                "Ref"
                "ref")
            (opt (seq "[" (* (not (any "]"))) "]"))
            "{"
            (group (* (not (any "}"))))
            (regexp "\\="))
        )
  )


(me/add-eager-package '("tex" "lsp") 'lsp-latex)
(use-package! lsp-latex
  :defer t
  :config
  (setq lsp-latex-build-executable "make"
        lsp-latex-root-directory nil
        lsp-latex-build-aux-directory "texbuild"
        lsp-latex-build-args '()

        lsp-latex-chktex-on-open-and-save t
        lsp-latex-chktex-on-edit nil)
  (lsp-latex-setup-variables)
  (lsp-register-custom-settings
   `(("texlab.auxDirectory" lsp-latex-build-aux-directory)))

  (map! :map LaTeX-mode-map
        :localleader
        "c" :desc "Compile" #'lsp-latex-build)
  )

(defun me/slice-par-line ()
  "Put a newline for each sentence inside range defined with START and END."
  (interactive "*")
  (save-excursion
    (forward-paragraph)
    (let ((end (point)))
      (backward-paragraph)
      (while (< (point) end)
        (forward-sentence)
        (delete-horizontal-space)
        (unless (= (line-end-position) (point))
          (newline)))))
  )
