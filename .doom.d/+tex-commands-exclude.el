;;; +tex-commands-exclude.el -*- lexical-binding: t; -*-

;; Latex commands not to spellcheck

;; Taken from auctex/tex-ispell.el
;; I took only the commands I am interested in and added a few of
;; my own

(setq me/TeX-ispell-skip-cmds-list
      '(;; base
        ("addcontentsline" . 2)

        ;; biblatex
        ("cite" . 1)
        ("cite*" . 1)
        ("Cite" . 1)
        ("parencite" . 1)
        ("parencite*" . 1)
        ("foocite" . 1)
        ("footcitetext" . 1)
        ("textcite" . 1)
        ("Textcite" . 1)
        ("citeauthor" . 1)
        ("citeauthor*" . 1)
        ("Citeauthor" . 1)
        ("Citeauthor*" . 1)
        ("fullcite" . 1)
        ("footfullcite" . 1)

        ;; cleveref.sty
        ("cref" . 1)
        ("Cref" . 1)
        ("cref*" . 1)
        ("Cref*" . 1)
        ("cpageref" . 1)
        ("Cpageref" . 1)
        ("namecref" . 1)
        ("nameCref" . 1)
        ("lcnamecref" . 1)
        ("labelcref" . 1)
        ("crefrange" . 2)
        ("Crefrange" . 2)
        ("cpagerefrange" . 2)
        ("Cpagerefrange" . 2)
        ("crefrange*" . 2)
        ("Crefrange*" . 2)

        ;; csquotes
        ;; ("enquote" . 1)
        ;; ("enquote*" . 1)

        ;; fontspec.sty
        ("addfontfeatures" . 1)

        ;; hyperref.sty
        ("href" . 1)
        ("url" . 1)
        ("nolinkurl" . 1)

        ;; listings.sty
        ("lstinputlisting" . 1)
        ("lstset" . 1)

        ;; nameref.sty
        ("nameref" . 1)
        ("Nameref" . 1)

        ;; siunitx.sty
        ("num" . 1)
        ("numlist" . 1)
        ("numproduct" . 1)
        ("numrange" . 2)
        ("ang" . 1)
        ("unit" . 1)
        ("qty" . 2)
        ("qtylist" . 2)
        ("qtyproduct" . 2)
        ("qtyrange" . 3)
        ))

(defvar me/TeX-ispell-skip-cmds-opt-arg-after-regexp
  (eval-when-compile
    (rx (: "\\" (or "fontspec"
                    ;; this will use regexp-opt
                    (or "ab" "Ab" "AB" "abp" "Abp" "ABP"
                        "as" "As" "AS" "asp" "Asp" "ASP"
                        "af" "Af" "AF" "afp" "Afp" "AFP"
                        "al" "Al" "AL" "alp" "Alp" "ALP")))))
  "Regexp for commands that have *one* optional argument *after*
their regular argument.
This includes glossaries accronyms.")


;; Reset default value
(setq ispell-tex-skip-alists '(nil nil))

;; base values
(TeX-ispell-skip-setcar
 '(("\\\\(" "\\\\)")))

;; I do not add verbatim commands

;; Environments
;; Latex envs with an opt argument to be skipped
(setq me/TeX-ispell-skip-envs-opt-arg-list
      '("enumerate"
        "itemize"))

;; Environments which should be skipped entirely here:
(setq me/TeX-ispell-skip-envs-list
      '("verbatim"
        "luacode*"
        ;; amsmath.sty
        "align"
        "align*"
        "multline"
        "multline*"
        ;; listings.sty
        "lstlisting"))


(defun me/TeX-ispell-sort-skip-cmds-list (arg)
  "Return elements from `TeX-ispell-skip-cmds-list' acc. to ARG."
  (when (member arg '(0 1 2 3))
    (let (cmds)
      (dolist (elt me/TeX-ispell-skip-cmds-list)
        (when (= (cdr elt) arg)
          (push (car elt) cmds)))
      cmds)))

(setq me/TeX-ispell-skip-cmds-opt-arg-regexp
  (eval-when-compile
    (concat "\\\\"
            (regexp-opt (me/TeX-ispell-sort-skip-cmds-list 0) t))))

(setq me/TeX-ispell-skip-cmds-one-arg-regexp
  (eval-when-compile
    (concat "\\\\"
            (regexp-opt (me/TeX-ispell-sort-skip-cmds-list 1) t))))

(setq me/TeX-ispell-skip-cmds-two-args-regexp
  (eval-when-compile
    (concat "\\\\"
            (regexp-opt (me/TeX-ispell-sort-skip-cmds-list 2) t))))

(setq me/TeX-ispell-skip-cmds-three-args-regexp
  (eval-when-compile
    (concat "\\\\"
            (regexp-opt (me/TeX-ispell-sort-skip-cmds-list 3) t))))

(setq me/TeX-ispell-skip-envs-opt-arg-regexp
  (eval-when-compile
    (regexp-opt me/TeX-ispell-skip-envs-opt-arg-list t)))

(setq me/TeX-ispell-skip-envs-regexp
  (eval-when-compile
    (regexp-opt me/TeX-ispell-skip-envs-list t)))

;; Make them available to Ispell:
(TeX-ispell-skip-setcar
 `((,me/TeX-ispell-skip-cmds-opt-arg-regexp ispell-tex-arg-end 0)
   (,me/TeX-ispell-skip-cmds-one-arg-regexp ispell-tex-arg-end 1)
   (,me/TeX-ispell-skip-cmds-two-args-regexp ispell-tex-arg-end 2)
   (,me/TeX-ispell-skip-cmds-three-args-regexp ispell-tex-arg-end 3)
   (,me/TeX-ispell-skip-cmds-opt-arg-after-regexp TeX-ispell-tex-arg-end 1 1 0)))

(TeX-ispell-skip-setcdr
 `((,me/TeX-ispell-skip-envs-opt-arg-regexp ispell-tex-arg-end 0)
   ,(cons me/TeX-ispell-skip-envs-regexp
          (concat "\\\\end{" me/TeX-ispell-skip-envs-regexp "}"))))
