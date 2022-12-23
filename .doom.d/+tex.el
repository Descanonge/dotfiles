;;; +tex.el -*- lexical-binding: t; -*-


(setq tex-directory "texbuild")
(setq font-latex-fontify-script nil)

(after! tex
  (setq TeX-output-dir "texbuild"))

(after! lsp-latex

  (setq lsp-latex-build-executable "make"
        lsp-latex-root-directory "."
        lsp-latex-build-aux-directory "texbuild"
        lsp-latex-build-args '())
  (lsp-latex-setup-variables)
  (lsp-register-custom-settings
   `(("texlab.auxDirectory" lsp-latex-build-aux-directory)))

  (map! :map LaTeX-mode-map
        :localleader
        "c" :desc "Compile" #'lsp-latex-build)
  )

(after! reftex
  (setq reftex-cite-format
        '((?a . "\\autocite[]{%l}")
          (?b . "\\blockcquote[]{%l}{}")
          (?c . "\\cite[]{%l}")
          (?f . "\\footcite[]{%l}")
          (?n . "\\nocite{%l}")
          (?P . "\\parencite[]{%l}")
          (?s . "\\smartcite[]{%l}")
          (?t . "\\textcite[]{%l}")
          (?A . "\\citeA[]{%l}")
          (?p . "\\pcite[]{%l}")))
  )

(after! auctex
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
        (message "Output file %S does not exist." output-file)))))


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
