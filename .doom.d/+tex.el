;;; +tex.el -*- lexical-binding: t; -*-


(setq tex-directory ".texbuild")

(require 'lsp-latex)

(after! lsp-latex
  (setq lsp-latex-build-aux-directory ".texbuild")
  (setq lsp-latex-build-args '("-pdflua" "-interaction=nonstopmode" "-synctex=1" "-outdir=.texbuild" "-cd" "%f"))

  (map! :map TeX-mode-map
        :localleader
        "c" :desc "Compile" #'lsp-latex-build)
  )

(after! auctex
  (add-to-list 'TeX-view-program-list
               '("Zathura"
                 ("zathura .texbuild/%o"
                  (mode-io-correlate " --synctex-forward %n:0:\"%b\" -x \"emacsclient +%{line} %{input}\""))
                 "zathura"))

  (defun TeX-view ()
    (interactive)
    (let ((output-file (TeX-active-master (TeX-output-extension))))
      (if (file-exists-p (concat tex-directory "/" output-file))
          (TeX-command "View" 'TeX-active-master 0)
        (message "Output file %S does not exist." output-file)))))
