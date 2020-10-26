;;; +python.el -*- lexical-binding: t; -*-

;;; Python main
(use-package! python
  :init
  (setq! python-shell-interpreter "ipython"
         python-shell-interpreter-args "console --simple-prompt"
         python-shell-prompt-detect-failure-warning nil))

;;; Python cells
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

;;; Zeal docsets
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


;;; Anaconda
(use-package! anaconda-mode
  :init
  (defun inhibit-anaconda-remote ()
    (when (file-remote-p (buffer-file-name))
      (anaconda-mode -1)
      (anaconda-eldoc-mode -1)))
  (add-hook! 'find-file-hook #'inhibit-anaconda-remote)
  )


;;; Jupyter
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


;;; Faces
(defface font-lock-ds-arguments-face
  '((t :inherit font-lock-doc-face
       :slant normal)) "Face for docstring arguments.")

(font-lock-add-keywords 'python-mode
                        '(("[ ^:]*:param \\([a-zA-Z0-9_^:]*\\):" 1 "font-lock-ds-arguments-face" t)))
