;;; +python.el -*- lexical-binding: t; -*-


;;; Python main
(me/add-eager-package "python" 'python)
(use-package! python
  :defer t
  :config
  (remove-hook! 'python-mode-local-vars-hook #'lsp!)

  (use-package! flycheck
    :defer t
    :config
    (setq flycheck-flake8rc "~/.config/flake8"
          flycheck-python-mypy-config "~/.config/mypy/config")
    )

  (defun me/python-enumerate (funcname &optional beg end)
    "Surround word at point by enumerate."
    (interactive "sFunction: \nr")
    (save-excursion
      (unless mark-active
        (evil-backward-symbol-begin 0)
        (setq beg (point))
        (evil-snipe-f 1 '(?\:))
        (setq end (point)))
      (evil-surround-region beg end 'block ?\) )
      (goto-char beg)
      (insert funcname))
    )

  (map! :map python-mode-map
        :localleader
        :desc "Enumerate" :niv "s" (lambda () (interactive) (me/python-enumerate "enumerate"))
        :desc "Zip" :niv "z" (lambda () (interactive) (me/python-enumerate "zip")))

  (defun me/open-package ()
    "Find file from python package.
Interactively ask for package name, listing those found in ~/.packages"
    (interactive)
    (let* ((condadir "~/.micromamba/")
           (env (completing-read
                 "Environement: "
                 (cl-remove-if
                  (lambda (f) (s-starts-with? "." f))
                  (directory-files (concat condadir "envs/")))))
           (pythonversion (car (-remove #'f-symlink?
                                        (directory-files
                                         (concat condadir "envs/" env "/lib/")
                                         t "^python3\.[[:digit:]]+" nil))))
           (package (concat pythonversion "/site-packages/" (completing-read
                     "Package: "
                     (cl-remove-if
                      (lambda (d) (--some (s-ends-with? it d)
                                          '(".dist-info" ".egg-info"
                                            ".egg-link" ".so" ".pth")))
                      (directory-files (concat pythonversion "/site-packages"))))))
           (file (if (f-dir? package)
                     (read-file-name
                      "File: "
                      (concat package "/")
                      (confirm-nonexistent-file-or-buffer))
                   package)))
      (find-file file)))
  )


;;; LSP
(me/add-eager-package '("python" "lsp") 'lsp-pylsp)
(use-package! lsp-pylsp
  :defer t
  :config
  (setq lsp-pylsp-configuration-sources ["flake8" "pydocstyle"]
        lsp-pylsp-plugins-flake8-enabled t

        lsp-pylsp-plugins-pydocstyle-enabled t
        lsp-pylsp-plugins-pydocstyle-convention "numpy"
        lsp-pylsp-plugins-pydocstyle-add-ignore ["D100" "D101" "D102" "D103"]

        lsp-pylsp-plugins-mccabe-enabled nil
        lsp-pylsp-plugins-pycodestyle-enabled nil
        lsp-pylsp-plugins-pyflakes-enabled nil
        lsp-pylsp-plugins-pylint-enabled nil)
  )

;;; Python cells
(me/add-eager-package "python" 'python-cell)
(use-package! python-cell
  :defer t
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
  :disabled
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

;;; Anaconda
(use-package! anaconda-mode
  :defer group-defer
  :init
  (defun inhibit-anaconda-remote ()
    (when (file-remote-p (buffer-file-name))
      (anaconda-mode -1)
      (anaconda-eldoc-mode -1)))
  (add-hook! 'find-file-hook #'inhibit-anaconda-remote)
  )


;;; Jupyter
(me/add-eager-package "python" 'jupyter)
(use-package! jupyter
  :defer t
  :config
  (setq jupyter-eval-short-result-max-lines 5)
  (defun jupyter-connect-name (filename)
    "Connect to a jupyter kernel by its FILENAME."
    (interactive (list (completing-read "Connection file name: "
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

  ;; Evil operator
  (evil-define-operator evil-jupyter-eval-region (beg end)
    "Send selection to jupyter kernel"
    :move-point nil
    (interactive "<r>")
    (jupyter-eval-region beg end))

  ;; Jupyter kb
  (map! :map jupyter-repl-interaction-mode-map "M-i" nil)
  (map! :map python-mode-map
        :localleader
        (:prefix ("r" . "run")
         :desc "Connect to kernel" "k" #'jupyter-connect-name
         :desc "Run line/region" "l" #'jupyter-eval-line-or-region
         :desc "Run" "r" #'evil-jupyter-eval-region
         :desc "Run string" "s" #'jupyter-eval-string-command
         :desc "Run cell" "c" #'jupyter-eval-cell
         :desc "Run buffer" "b" #'jupyter-eval-buffer
         :desc "Interrupt kernel" "i" #'jupyter-interrupt-kernel
         )))


;; ;;; Faces
;; (defface font-lock-ds-arguments-face
;;   '((t :inherit font-lock-doc-face
;;        :slant normal)) "Face for docstring arguments.")

;; (font-lock-add-keywords 'python-mode
;;                         '(("[ ^:]*:param \\([a-zA-Z0-9_^:]*\\):" 1 "font-lock-ds-arguments-face" t)))
