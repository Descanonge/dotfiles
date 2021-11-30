;;; +flycheck.el -*- lexical-binding: t; -*-

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
                                                         emacs-lisp-checkdoc))

  :config
  (setq flycheck-gfortran-language-standard "f2008")
  (add-to-list 'flycheck-gfortran-include-path "/usr/include"))
