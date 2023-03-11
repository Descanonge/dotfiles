;;; +flycheck.el -*- lexical-binding: t; -*-


;;; Flycheck
(use-package! flycheck
  :preface
  (defvar-local flycheck-local-checkers nil)
  (defun +flycheck-checker-get(fn checker property)
    (or (alist-get property (alist-get checker flycheck-local-checkers))
        (funcall fn checker property)))
  (advice-add 'flycheck-checker-get :around '+flycheck-checker-get)

  :init
  ;; Set flycheck to check at save
  (setq! flycheck-check-syntax-automatically '(mode-enabled save))

  (setq flycheck-disabled-checkers '(python-mypy python-pycompile python-pylint
                                     emacs-lisp-checkdoc))

  :config
  (setq flycheck-gfortran-language-standard "f2008")
  (add-to-list 'flycheck-gfortran-include-path "/usr/include"))

(defun +default/diagnostics (&rest arg)
  "List diagnostics for the current buffer/project.
If the the vertico and lsp modules are active, list lsp diagnostics for the
current project. Otherwise list them for the current buffer.

Redefined from modules/config/default/autoload/default.el so that
consult-lsp-diagnostics does not have priority above flycheck.
"
  (interactive)
  (cond ((and (modulep! :checkers syntax)
              (bound-and-true-p flycheck-mode))
         (if (modulep! :completion vertico)
             (consult-flycheck)
           (flycheck-list-errors)))
        ((and (modulep! :completion vertico)
              (modulep! :tools lsp)
              (bound-and-true-p lsp-mode))
         (consult-lsp-diagnostics arg))
        ((bound-and-true-p flymake-mode)
         (if (modulep! :completion vertico)
             (consult-flymake)
           (flymake-show-diagnostics-buffer)))
        (t
         (user-error "No diagnostics backend detected. Enable flycheck or \
flymake, or set up lsp-mode if applicable (see :lang lsp)"))))
