;;; +cpp.el -*- lexical-binding: t; -*-

;;; C++
(after! flycheck
  (setq flycheck-cppcheck-include-path '("./include" "../include")))

;; (set-eglot-client! 'c++-mode '("clangd-9" "-j=3" "--clang-tidy"))
;; (set-eglot-client! 'c++-mode '("ccls" "--init={\"index\": {\"threads\": 3}}"))

(defun chaeck/find-projectile-project (dir)
  (let ((probe (locate-dominating-file dir ".projectile")))
    (when probe (cons 'transient probe))))

(add-hook 'project-find-functions 'chaeck/find-projectile-project 'append)
(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"))
(setq lsp-ui-doc-enable nil)
(setq lsp-signature-render-documentation nil)
(after! lsp-clangd (set-lsp-priority! 'clangd 2))
