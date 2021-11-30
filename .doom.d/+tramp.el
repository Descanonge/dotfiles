;;; +tramp.el -*- lexical-binding: t; -*-

;; Add path for git on @ciclad
(after! tramp
  ;; (setq tramp-sh-extra-args '(("/bash\\'" . "-norc")))
  (setenv "SHELL" "/bin/bash")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; (add-to-list 'tramp-remote-path "/opt/git/2.7.4/bin")
  (add-to-list 'tramp-methods
               '("yadm"
                 (tramp-login-program "yadm")
                 (tramp-login-args (("enter")))
                 (tramp-login-env (("SHELL") ("/bin/sh")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-args ("-c"))))
  (map! :leader "g." (cmd! (magit-status  "/yadm::")))

  (map! :leader
        (:prefix ("g," . "set exec")
         :desc "Local" "l" #'(lambda () (interactive) (setq magit-git-executable "git"))
         :desc "Ciclad" "c" #'(lambda () (interactive) (setq magit-git-executable "/opt/git/2.7.4/bin/git"))
         )))
