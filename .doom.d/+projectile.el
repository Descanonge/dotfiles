;;; +projectile.el -*- lexical-binding: t; -*-


;;; Projectile
(use-package! projectile
  :init
  (setq projectile-track-known-projects-automatically nil)
  (setq projectile-globally-ignored-file-suffixes
        '(".swp" ".png" ".jpg" ".avi" ".mp4" ".svg" ".mkv" ".xcf"
               ".pdf" ".dvi"
               ".pyc" ".pyo" ".pyd"
               ".o" ".so" ".a" ".exe" ".o.d")
        projectile-globally-ignored-files
        '()
        projectile-globally-ignored-directories
        '("__pycache__" "*.egg-info" ".git"
          ".jekyll-cache" "_build")
        projectile-sort-order 'default)

  (load! "+projects.el")

  :config
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-known-projects projectile-projects)
  (setq projectile-generic-command
        (lambda (_)
          (let (bin)
            (cond
             ((setq bin (cl-find-if (doom-rpartial #'executable-find t)
                                    '("fdfind" "fd")))
              (concat (format "%s . -0 -H -E .git --color=never --type file --type symlink --follow"
                              bin)
                      (cl-loop for dir in projectile-globally-ignored-directories
                               concat " -E "
                               concat (shell-quote-argument dir))))
             ((concat "find . -type f"
                      (cl-loop for dir in projectile-globally-ignored-directories
                               concat " -not -path "
                               concat (shell-quote-argument (format "*/%s/*" dir)))
                      " -printf '%P\\0'"))))))

  (defun projectile-project-name-function-remote (project-root)
    (let* ((dir (directory-file-name project-root))
           (name (file-name-nondirectory dir))
           (remote-p (file-remote-p dir 'host))
           (remote (if remote-p (format "@%s" remote-p))))
      (concat name remote)
      ))

  (setq projectile-project-name-function #'projectile-project-name-function-remote)
  )
