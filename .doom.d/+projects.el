
(defun p:projects (&rest projects)
  (flatten-list projects))

(defun p:grp (dir &rest args)
  (mapcar (lambda (it) (concat dir "/" it)) (flatten-list args)))

(setq projectile-projects (p:projects
 "~/.scripts"
 (p:grp "~/")
 (p:grp "~/Documents"
        (p:grp "Libraries"
               (p:grp "Python"
                      '("filefinder"
                        "MyPack"
                        "Tol_colors"
                        "Tomate"))
               (p:grp "Web"
                      '("VisibleEartHome"
                        "Pinako")))
        (p:grp "Work"
               '("Fronts"
                 "Thèse"))
        (p:grp "Applications"
               '("dateloop"
                 "pyPalet"
                 "FilmVoter"
                 "notmuch-notifier@cinnamon.org"
                 "JS/Bot-ulus")))
 "/sshx:ciclad:/home/chaeck/Fronts"))
