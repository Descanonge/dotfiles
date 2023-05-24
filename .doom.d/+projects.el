
(defun p:projects (&rest projects)
  (flatten-list projects))

(defun p:grp (dir &rest args)
  (mapcar (lambda (it) (concat dir "/" it)) (flatten-list args)))


(defcustom me/projects nil
  "List of my projects."
  :group 'me :type '(repeat string))

(setq-default me/projects (p:projects
 "~/.scripts"
 (p:grp "~/")
 (p:grp "~/Documents"
        (p:grp "Libraries"
               (p:grp "Python"
                      '("filefinder"
                        "cf-xarray"
                        "xarray-histogram"
                        "MyPack"
                        "Tol_colors"
                        "Tomate"))
               (p:grp "Web"
                      '("VisibleEartHome")))
        (p:grp "Work"
               '("Article_methodo"
                 "Fronts"
                 "Soutenance"
                 "Thèse"))
        (p:grp "Applications"
               '("dateloop"
                 "pyPalet"
                 "FilmVoter"
                 "notmuch-notifier@cinnamon.org"
                 "JS/Bot-ulus"))
        (p:grp "Websites"
               '("clementhaeck.com"
                 "jzargo")))
 "/sshx:spirit:/home/chaeck/Fronts"))

(setq projectile-known-projects me/projects)
