;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; (package! zotxt)

;; (package! org-caldav)


;; Python
(package! python-cell)

(package! doom-themes)

(package! zeal-at-point)

(package! liquid-mode
  :recipe (:host github
           :repo "alesguzik/liquid-mode"
           :files ("*.el")))

(package! parrot)

(package! web-mode)
(package! zotxt)

(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))
;; When using org-roam via the `+roam` flag
(unpin! org-roam company-org-roam)
;; When using bibtex-completion via the `biblio` module
(unpin! bibtex-completion helm-bibtex ivy-bibtex)

(package! org-ref)

(package! company-bibtex)
