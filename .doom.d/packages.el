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
