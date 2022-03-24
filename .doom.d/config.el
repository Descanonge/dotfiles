;;; .doom.d/config.el --- Config

;;; General info
(setq user-full-name "Clément Haëck"
      user-mail-address "clement.haeck@posteo.net")

(setq parse-time-months
      '(("jan" . 1) ("feb" . 2) ("mar" . 3) ("apr" . 4) ("may" . 5) ("jun" . 6)
        ("jul" . 7) ("aug" . 8) ("sep" . 9) ("oct" . 10) ("nov" . 11) ("dec" . 12)
        ("january" . 1) ("february" . 2) ("march" . 3) ("april" . 4) ("june" . 6)
        ("july" . 7) ("august" . 8) ("september" . 9) ("october" . 10)
        ("november" . 11) ("december" . 12)
        ("fev" . 2) ("avr" . 4) ("mai" . 5) ("aou" . 8)
        ("janvier" . 1) ("février" . 2) ("mars" . 3) ("avril" . 4) ("juin" . 6)
        ("juillet" . 7) ("aout" . 8) ("septembre" . 9) ("octobre" . 10)
        ("novembre" . 11) ("décembre" . 12))
      parse-time-weekdays
      '(("sun" . 0) ("mon" . 1) ("tue" . 2) ("wed" . 3)
        ("thu" . 4) ("fri" . 5) ("sat" . 6)
        ("sunday" . 0) ("monday" . 1) ("tuesday" . 2) ("wednesday" . 3)
        ("thursday" . 4) ("friday" . 5) ("saturday" . 6)
        ("dim" . 0) ("lun" . 1) ("mar" . 2) ("mer" . 3)
        ("jeu" . 4) ("ven" . 5) ("sam" . 6)
        ("dimanche" . 0) ("lundi" . 1) ("mardi" . 2) ("mercredi" . 3)
        ("jeudi" . 4) ("vendredi" . 5) ("samedi" . 6)))

;;; Evil settings and general keybindings
(load! "+evil.el")

(after! ivy
 (setq ivy-extra-directories nil))

;;; Preferred viewers
(use-package! mailcap
  :config
  (add-to-list 'mailcap-user-mime-data
        '((viewer . "evince %s")
          (type . "application/pdf"))))

;;; Magit
(after! magit
  ;; Find git rather than prescribe its location. Useful for tramp
  (setq magit-git-executable "git")
  ;; Scroll in magit buffers
  (map! (:map magit-mode-map
          :prefix "z"
          :nv "t" #'evil-scroll-line-to-top)

        ;; Unmap for movement
        (:map magit-mode-map
          "M-n" nil)

        :leader
        :desc "Diff" "gd" #'magit-diff))


;;; Completion
(after! vertico
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
        '((find-file (vertico-sort-function . vertico-sort-alpha))
          (projectile-find-file (vertico-sort-function . vertico-sort-history-alpha))))
  )


;;; Direnv
(after! direnv
  (setq direnv-always-show-summary nil))

;;; RST
(use-package! rst
  :config
  (map! :map rst-mode-map
        "]g" #'rst-forward-section
        "[g" #'rst-backward-section))

(use-package! liquid-mode)

(load! "+theme.el")
(load! "+projectile.el")
(load! "+mail.el")
(load! "+flycheck.el")

;;; Langs
(load! "+org.el")
(load! "+python.el")
(load! "+cpp.el")
(load! "+tex.el")

(load! "+dashboard.el")
(load! "+tramp.el")

;;; Server
(server-start)
