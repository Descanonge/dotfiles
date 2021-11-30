;;; .doom.d/config.el --- Config

;;; General info
(setq user-full-name "Clément Haëck"
      user-mail-address "clement.haeck@posteo.net")

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
