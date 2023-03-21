;;; .doom.d/config.el --- Config

;;; General info
(setq user-full-name "Clément Haëck"
      user-mail-address "clement.haeck@posteo.net"
      doom-user-dir "/home/clement/.doom.d/")

(defgroup me nil
  "Personal customization variables.")

(setq use-package-verbose t)

;;; Personnal bindings
;; which override local keymaps
(defvar me/local-intercept-mode-map
  (make-sparse-keymap)
  "Keymap for minor mode which should override/intercept anything else.")
(define-minor-mode me/local-intercept-mode
  "A minor mode to override local keymaps."
  :global t)

;; Safety for minibuffer
(defun me/local-intercept-turnoff ()
  (me/local-intercept-mode 0))
;; (add-hook 'minibuffer-setup-hook 'me/local-intercept-turnoff)

;; Make the keymap an intercept one, so it is before
;; minor modes keymaps. For global and 3 states
(dolist (state '(normal visual insert))
  (evil-make-intercept-map
   (evil-get-auxiliary-keymap me/local-intercept-mode-map state t t)
   state))
(evil-make-intercept-map me/local-intercept-mode-map)


;; Alias for map! command
(add-to-list 'general-keymap-aliases
             '(local-intercept . me/local-intercept-mode-map))

;;; Eager loading
;; maybe a misnomer, its for package that I load at the end of
;; my configuration.
(defcustom me/eager-load-groups nil
  "Alist of groups of package to eager load or not."
  :group 'me
  :type '(alist :key-type string :value-type bool))

(setq me/eager-load-groups
      '(("python" . t)
        ("lsp" . t)
        ("notmuch" . t)
        ("magit" . t)
        ("tex" . t)
        ("org" . t)
        ("yas" . t)))

(defun me/eager-loadp (group)
  "True if GROUP is to be lazy-loaded."
  (assoc-string group me/eager-load-groups))

(defvar me/eager-load-packages nil
  "Packages to require, regrouped.")

(require 'dash)
(defun me/add-eager-package (groups packages)
  "Make PACKAGES to be eager loaded at the end of config.el,
only if GROUPS are set to be eager loaded."
  (unless (listp packages)
    (setq packages (list packages)))
  (unless (listp groups)
    (setq groups (list groups)))
  (when (-all-p #'me/eager-loadp groups)
    (dolist (pack packages)
      (add-to-list 'me/eager-load-packages pack)))
  )

(defun me/eager-load ()
  "Load packages that were marked as eager loaded."
  (dolist (pack (reverse me/eager-load-packages))
    (message "Eager loading of %s" pack)
    (require pack))
  )

;;; Fill Column indicator
(face-spec-set 'fill-column-indicator
               '((t (:foreground "light gray"))))

;;; Evil
(load! "+evil.el")
(load! "+bindings.el")


;;; Preferred viewers
(after! mailcap
  (add-to-list 'mailcap-user-mime-data
               '((viewer . "evince %s")
                 (type . "application/pdf"))))

(me/add-eager-package "yas" 'yasnippet)
(use-package! yasnippet
  :config
  ;; disable automatic expansion, I don't use it
  (yas-global-mode -1))

;;; Magit
(me/add-eager-package "magit" 'magit)
(use-package! magit
  :defer t
  :config
  ;; Find git rather than prescribe its location. Useful for tramp
  (setq magit-git-executable "git")
  ;; Scroll in magit buffers
  (map! (:map magit-mode-map
         :prefix "z"
         :nv "t" #'evil-scroll-line-to-top)

        :leader
        :desc "Diff" "gd" #'magit-diff))

;;; Completion
(use-package! vertico
  :config
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
        '((find-file (vertico-sort-function . vertico-sort-alpha))
          (projectile-find-file (vertico-sort-function . vertico-sort-history-alpha))))
  (map! :map doom-leader-map
        "·" :desc "Repeat search" #'vertico-repeat-select
        "'" :desc "Repeat last search" #'vertico-repeat-last)
  )

(use-package! orderless
  :defer t
  :config
  (setq orderless-component-separator #'orderless-escapable-split-on-space)
  )

;;; Man pages
(use-package! man
  :defer t
  :config
  (map! :map doom-leader-open-map
        :desc "Man pages" "?" (lambda () (interactive) (call-interactively #'man))))


;;; Direnv
(use-package! direnv
  :defer t
  :config
  (setq direnv-always-show-summary nil))

;;; RST
(use-package! rst
  :defer t
  :config
  (map! :map rst-mode-map
        "]g" #'rst-forward-section
        "[g" #'rst-backward-section))

;;; Julia
(after! julia
  (setq lsp-julia-default-environment "~/.julia/environments/v1.8"))

;;; Calendar
(map! :map doom-leader-open-map
      "c" #'=calendar)

(load! "+theme.el")
(load! "+projectile.el")
(load! "+mail.el")
(load! "+check.el")
(load! "+spell.el")

;;; Langs
(load! "+org.el")
(load! "+python.el")
(load! "+cpp.el")
(load! "+tex.el")

(load! "+dashboard.el")
(load! "+tramp.el")

;;; Load packages to be eager loaded
(me/add-eager-package "lsp" 'lsp)
(me/eager-load)

(me/local-intercept-mode t)

;;; Server
(server-start)
