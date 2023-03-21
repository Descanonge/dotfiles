;;; +mail.el -*- lexical-binding: t; -*-

(defgroup me/mail nil
  "Mail related variables"
  :group 'me)

(use-package! message
  :defer t
  :config
  (setq message-fill-column nil))

(me/add-eager-package "notmuch" 'notmuch)
(use-package! notmuch
  :defer t
  :config
  (setq message-signature
        (concat "Clément Haëck - Doctorant\n"
                "Laboratoire d'Océanographie et du Climat:\n"
                " Expérimentations et Approches Numériques\n"
                "clement.haeck@locean.ipsl.fr | +33-(0)6 75 50 96 73\n"
                "Github <https://github.com/Descanonge>\n"
                "Gitlab <https://gitlab.in2p3.fr/clement.haeck>\n"))

  (setq message-auto-save-directory "/tmp"
        sendmail-program "/usr/bin/msmtp"
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from") ; , "--read-recipients")
        message-send-mail-function #'message-send-mail-with-sendmail)

  (defcustom me/notmuch-accounts
    '(("posteo" . (:address "clement.haeck@posteo.net" :sent "Sent"))
      ("locean" . (:address "clement.haeck@locean.ipsl.fr" :sent "Sent"))
      ("orange" . (:address "clement-haeck@orange.fr" :sent "OUTBOX"))
      ("ens" . (:address "clement.haeck@ens-paris-saclay.fr" :sent "Sent")))
    "Alist of mail accounts. Each element has the account name/designation
for key and a plist as value defining the :address and :sent properties."
    :type '(alist string (plist :key-value (:options (:address :sent)) :value-type string))
    :group 'me/mail)

  (defun me/notmuch-accounts-list ()
    "Return list of notmuch-accounts names."
    (mapcar #'car me/notmuch-accounts))

  (defun me/notmuch-reply-find-sender-account (tags)
    "Return account name corresponding to TAGS.
If no tag correspond to an account name, the first account is used."
    (or (car (seq-intersection tags (me/notmuch-accounts-list)))
        (car (car me/notmuch-accounts))))

  (defun me/notmuch-account-get-prop (account prop)
    "Return PROP property of ACCOUNT."
    (plist-get (alist-get account me/notmuch-accounts
                          (car (car me/notmuch-accounts))
                          nil #'string-equal)
               prop))

  (defun me/notmuch-identity-from-account (account)
    "Return name + address for ACCOUNT."
    (message-make-from user-full-name (me/notmuch-account-get-prop account :address)))

  (defun me/notmuch-get-fcc (tags account)
    "Get Fcc field for a reply.
Add the mail folder to put the message in (:sent property) and format the TAGS."
    (mapconcat #'identity
               (flatten-list (list(format "%s/%s" account (me/notmuch-account-get-prop
                                                           account :sent))
                                  tags "sent"))
               " +"))

  (defcustom me/notmuch-reply-tags-remove
    '("inbox" "attachment" "unread")
    "Tags to remove when replying to message."
    :group 'me/mail :type '(repeat string))

  (setq notmuch-wash-signature-lines-max 0
        +notmuch-sync-backend 'mbsync
        notmuch-search-result-format
        '(("date" . "%12s ")
          ("count" . "%-7s ")
          ("authors" . "%-30s ")
          ("subject" . "%-50s ")
          ("tags" . "(%s)")))

  (setq +notmuch-home-function (lambda () (notmuch-search "tag:inbox")))
  (defun notmuch-protect-inbox ()
    (if (string-equal notmuch-search-query-string "tag:inbox")
        (emacs-lock-mode 'kill)))
  (add-hook! 'notmuch-search-hook #'notmuch-protect-inbox)

  ;; Add current line highlight to notmuch
  (appendq! global-hl-line-modes '(notmuch-search-mode
                                   notmuch-tree-mode))

  (set-popup-rule! "^\\*notmuch*" :ignore t)
  (set-popup-rule! "^\\*subject:notmuch*" :ignore t)

  (defun me/notmuch-compose (&optional to)
    "Compose new mail.
Ask for account to use interactively.
If TO is specified, add it to the To: header."
    (interactive)
    (let* ((account (completing-read "Account: " (me/notmuch-accounts-list)))
           (fcc (me/notmuch-get-fcc (list account) account)))
      (notmuch-mua-mail to nil
                        (list (cons 'From (me/notmuch-identity-from-account account))
                              (cons 'Fcc fcc)))))

  (defun me/notmuch-toggle-mark ()
    "Mark message at point. (tag them +mark)"
    (interactive)
    (evil-collection-notmuch-toggle-tag "mark" "search" 'notmuch-search-next-thread))

  (defun me/notmuch-tag-marked-messages (tag-changes)
    "Apply TAG-CHANGES to marked messages"
    (interactive (notmuch-search-interactive-tag-changes))
    (let ((search-string "tag:mark"))
      (notmuch-tag search-string (append tag-changes "-mark"))))

  (defun evil-collection-notmuch-show-toggle-trashed () (interactive)
         (notmuch-show-tag '("-inbox"))
         (evil-collection-notmuch-toggle-tag "trashed" "show"))
  (defun evil-collection-notmuch-tree-toggle-trashed () (interactive)
         (notmuch-tree-tag '("-inbox"))
         (evil-collection-notmuch-toggle-tag "trashed" "tree"))
  (defun evil-collection-notmuch-search-toggle-trashed () (interactive)
         (notmuch-search-tag '("-inbox"))
         (evil-collection-notmuch-toggle-tag "trashed" "search" 'notmuch-search-next-thread))

  (after! evil-collection
    :config
    (evil-collection-define-key 'normal 'notmuch-show-mode-map
      "d" #'evil-collection-notmuch-show-toggle-trashed
      "D" #'evil-collection-notmuch-show-toggle-delete)
    (evil-collection-define-key 'normal 'notmuch-tree-mode-map
      "d" #'evil-collection-notmuch-tree-toggle-trashed
      "D" #'evil-collection-notmuch-tree-toggle-delete)
    (dolist (state '(normal visual))
      (evil-collection-define-key state 'notmuch-search-mode-map
        "d" #'evil-collection-notmuch-search-toggle-trashed
        "D" #'evil-collection-notmuch-search-toggle-delete)))

  (map! :localleader
        :map (notmuch-search-mode-map notmuch-tree-mode-map notmuch-show-mode-map)
        :desc "New mail" "c" #'me/notmuch-compose
        :desc "Show flagged" "f" #'(lambda () (interactive) (notmuch-search "tag:flagged"))
        :map notmuch-message-mode-map
        :desc "Attach file" :n "a" #'mml-attach-file)

  (map! :map notmuch-tree-mode-map
        :desc "Quit" "q" #'(lambda () (interactive) (notmuch-tree-quit t))
        :desc "Quit" "Q" #'(lambda () (interactive) (notmuch-tree-quit t))

        :map notmuch-search-mode-map
        :desc "Toggle mark" :n "m" #'me/notmuch-toggle-mark
        :desc "Tag marked" :n "M" #'me/notmuch-tag-marked-messages)

  (defun notmuch-search-show-thread (&optional elide-toggle)
    "Display the currently selected thread.

With a prefix argument, invert the default value of
`notmuch-show-only-matching-messages' when displaying the
thread.

Redefined to set buffer name as *notmuch:%s*
"
    (interactive "P")
    (let ((thread-id (notmuch-search-find-thread-id)))
      (if thread-id
          (notmuch-show thread-id
                        elide-toggle
                        (current-buffer)
                        notmuch-search-query-string
                        ;; Name the buffer based on the subject.
                        (format "*notmuch:%s*" (truncate-string-to-width
                                                (notmuch-search-find-subject)
                                                30 nil nil t)))
        (message "End of search results."))))

  (face-spec-set 'notmuch-crypto-decryption
                 '((t (:background nil :foreground "#b751b6"))))
  (face-spec-set 'notmuch-crypto-part-header
                 '((t (:inherit message-mml :background nil :foreground nil))))
  (face-spec-set 'notmuch-crypto-signature-bad
                 '((t (:inherit error :background nil :foreground nil))))
  (face-spec-set 'notmuch-crypto-signature-good
                 '((t (:inherit success :background nil :foreground nil))))
  (face-spec-set 'notmuch-crypto-signature-good-key
                 '((t (:inherit success :background nil :foreground nil))))
  (face-spec-set 'notmuch-crypto-signature-unknown
                 '((t (:inherit warning :background nil :foreground nil))))

  )

(use-package! notmuch-mua
  :after notmuch
  :config

  (defun notmuch-mua-reply (query-string &optional sender reply-all)
    "Redefined from notmuch.el so we automatically add From:
we remove some tags and add the account tag"
    (let ((args '("reply" "--format=sexp" "--format-version=4"))
          (process-crypto notmuch-show-process-crypto)
          reply
          original)
      (when process-crypto
        (setq args (append args '("--decrypt=true"))))
      (if reply-all
          (setq args (append args '("--reply-to=all")))
        (setq args (append args '("--reply-to=sender"))))
      (setq args (append args (list query-string)))
      ;; Get the reply object as SEXP, and parse it into an elisp object.
      (setq reply (apply #'notmuch-call-notmuch-sexp args))
      ;; Extract the original message to simplify the following code.
      (setq original (plist-get reply :original))
      ;; Extract the headers of both the reply and the original message.
      (let* ((original-headers (plist-get original :headers))
             (reply-headers (plist-get reply :reply-headers))
             ;; CHANGE: remove some tags, add account tag
             (tags (seq-difference (plist-get original :tags) me/notmuch-reply-tags-remove))
             (account (me/notmuch-reply-find-sender-account tags)))
        (plist-put reply-headers :From (me/notmuch-identity-from-account account))
        (plist-put reply-headers :Fcc (me/notmuch-get-fcc tags account))
        (let
            ;; Overlay the composition window on that being used to read
            ;; the original message.
            ((same-window-regexps '("\\*mail .*")))
          ;; We modify message-header-format-alist to get around
          ;; a bug in message.el.  See the comment above on
          ;; notmuch-mua-insert-references.
          (let ((message-header-format-alist
                 (cl-loop for pair in message-header-format-alist
                          if (eq (car pair) 'References)
                          collect (cons 'References
                                        (apply-partially
                                         'notmuch-mua-insert-references
                                         (cdr pair)))
                          else
                          collect pair)))
            (notmuch-mua-mail (plist-get reply-headers :To)
                              (notmuch-sanitize (plist-get reply-headers :Subject))
                              (notmuch-headers-plist-to-alist reply-headers)
                              nil (notmuch-mua-get-switch-function))))
        ;; Create a buffer-local queue for tag changes triggered when
        ;; sending the reply.
        (when notmuch-message-replied-tags
          (setq-local notmuch-message-queued-tag-changes
                      (list (cons query-string notmuch-message-replied-tags))))
        ;; Insert the message body - but put it in front of the signature
        ;; if one is present, and after any other content
        ;; message*setup-hooks may have added to the message body already.
        (save-restriction
          (message-goto-body)
          (narrow-to-region (point) (point-max))
          (goto-char (point-max))
          (if (re-search-backward message-signature-separator nil t)
              (when message-signature-insert-empty-line
                (forward-line -1))
            (goto-char (point-max))))
        (let ((from (plist-get original-headers :From))
              (date (plist-get original-headers :Date))
              (start (point)))
          ;; notmuch-mua-cite-function constructs a citation line based
          ;; on the From and Date headers of the original message, which
          ;; are assumed to be in the buffer.
          (insert "From: " from "\n")
          (insert "Date: " date "\n\n")
          (insert
           (with-temp-buffer
             (let
                 ;; Don't attempt to clean up messages, excerpt
                 ;; citations, etc. in the original message before
                 ;; quoting.
                 ((notmuch-show-insert-text/plain-hook nil)
                  ;; Don't omit long parts.
                  (notmuch-show-max-text-part-size 0)
                  ;; Insert headers for parts as appropriate for replying.
                  (notmuch-show-insert-header-p-function
                   notmuch-mua-reply-insert-header-p-function)
                  ;; Ensure that any encrypted parts are
                  ;; decrypted during the generation of the reply
                  ;; text.
                  (notmuch-show-process-crypto process-crypto)
                  ;; Don't indent multipart sub-parts.
                  (notmuch-show-indent-multipart nil)
                  ;; Stop certain mime types from being inlined
                  (mm-inline-override-types (notmuch--inline-override-types)))
               ;; We don't want sigstatus buttons (an information leak and usually wrong anyway).
               (cl-letf (((symbol-function 'notmuch-crypto-insert-sigstatus-button) #'ignore)
                         ((symbol-function 'notmuch-crypto-insert-encstatus-button) #'ignore))
                 (notmuch-show-insert-body original (plist-get original :body) 0)
                 (buffer-substring-no-properties (point-min) (point-max))))))
          (set-mark (point))
          (goto-char start)
          ;; Quote the original message according to the user's configured style.
          (funcall notmuch-mua-cite-function)))
      ;; Crypto processing based crypto content of the original message
      (when process-crypto
        (notmuch-mua-reply-crypto (plist-get original :body))))
    ;; Push mark right before signature, if any.
    (message-goto-signature)
    (unless (eobp)
      (end-of-line -1))
    (push-mark)
    (message-goto-body)
    (set-buffer-modified-p nil))


(defun notmuch-read-query (prompt)
  "Read a notmuch-query from the minibuffer with completion.

PROMPT is the string to prompt with.
Redefined from notmuch.el so it support completion of email addresses."
  (let* ((all-tags
          (mapcar (lambda (tag) (notmuch-escape-boolean-term tag))
                  (notmuch--process-lines notmuch-command "search" "--output=tags" "*")))
         (all-addr (notmuch-address-options ""))
         (completions
          (append (list "folder:" "path:" "thread:" "id:" "date:" "from:" "to:"
                        "subject:" "attachment:")
                  (mapcar (lambda (tag) (concat "tag:" tag)) all-tags)
                  (mapcar (lambda (tag) (concat "is:" tag)) all-tags)
                  ;; CHANGE: add address completion
                  (mapcar (lambda (addr) (concat "from:" addr)) all-addr)
                  (mapcar (lambda (addr) (concat "to:" addr)) all-addr)
                  (mapcar (lambda (mimetype) (concat "mimetype:" mimetype))
                          (mailcap-mime-types))))
         (keymap (copy-keymap minibuffer-local-map))
         (current-query (cl-case major-mode
                          (notmuch-search-mode (notmuch-search-get-query))
                          (notmuch-show-mode (notmuch-show-get-query))
                          (notmuch-tree-mode (notmuch-tree-get-query))))
         (minibuffer-completion-table
          (completion-table-dynamic
           (lambda (string)
             ;; Generate a list of possible completions for the current input.
             (cond
              ;; This ugly regexp is used to get the last word of the input
              ;; possibly preceded by a '('.
              ((string-match "\\(^\\|.* (?\\)\\([^ ]*\\)$" string)
               (mapcar (lambda (compl)
                         (concat (match-string-no-properties 1 string) compl))
                       (all-completions (match-string-no-properties 2 string)
                                        completions)))
              (t (list string)))))))
    ;; This was simpler than convincing completing-read to accept spaces:
    (define-key keymap (kbd "TAB") 'minibuffer-complete)
    (let ((history-delete-duplicates t))
      (read-from-minibuffer prompt nil keymap nil
                            'notmuch-search-history current-query nil))))
)
