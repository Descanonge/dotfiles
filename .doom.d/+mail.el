;;; +mail.el -*- lexical-binding: t; -*-


(setq message-signature
      (concat "Clément Haëck\n"
              "Laboratoire d'Océanographie et du Climat:\n"
              " Expérimentations et Approches Numériques\n"
              "clement.haeck@locean.ipsl.fr | +33-(0)6 75 50 96 73\n"
              "Github <https://github.com/Descanonge>\n"
              "Gitlab <https://gitlab.in2p3.fr/clement.haeck>\n"))

(setq message-auto-save-directory "/tmp")

(defun notmuch-accounts-list ()
  (mapcar (lambda (m) (car m)) notmuch-accounts))

(defun notmuch-reply-find-sender-account (tags)
  "Find sender account from TAGS."
  (or (car (seq-intersection tags (notmuch-accounts-list)))
      (car (car notmuch-accounts))))

(defun notmuch-identity-from-account (account)
  "Return name + address for ACCOUNT."
  (message-make-from user-full-name
                     (plist-get (cdr (assoc account notmuch-accounts)) 'address)))

(defun notmuch-get-fcc (tags account)
  "Get Fcc on reply."
  (mapconcat 'identity
             (concatenate 'list
                          (list (concat account "/"
                                        (plist-get (cdr (assoc account notmuch-accounts))
                                                   'sent)))
                          '("sent")
                          tags)
             " +"))

;;; MAIL
(use-package! notmuch
  :config
  (defvar notmuch-accounts
    '(("posteo" . (address "clement.haeck@posteo.net" sent "Sent"))
      ("locean" . (address "clement.haeck@locean.ipsl.fr" sent "Sent"))
      ("orange" . (address "clement-haeck@orange.fr" sent "OUTBOX"))
      ("ens" . (address "clement.haeck@ens-paris-saclay.fr" sent "Sent"))))
  (defvar notmuch-reply-tags-remove
    '("inbox" "atch" "unread"))

  (setq notmuch-wash-signature-lines-max 0)

  (defun notmuch-tag-marked-messages (tag-changes &optional beg end)
    "Apply TAG-CHANGES to marked messages"
    (interactive (notmuch-search-interactive-tag-changes))
    (setq beg (point-min))
    (setq end (point-max))
    (setq tag-changes (append tag-changes '("-mark")))
    (let ((search-string "tag:mark"))
      (notmuch-tag search-string tag-changes)
      ;; (notmuch-search-foreach-result beg end
      ;;   (lambda (pos)
      ;;     (notmuch-search-set-tags
      ;;      (notmuch-update-tags (notmuch-search-get-tags pos) tag-changes)
      ;;      pos)))
      ))

  (map! :map notmuch-tree-mode-map
        "Q" #'(lambda () (interactive) (notmuch-tree-quit t))
        :map notmuch-search-mode-map
        :n "m" #'(lambda () "Mark message for batch tag"
                   (interactive)
                   (evil-collection-notmuch-toggle-tag "mark" "search" 'notmuch-search-next-thread))
        :n "M" #'notmuch-tag-marked-messages)


  ;; (add-hook! 'notmuch-show-hook #'(lambda () (interactive) (visual-fill-column-mode)
  )

(after! notmuch
  (defun +notmuch/compose ()
    "Compose new mail"
    (interactive)
    (let* ((account (completing-read "Account: " (notmuch-accounts-list)))
           (fcc (notmuch-get-fcc (list account) account)))
      (notmuch-mua-mail nil nil
                        (list (cons 'From (notmuch-identity-from-account account))
                              (cons 'Fcc fcc)))))
  )

(setq sendmail-program "/usr/bin/msmtp"
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from"); , "--read-recipients")
        message-send-mail-function #'message-send-mail-with-sendmail)

(defun notmuch-mua-reply (query-string &optional sender reply-all)
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
           (tags (seq-difference (plist-get original :tags) notmuch-reply-tags-remove))
           (account (notmuch-reply-find-sender-account tags)))
      (print account)
      (plist-put reply-headers :From (notmuch-identity-from-account account))
      (plist-put reply-headers :Fcc (notmuch-get-fcc tags account))
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
                (notmuch-show-indent-multipart nil))
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
