;;; csquotes.el -*- lexical-binding: t; -*-

(require 'tex)
(require 'latex)

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(TeX-add-style-hook
 "csquotes"
 (lambda ()
   (setq font-latex-match-textual-keywords-local
         (--remove (member (car it)
                           '("enquote" "foreignquote" "hyphenquote"
                             "textquote" "textcquote"
                             "foreigntextquote" "foreigntextcquote"
                             "hyphentextquote" "hyphentextcquote"
                             "blockquote" "blockcquote"
                             "hyphenblockquote" "hyphenblockcquote"
                             "foreignblockquote" "foreignblockcquote"
                             "hybridblockquote" "hybridblockcquote"))
                   font-latex-match-textual-keywords-local)))
 :latex)

;;; csquotes.el ends here
