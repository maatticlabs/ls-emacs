;-*- coding: iso-8859-1; -*-

;;;;unix_ms_filename_correspondency lse-mode-alist:el lse_mode:el
;;;; Copyright (C) 1994-2007 Mag. Christian Tanzer. All rights reserved.
;;;; Glasauergasse 32, A--1130 Wien, Austria. tanzer.co.at

;;;; This file is part of LS-Emacs, a package built on top of GNU Emacs.
;;;;
;;;; Like GNU Emacs, LS-Emacs is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License as published
;;;; by the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;;
;;;; Like GNU Emacs, LS-Emacs is distributed in the hope that it will be
;;;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;;;++
;;;; Name
;;;;    lse-mode-alist
;;;;
;;;; Purpose
;;;;    Define mapping from file-name patterns to major modes
;;;;
;;;; Revision Dates
;;;;    14-Jun-1994 (CT) Creation (of comment)
;;;;    14-Jun-1994 (CT) Definition of lse modes moved into lse-language-*.lse
;;;;    24-Sep-1994 (CT) Use c++-mode instead of c-mode for .h-files
;;;;    28-Dec-1994 (CT) aufwandserfassung functions moved in here
;;;;    17-May-1995 (CT) lse-mail-mode added
;;;;    25-May-1995 (CT) lse-mail-mode moved to lse-language-mail.lse
;;;;    19-Aug-1995 (CT) lse-bib-kartei:check-isbn added
;;;;    24-Aug-1995 (CT) Error in lse-bib-kartei:check-isbn corrected
;;;;                     (get-buffer-create used, otherwise a non-existing
;;;;                     `*Help*' buffer results in an error!)
;;;;    17-Sep-1995 (CT) Use c++-mode instead of c-mode for .y- and .l-files
;;;;    15-Oct-1995 (CT) lse-check-isbn added
;;;;     4-Mar-1996 (CT) c++-mode-setup added (factored from c++-mode)
;;;;     4-Apr-1996 (CT) Pattern for lse-buchhaltung-mode changed
;;;;    10-Jun-1996 (CT) Set `mode-name' Aufwand
;;;;     4-Jul-1996 (CT) Use lse-aufwandserfassung-keymap
;;;;     7-Oct-1996 (CT) Removed conditional code for Emacs version 18
;;;;                     Replaced lse-self-insert-pair by lse-insert-dquotes
;;;;     7-Oct-1996 (CT) `lse-insert' replaced by `insert'
;;;;    11-Oct-1996 (CT) Use `define-derived-mode' to define modes with their
;;;;                     own keymap (instead of trickery with
;;;;                     lse-aufwandserfassung-keymap)
;;;;     2-Mar-1998 (CT) lse-python-mode added
;;;;     3-Apr-1998 (CT) Font-Locking for olt-macros added
;;;;    14-Apr-1998 (CT) Keywords added to olt font locking
;;;;    26-Jul-1998 (CT) Added `kw[0-9][0-9]' to pattern for
;;;;                     lse-aufwandserfassung-*mode
;;;;     1-May-1999 (CT) lse-kartei-mode added
;;;;     3-Aug-1999 (CT) `latex2e' added
;;;;    19-Jan-2001 (CT) `lse-aufwand:show-total' tested with
;;;;                     `weekly_effort.py'
;;;;    24-Mar-2002 (CT) Use `lse-c-mode` instead of `c-mode`
;;;;    11-Sep-2002 (CT) Use `lse-rest-mode` for `.txt` files
;;;;    28-Oct-2002 (CT) /ercos and /xbw added to `auto-mode-alist`
;;;;    21-Nov-2002 (CT) Use `python-mode` instead of `lse-python-mode`
;;;;     9-Mar-2003 (CT) `lse-diary-mode` added
;;;;     9-Mar-2003 (CT) Locally unset `{`, `[`, and `(` for LaTeX
;;;;     9-Mar-2003 (CT) Use `lse-key-template-tab-l` for
;;;;                     lse-aufwandserfassung-mode and lse-kartei-mode
;;;;    12-Oct-2007 (CT) Added keybindings for `lse-cal:diary:next-day` and
;;;;                     `lse-cal:diary:prev-day` to `lse-diary-mode`
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-mode-alist)

;;;;
;;;; copied from files.el and changed according to lse-needs
;;;;
(setq auto-mode-alist
  '(
     ("^/swing.*proj.*aufwand/.*kw[0-9][0-9]\\.dat"            . lse-aufwandserfassung-project-mode)
     ("^/swing.*/pm/aufwand/.*kw[0-9][0-9]\\.dat"              . lse-aufwandserfassung-project-mode)
     ("^/ercos.*/pm/aufwand/.*kw[0-9][0-9]\\.dat"              . lse-aufwandserfassung-project-mode); 28-Oct-2002
     ("^/xbw.*/pm/aufwand/.*kw[0-9][0-9]\\.dat"                . lse-aufwandserfassung-project-mode); 28-Oct-2002
     ("^/swing.*aufwand\\(serfassung\\)?/.*kw[0-9][0-9]\\.dat" . lse-aufwandserfassung-mode)
     ("^/tttech/company/time/.*kw[0-9][0-9]\\.dat"             . lse-aufwandserfassung-mode)
     ("\\.diary"                           . lse-diary-mode);  9-Mar-2003
     ("^/swing.*awk/"                      . awk-mode)
     ("\\.awk\\'"                          . awk-mode)
     ("^/swing.*backup/"                   . lse-bash-mode)
     ("^/swing.*com/"                      . lse-bash-mode)
     ("^/swing.*login/"                    . lse-bash-mode)
     ("^/swing.*uaf/"                      . lse-bash-mode)
     ("^/swing.*finanzplanung/"            . lse-finanzplanung-mode)
     ("^/swing.*buchhaltung/.*\\.dat"      . lse-buchhaltung-mode); 4-Apr-1996 .dat
     ("^/swing/kassabuch/199[0-9]/"        . lse-kassabuch-mode)
     ("\\.lse\\'"                          . lse-lse-mode)
     ("\\.py\\'"                           . python-mode); 21-Nov-2002 ;  2-Mar-1998
     ("^/swing.*perl/"                     . perl-mode)
     ("\\.txt\\'"                          . lse-rest-mode); 11-Sep-2002 lse-rest-mode; 9-Jul-1997 lse-text-mode
     ("\\.text\\'"                         . lse-text-mode);  9-Jul-1997 lse-
     ("\\.c\\'"                            . lse-c-mode)
     ("\\.h\\'"                            . c++-mode); 24-Sep-1994 c++ but c
     ("\\.y\\'"                            . c++-mode); 17-Sep-1995 instead c
     ("\\.l\\'"                            . c++-mode); 17-Sep-1995 instead c
     ("\\.lex\\'"                          . c++-mode); 17-Sep-1995 instead c
     ("\\.tex\\'"                          . LaTeX-mode)
     ("^/swing.*stydoc/"                   . LaTeX-mode)
     ("^/swing.*latex2e/"                  . LaTeX-mode); 3-Aug-1999
     ("\\.el\\'"                           . emacs-lisp-mode)
     ("\\.sed\\'"                          . lse-sed-mode)
     ("\\.f\\'"                            . fortran-mode)
     ("\\.for\\'"                          . fortran-mode)
     ("\\.cc\\'"                           . c++-mode)
     ("\\.C\\'"                            . c++-mode)
     ("\\.tin\\'"                          . c++-mode); 17-Jun-1997
     ("\\.inc\\'"                          . c++-mode);  2-Sep-1997
;;;  ("\\.mk\\'"                           . makefile-mode)
;;;  ("[Mm]akefile"                        . makefile-mode)
     ("ChangeLog\\'"                       . change-log-mode)
     ("ChangeLog.[0-9]+\\'"                . change-log-mode)
     ("\\$CHANGE_LOG\\$\\.TXT"             . change-log-mode)
     ("\\.sty\\'"                          . LaTeX-mode)
     ("\\.texinfo\\'"                      . texinfo-mode)
     ("\\.texi\\'"                         . texinfo-mode)
     ("\\.tar\\'"                          . tar-mode)
     ("\\.sgm\\'"                          . sgml-mode)
     ("\\.sgml\\'"                         . sgml-mode)
     ("\\.dtd\\'"                          . sgml-mode)
     ("[]>:/]\\..*emacs\\'"                . emacs-lisp-mode)
   )
)

;;;+
;;; functions used by a number of languages
;;;-
(defvar                      lse-tex-mode:keys@defined nil)
(make-variable-buffer-local 'lse-tex-mode:keys@defined)

(defun lse-insert-comma-tex ()
  (interactive)
  (if (equal (preceding-char) ?\\)
      (insert ",")
    (lse-insert+blank-maybe ",")
  )
; lse-insert-comma-tex
)

(defun lse-latex-mode:define-keys ()
  (interactive)
  (setq indent-line-function 'lse-indent-line)
  (if lse-tex-mode:keys@defined
      t
    (setq lse-tex-mode:keys@defined t)
    (if (current-local-map)
        (local-set-key [gold ?\C-i]
                       (lookup-key (current-local-map) "\C-i")
        )
    )
    (local-set-key [gold ?\"] 'lse-insert-dquotes); 7-Oct-1996 ; 'self-insert-pair
    (local-set-key "\""       'self-insert-command)
    (local-set-key ","        'lse-insert-comma-tex)
    (local-unset-key "\n")
    (local-unset-key "\C-j")
    (local-unset-key "{");  9-Mar-2003
    (local-unset-key "[");  9-Mar-2003
    (local-unset-key "(");  9-Mar-2003
  )
; lse-latex-mode:define-keys
)
;;;+
;;; functions used by aufwandserfassung languages
;;;-
(defvar lse-aufwand:summary-buffer " $aufwands total$")

(defun lse-aufwand:show-total ()
  (interactive)
  (with-output-to-temp-buffer lse-aufwand:summary-buffer
    (call-process-region (point-min) (point-max)
                         "/swing/perl/show_week_aufwand_summary"
                         ; "/swing/python/weekly_effort.py" ;
                         nil lse-aufwand:summary-buffer nil
                         ; "-s" "-D"
    )
  )
; lse-aufwand:show-total
)

;;; 11-Oct-1996
(define-derived-mode lse-aufwandserfassung-mode text-mode "Aufwand"
  "Major mode for aufwandserfassung.
\\{lse-aufwandserfassung-mode-map}"
  (lse-language:use "aufwandserfassung")
  (lse-key-template-tab-l);  9-Mar-2003
)
;;; 11-Oct-1996
;;;  9-Mar-2003 ; (define-key lse-aufwandserfassung-mode-map [tab]    'lse-expand-or-goto-next)
;;; (define-key lse-aufwandserfassung-mode-map [?\C-i] 'lse-expand-or-goto-next)
;;; 25-Feb-1998
(define-key lse-aufwandserfassung-mode-map [return] 'lse-goto-next-fill-in)

;;; 11-Oct-1996
(define-derived-mode lse-aufwandserfassung-project-mode
                     lse-aufwandserfassung-mode "P-Aufwand"
  "Major mode for project aufwandserfassung.
\\{lse-aufwandserfassung-project-mode-map}"
  (lse-language:use "aufwandserfassung-project")
)

;;;  9-Mar-2003
(define-derived-mode lse-diary-mode text-mode "Diary"
  "Major mode for diary"
  (lse-language:use "diary")
  (lse-key-template-tab-l); 10-Mar-2003
  (local-set-key [M-home]    'lse-cal:switch-diary)
  (local-set-key [s-home]    'lse-cal:switch-diary)
  (local-set-key [red right] 'lse-cal:diary:next-day)
  (local-set-key [red left]  'lse-cal:diary:prev-day)
)

;;;  1-May-1999
(define-derived-mode lse-kartei-mode text-mode "Kartei"
  "Major mode for kartei.
\\{lse-kartei-mode-map}"
  (message "kartei-mode")
  (lse-key-template-tab-l);  9-Mar-2003
)
;;;  1-May-1999
;;;  9-Mar-2003 ; (define-key lse-kartei-mode-map [tab]    'lse-expand-or-goto-next)

;;;+
;;; Function for checking ISBN numbers of bib kartei entries
;;;-
(defun lse-bib-kartei:check-isbn ()
  (interactive)
  (let* ((range      (lse-fill-in:range  lse_current_fill-in))
         (head-pos   (lse-range:head-pos range))
         (tail-pos   (lse-range:tail-pos range))
        )
    (swing-kartei:bib:check-isbn
        (buffer-substring-no-properties head-pos tail-pos)
    )
  )
; lse-bib-kartei:check-isbn
)

;;; 15-Oct-1995
(defun lse-check-isbn ()
  "Check ISBN contained in selected range or current word"
  (interactive)
  (if (lse-tpu:position-of-select-mark)
      (swing-kartei:bib:check-isbn
          (buffer-substring-no-properties
               (lse-tpu:selection-head-pos) (lse-tpu:selection-tail-pos)
          )
      )
    (let ((cwr (lse-tpu:current-word-range))
         )
      (if cwr
          (swing-kartei:bib:check-isbn
               (buffer-substring-no-properties
                   (lse-range:head cwr) (lse-range:tail cwr)
               )
          )
      )
    )
  )
; lse-check-isbn
)

;;;  4-Mar-1996
(if lse-emacs20-p
    t
  (defun c++-mode-setup ()
    (interactive)
    (kill-all-local-variables)
    (set-syntax-table c++-mode-syntax-table)
    (setq major-mode     'c++-mode
          mode-name      "C++"
          comment-column 32
    )
    (set (make-local-variable 'comment-start) "// ")
    (set (make-local-variable 'comment-end) "")
    (set (make-local-variable 'comment-start-skip) "/\\*+ *\\|// *")
    (set (make-local-variable 'paragraph-start) (concat "^$\\|" page-delimiter))
    (set (make-local-variable 'paragraph-separate) paragraph-start)
    (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
    (set (make-local-variable 'require-final-newline) t)
    (set (make-local-variable 'parse-sexp-ignore-comments) nil)
    (run-hooks 'c++-mode-hook)
  )
)

(if lse-emacsX-p
    (progn
      (make-face 'olt-font-lock-keyword-face)
      (make-face 'olt-font-lock-macro-face)
      (make-face 'olt-font-lock-macro-block-face)
      (if (x-display-color-p)
          (progn
            (set-face-foreground 'olt-font-lock-keyword-face       "DarkSlateBlue")
            (set-face-foreground 'olt-font-lock-macro-face         "OrangeRed")
            (set-face-foreground 'olt-font-lock-macro-block-face   "Red")
          )
      )
      (defvar olt-font-lock-keyword-face     'olt-font-lock-keyword-face)
      (defvar olt-font-lock-macro-face       'olt-font-lock-macro-face)
      (defvar olt-font-lock-macro-block-face 'olt-font-lock-macro-block-face)
    )
)

;;;  4-Mar-1996
(defun olt-mode ()
  (interactive)
  (c++-mode)
  (lse-language:use "olt")
  ;; 3-Apr-1998 ;; (setq major-mode     'olt-mode mode-name      "OLT")

  (if lse-emacsX-p
    (font-lock-add-keywords 'c++-mode
        '(("@\\(end\\)?\\(forall\\|if\\|macro\\|noexpand\\|while\\)"
          . olt-font-lock-macro-block-face
          )
          ("@[A-Za-z_]+"                   . olt-font-lock-macro-face)
          ("\\<activate[A-Z][A-Za-z_]+"    . olt-font-lock-keyword-face)
          ("\\<begin[A-Z][A-Za-z_]+"       . olt-font-lock-keyword-face)
          ("\\<contains[A-Z][A-Za-z_]+"    . olt-font-lock-keyword-face)
          ("\\<define[A-Z][A-Za-z_]+"      . olt-font-lock-keyword-face)
          ("\\<end[A-Z][A-Za-z_]+"         . olt-font-lock-keyword-face)
          ("\\<exports[A-Z][A-Za-z_]+"     . olt-font-lock-keyword-face)
          ("\\<initialize[A-Z][A-Za-z_]+"  . olt-font-lock-keyword-face)
          ("\\<renames[A-Z][A-Za-z_]+"     . olt-font-lock-keyword-face)
          ("\\<uses[A-Z][A-Za-z_]+"        . olt-font-lock-keyword-face)
         )
    );  3-Apr-1998
  );  3-Apr-1998
; olt-mode
)
