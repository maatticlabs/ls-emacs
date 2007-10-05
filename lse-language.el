;-*- unibyte: t; coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work

;;;;unix_ms_filename_correspondency lse-language:el lse_lngg:el
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
;;;;    lse-language
;;;;
;;;; Purpose
;;;;    Define functions for definition, compilation, and loading of lse
;;;;    languages
;;;;
;;;; Revision Dates
;;;;    24-May-1994 (CT) Creation (of comment)
;;;;    24-May-1994 (CT) Definition of fill-in "$$default$$separator" added
;;;;    26-May-1994 (CT) Interactive functions moved to lse-interactive
;;;;     3-Jun-1994 (CT) Use lse-load-path
;;;;    14-Jun-1994 (CT) Use lse-language:master_prefix
;;;;    13-Sep-1994 (CT) lse-language:fill-in-refs & lse-language:fill-in-defs
;;;;    11-Oct-1996 (CT) lse-language:name added
;;;;    11-Oct-1996 (CT) Comments added
;;;;     5-Oct-2007 (CT) Use `lse-language:call-hook` instead of `funcall`
;;;;                     (and try to pass `t` to each of the hooks there)
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-language)

(defvar lse-language:master_prefix "lse-language-")
(defvar lse-language:source_ext    ".lse")
(defvar lse-language:compil_ext    ".lsc")

(defun lse-compile@break-line ()
  (let (curr
        prev
        (eol (lse-tpu:line-tail-pos))
        state
       )
    (save-excursion
      (beginning-of-line)
      (down-list 1)
      (setq prev (point))
      (while (> (- eol (point)) 511)
        (setq state (parse-partial-sexp (point) (+ (point) 256) nil nil state))
        (if (elt state 3)
            (progn
              (while (elt state 3)
                (setq state (parse-partial-sexp (point) eol nil t state))
              )
              (setq curr (point))
            )
          (setq curr (or (elt state 2) (elt state 1)))
        )
        (if curr
            (progn
              (goto-char curr)
              (insert-before-markers "\n")
              (setq prev (point))
            )
          (error "Did not find break point")
        )
      )
    )
  )
)

(defun lse-compile@write-one-fill-in (f)
  (if (equal f 0)
      t
    (princ "(lse-define-compiled-fill-in " (current-buffer))
    (prin1  (symbol-name  f)               (current-buffer))
    (terpri                                (current-buffer))
    (princ  "'"                            (current-buffer))
    (prin1  (symbol-plist f)               (current-buffer))
    (if (> (current-column) 512)           (lse-compile@break-line))
    (terpri                                (current-buffer))
    (princ  ")"                            (current-buffer))
    (terpri                                (current-buffer))
    (terpri                                (current-buffer))
  )
)

(defun lse-compile@write-one-token (tok)
  (if (equal tok 0)
      t
    (if (symbol-function tok)
        (princ "(lse-define-compiled-token "        (current-buffer))
      (princ "(lse-define-compiled-simple-token "   (current-buffer))
    )
    (prin1  (symbol-name     tok)          (current-buffer))
    (princ  " "                            (current-buffer))
    (if (not (symbol-function tok))
        (princ "'"                         (current-buffer))
    )
    (prin1  (symbol-value    tok)          (current-buffer))
    (princ  ")"                            (current-buffer))
    (terpri                                (current-buffer))
  )
)

(defun lse-define-compiled-fill-in (name plist)
  (let ((psym (intern (downcase name) lse_fill-in_table))
       )
    (setplist psym plist)
  )
)

(defun lse-define-compiled-token (name tval)
  (let ((tsym (intern (downcase name) lse_token_table))
       )
    (set  tsym tval)
    (fset tsym 'lse-expand-fill-in-token)
  )
)

(defun lse-define-compiled-simple-token (name tval)
  (let ((tsym (intern (downcase name) lse_token_table))
       )
    (set  tsym tval)
    (fset tsym nil)
  )
)

(defvar                      lse-language:table (make-vector 47 0)
  "Table holding all defined languages of LS-Emacs"
)
(defvar                      lse-language:@compiling      nil)
(defvar                      lse-language:fill-in-refs    nil); 13-Sep-1994
(defvar                      lse-language:fill-in-defs    nil); 13-Sep-1994
(defvar                      lse-language:initial-fill-in nil
  "Initial fill-in of language. This is inserted into an empty buffer."
)
(defvar                      lse-language:expand-initial  nil
  "If value is non-nil, the initial fill-in is automatically expanded."
)
(defvar                      lse-language:name            nil
  "Name of language used in buffer."
); 11-Oct-1996
(make-variable-buffer-local 'lse-language:initial-fill-in)
(make-variable-buffer-local 'lse-language:expand-initial)
(make-variable-buffer-local 'lse-language:name); 11-Oct-1996

(defun lse-language:define
           (name properties hooks files &optional fill-in-size token-size)
  (let ((lsym (intern-soft (downcase name) lse-language:table))
        new
       )
    (if (or lse-language:@compiling (not lsym))
        (progn
          (setq new  t)
          (setq lsym (intern (downcase name) lse-language:table))
          (put  lsym 'fill-in-table (make-vector (or fill-in-size 137) 0))
          (put  lsym 'token-table   (make-vector (or token-size    67) 0))
        )
    )
    (put lsym 'files      files)
    (put lsym 'properties properties)
    (put lsym 'hooks      hooks)
    (if new
        (lse-define:message "Language `%25s' newly defined" name)
      (lse-define:message "Language `%25s' redefined" name)
    )
    lsym
  )
; lse-language:define
)

(defun lse-language:set-property (name value)
  (set name value)
)

;;;  5-Oct-2007
(defun lse-language:call-hook (hook)
  (cond ((symbolp hook)
         (condition-case nil
             (funcall hook t)
           (wrong-number-of-arguments (funcall hook))
         )
        )
        ((consp hook) (eval hook))
  )
; lse-language:call-hook
)

(defun lse-language:use-loaded (lsym &optional initial)
  (mapc (function (lambda (x) (apply 'lse-language:set-property x)))
        (get lsym 'properties)
  )
  (mapc 'lse-language:call-hook (get lsym 'hooks))
  (lse-tpu:set-word-char-for-idents)
  (setq lse_token_table   (get lsym 'token-table))
  (setq lse_fill-in_table (get lsym 'fill-in-table))
  (setq lse-language:name (symbol-name lsym))
  (and (bobp) (eobp) initial lse-language:initial-fill-in
       (progn
         (lse-fill-in-insert lse-language:initial-fill-in)
         (insert "\n")
         (goto-char 1)
         (lse-goto-next-fill-in)
         (if lse-language:expand-initial (lse-expand))
         (save-excursion
           (goto-char (point-max))
           (insert "\n\n")
         )
         (goto-char 1)
         (lse-goto-next-fill-in)
         (set-buffer-modified-p nil)
       )
  )
; lse-language:use-loaded
)

(defun lse-language:load@internal (name load-fct)
  (let ((lsym      (lse-language:pre-load name))
        (load-path lse-load-path)
       )
    (setq lse_token_table   (get lsym 'token-table))
    (setq lse_fill-in_table (get lsym 'fill-in-table))

    (if (not (funcall load-fct lsym))
        (error "Error in definition of language %s" name); (setq lsym nil)
      (put lsym 'token-table   lse_token_table)
      (put lsym 'fill-in-table lse_fill-in_table)
      (put lsym 'loaded t)
    )
    lsym
  )
; lse-language:load@internal
)

(defun lse-language:load-from-source (name)
  (setq lse-language:fill-in-refs nil); 13-Sep-1994
  (setq lse-language:fill-in-defs nil); 13-Sep-1994
  (lse-language:load@internal
    name
    (function
       (lambda (lsym)
         (let (result)
           (lse-define-fill-in "$$default$$separator"
                               '(replacement lse-newline-and-indent)
           )
           (setq result
                 (load (concat "lse-templates-generic" lse-language:source_ext)
                       t nil t
                 )
           )
           (mapcar
              (function
                 (lambda (f)
                   (lse-define:message "Loading  `%25s'" f)
                   (if (load (concat f lse-language:source_ext) t nil t)
                       t                ; relax
                     (setq result nil)
                     (lse-define:message "Error in file %s" f)
                   )
                 )
              )
              (get lsym 'files)
           )
           result
         )
       )
    )
  )
; lse-language:load-from-source
)

(defun lse-language:load-compiled (name)
  (lse-language:load@internal
     name
     (function
       (lambda (lsym)
         (load (concat lse-language:master_prefix name lse-language:compil_ext)
               t nil t
         )
       )
     )
  )
; lse-language:load-compiled
)

(defun lse-language:pre-load (name)
  (let ((load-path lse-load-path)
        (lsym (intern-soft (downcase name) lse-language:table))
       )
    (if lsym
        t                               ; relax
      (load (concat lse-language:master_prefix name lse-language:source_ext)
            nil nil t
      )
      (setq lsym (intern-soft (downcase name) lse-language:table))
      (if (not lsym)
          (error "Can't find language %s" name)
      )
    )
    lsym
  )
; lse-language:pre-load
)

; (lse-language:compile "generic")
; (lse-language:compile "elisp")
; (lse-language:compile "finanzplanung")
; (lse-language:compile "kassabuch")
; (lse-language:compile "aufwandserfassung")
; (lse-language:compile "lse")
; (lse-language:compile "latex")
; (lse-language:compile "perl")
; (lse-language:compile "person-kartei")
; (lse-language:compile "firma-kartei")
; (lse-language:compile "bib-kartei")
; (lse-language:compile "quot-kartei")
; (lse-language:compile "bash")
; (setq debug-on-error t)
; (setq debug-on-error nil)
