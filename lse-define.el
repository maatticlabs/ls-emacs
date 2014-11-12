;-*- coding: utf-8 -*-

;;;; Copyright (C) 1994-2014 Mag. Christian Tanzer. All rights reserved.
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
;;;;    lse-define
;;;;
;;;; Purpose
;;;;    Functions for definition of emacs lse templates (fill-in and token)
;;;;
;;;; Revision Dates
;;;;    24-May-1994 (CT) Creation (of comment header)
;;;;    24-May-1994 (CT) rcompletion-leading and rcompletion-trailer added
;;;;    26-May-1994 (CT) rcompletion-action       added
;;;;                     lse-define-simple-token  added
;;;;                     max-line-move, dont-move added
;;;;                     no-history               added
;;;;    12-Jun-1994 (CT) auto-replicate           added
;;;;    15-Jun-1994 (CT) dont-move                removed
;;;;                     (redundant, use '(max-line-move 0) instead)
;;;;    26-Jun-1994 (CT) replacement-vanguard     added
;;;;    13-Sep-1994 (CT) lse-language:fill-in-refs & lse-language:fill-in-defs
;;;;    18-Feb-1995 (CT) kill-action added
;;;;    12-Apr-1995 (CT) lse-anchor-indent added
;;;;    17-Aug-2000 (CT) auto-expand added
;;;;     4-Oct-2002 (CT) hang-indent added
;;;;     4-Oct-2002 (CT) properties  added
;;;;    17-Jul-2009 (CT) `lse-define-fill-in-menu` robustified
;;;;    19-Jan-2011 (CT) `lse-define:fill-in-replacement` changed to map `$`
;;;;                     to `lse-auto-expand-replacement-fill-in`
;;;;    20-Jan-2011 (CT) `lse-define:fill-in-replacement` changed to consider
;;;;                     `no-sep` for `consp` elements
;;;;    25-Jan-2011 (CT) `lse-indent:set:curr` added to
;;;;                     `lse-define:fill-in-replacement`
;;;;    28-Jan-2011 (CT) `lse-newline-and-indent-to` added to
;;;;                     `lse-define:fill-in-replacement`
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-define)

(defvar lse::token-table   (make-vector 137 0))
(defvar lse_fill-in_table (make-vector 137 0)
  ;; 25-Nov-1993: SWING$_LSE:[*...]*.LSE contained 3607 placeholder definitions
)

(make-variable-buffer-local 'lse::token-table)
(make-variable-buffer-local 'lse_fill-in_table)

(defun lse_fill-in:definition (name)
  (intern-soft (downcase name) lse_fill-in_table)
)

(defun lse-define:message (&rest text)
  (princ
      (apply 'format text)
      (get-buffer-create " $LSE-definition-log$")
  )
  (terpri (get-buffer " $LSE-definition-log$"))
)

(defun lse-define:typed-option (typep tail)
  (if tail
      (if (funcall typep tail)
          tail
        (if (and (consp tail) (= (length tail) 1) (funcall typep (car tail)))
            (car tail)
        )
      )
  )
; lse-define:typed-option
)

(defun lse-define:integer-option (tail)
  (lse-define:typed-option 'integerp tail)
; lse-define:string-option
)

(defun lse-define:string-option (tail)
  (lse-define:typed-option 'stringp tail)
; lse-define:string-option
)

(defun lse-define-fill-in-option (psym name head tail &optional dont-separate)
  (let ((result t)
        (integer-option (lse-define:integer-option tail))
        (string-option  (lse-define:string-option  tail))
       )
    (cond ((memq head '(leading trailer))
           (if string-option
               (if (consp tail)
                   (put psym head (regexp-quote string-option))
                 (put psym head string-option)
               )
             (lse-define:message
                "Fill-in     %25s: option `%s' requires a string value instead of: `%s'"
                name (symbol-name head) tail
             )
           )
          )
          ((eq head 'description)
           (cond (string-option
                  (put psym head string-option)
                 )
                 ((consp tail)
                  (put psym  head (car tail))
                  (put psym 'help (mapconcat 'identity (cdr tail) "\n"))
                 )
                 (t (lse-define:message
                     "Fill-in     %25s: option `%s' requires a string value instead of: `%s'"
                     name (symbol-name head) tail
                    )
                 )
           )
          )
          ((memq head '(separator            rcompletion-action
                        rcompletion-leading  rcompletion-trailer
                        replacement-leading  replacement-trailer
                        replacement-vanguard ; 26-Jun-1994
                        kill-action          ; 18-Feb-1995
                       )
           )
           (put psym head
                (lse-define-fill-in::inner
                  (concat "$" name "$" (symbol-name head))
                  t
                  (list (cons 'replacement tail))
                )
           )
          )
          ((eq head 'token)
           (lse-define-fill-in-token (or string-option name) psym)
          )
          ((memq head '(no-history sort auto-expand))
           (put psym head t)
          )
          ((memq head '(max-line-move auto-replicate hang-indent))
           (if integer-option
               (put psym head integer-option)
             (lse-define:message
                "Fill-in     %25s: option `%s' requires an integer value instead of: `%s'"
                name (symbol-name head) tail
             )
           )
          )
          ((eq head 'properties);  4-Oct-2002
           (put psym head tail)
          )
          (t (lse-define:message
                "Fill-in     `%25s': unknown option `%s' ignored" name head
             )
          )
    )
    result
  )
; lse-define-fill-in-option
)

(defun lse-define-fill-in-function    (psym name body &optional dont-separate)
  (let (next
        function
       )
    (while body
      (setq next (car body))
      (setq body (cdr body))
      (cond ((symbolp next)
             (lse-add-to-list function next)
            )
            ((and (consp   next)
                  (fboundp (car next))
             )
             (lse-add-to-list function next)
            )
            (t
             (lse-define:message
                  "Fill-in     `%25s': unknown item `%s' ignored" name next
             )
            )
      )
    )
    (setq function (nreverse  function))
    (if function
        (put psym 'function function)
      (lse-define:message "Fill-in     `%25s': no function given" name
      )
    )
  )
; lse-define-fill-in-function
)

(defun lse-define-fill-in-menu (psym name body &optional dont-separate)
  (let (next
        menu-entries
        entry
       )
    (while body
      (setq next (car body))
      (setq body (cdr body))
      (cond ((eq next '@)
             (setq next (car body))
             (setq body (cdr body))
             (cond ((symbolp next) (setq entry (symbol-name next)))
                   ((stringp next) (setq entry next))
                   (t              (setq entry nil)); 17-Jul-2009
             )
             (if entry
                 (progn
                   (lse-add-to-list menu-entries '@)
                   (lse-add-to-list menu-entries entry)
                   (lse-add-to-list lse-language:fill-in-refs entry)
                 )
               (lse-define:message
                    "Fill-in     `%25s': unknown item `%s' ignored" name next
               )
             )
            )
            ((symbolp next)
             (setq entry (downcase (symbol-name next)))
             (lse-add-to-list
                menu-entries
                (intern entry lse_fill-in_table)
             )
             (lse-add-to-list lse-language:fill-in-refs entry)
            )
            ((stringp next)
             (lse-add-to-list menu-entries next)
            )
            ((consp next)
             (lse-add-to-list menu-entries next)
            )
            (t
             (lse-define:message
                  "Fill-in     `%25s': unknown item `%s' ignored" name next
             )
            )
      )
    )
    (setq menu-entries (nreverse  menu-entries))
    (if menu-entries
        (put psym 'menu menu-entries)
      (lse-define:message
           "Fill-in      `%25s': list of menu-entries is empty" name
      )
    )
  )
; lse-define-fill-in-menu
)

(defun lse-define:fill-in-replacement (psym name body &optional dont-separate)
  (let (next
        replacement
        item_sep
        head
        tail
        fill-in-ref
        (dont-separate-next t)
       )
    (while body
      (if (or dont-separate dont-separate-next)
          (setq item_sep nil)
        (setq item_sep 'lse-newline-and-indent)
      )
      (setq dont-separate-next nil)
      (setq next (car body))
      (setq body (cdr body))
      (cond ((consp next)
             (setq head (car next))
             (if (eq head 'line)
                 (setq replacement
                     (append
                        (lse-define:fill-in-replacement psym name (cdr next) t)
                        (if item_sep (cons item_sep replacement) replacement)
                     )
                 )
               (let ((no-sep
                       (memq head
                         '(lse-anchor-indent      lse-hang-indent
                           lse-indent             lse-indent-to-pattern
                           lse-prev-indent        lse-reindent
                           lse-indent:>           lse-indent:<
                           lse-indent:set         lse-indent:set:prev
                           lse-indent:set:curr    lse-newline-and-indent-to
                           lse-newline-and-indent lse-newline-and-indent-unless
                          )
                       )
                     )
                    )
                 (if (and item_sep (not no-sep))
                     (lse-add-to-list replacement item_sep)
                 )
                 (lse-add-to-list replacement next)
                 (if no-sep (setq dont-separate-next t))
               )
             )
            )
            ((eq next '@)
             (setq tail (car body))
             (setq body (cdr body))
             (if (symbolp tail) (setq tail (symbol-name tail)))
             (if item_sep (lse-add-to-list replacement item_sep))
             (lse-add-to-list replacement (list 'lse-auto-expand-fill-in tail))
             (lse-add-to-list lse-language:fill-in-refs tail)
            )
            ((eq next '$); 19-Jan-2011
             (setq tail (car body))
             (setq body (cdr body))
             (if (symbolp tail) (setq tail (symbol-name tail)))
             (if item_sep (lse-add-to-list replacement item_sep))
             (lse-add-to-list replacement
               (list 'lse-auto-expand-replacement-fill-in tail)
             )
             (setq dont-separate-next t)
             (lse-add-to-list lse-language:fill-in-refs tail)
            )
            ((eq next '&) (setq dont-separate-next t))
            ((symbolp next)
             (if (and item_sep
                      (not (memq next
                             '(lse-tabulator       delete-horizontal-space
                               fixup-whitespace    just-one-space
                               lse-indent:<        lse-indent:>
                               lse-indent:set:curr
                               lse-newline         lse-newline-and-indent
                              )
                           )
                      )
                 )
                 (lse-add-to-list replacement item_sep)
             )
             (lse-add-to-list replacement next)
             (if (memq next
                       '(lse-tabulator            lse-no-indent
                         lse-newline              lse-newline-and-indent
                         lse-reindent             lse-newline-and-indent-unless
                         lse-indent+1             lse-indent-1
                         lse-indent:<             lse-indent:>
                         lse-anchor-indent        lse-expansion-indent
                         lse-environment-indent   lse-outer-environment-indent
                         lse-hang-indent          lse-prev-indent
                         lse-indent:set           lse-indent:set:prev
                         lse-indent:set:curr
                         delete-horizontal-space  delete-indentation
                         fixup-whitespace         just-one-space
                         delete-blank-lines
                        )
                 )
                 (setq dont-separate-next t)
             )
            )
            ((stringp next)
             (if item_sep (lse-add-to-list replacement item_sep))
             (lse-add-to-list replacement next)
            )
            (t
             (lse-define:message
                  "Fill-in     `%25s': unknown item `%s' ignored" name next
             )
            )
      )
    )
    replacement
  )
)

(defun lse-define-fill-in-replacement (psym name body &optional dont-separate)
  (let (replacement)
    (setq replacement
          (lse-define:fill-in-replacement psym name body dont-separate)
    )
    (setq replacement (nreverse  replacement))
    (if replacement   (put psym 'replacement replacement))
  )
)

(defun lse-define-fill-in-properties  (psym name body &optional dont-separate)
  (let (next
        fill-in-type
       )
    (while body
      (setq next (car body))
      (setq body (cdr body))
      (if (consp next)
          (let ((head (car next))
                (tail (cdr next))
               )
            (cond ((memq head '(replacement menu function))
                   (if fill-in-type
                       (message "Fill-in `%25s': multiple expansions (%s, %s)"
                                name fill-in-type head
                       )
                     (setq fill-in-type head)
                     (funcall
                        (symbol-function
                          (intern
                            (concat "lse-define-fill-in-" (symbol-name head))
                          )
                        )
                        psym name tail dont-separate
                     )
                   )
                  )
                  (t (lse-define-fill-in-option psym name head tail))
            )
          )
        (message
             "Fill-in     `%25s': invalid definition `%s'" name next
        )
      )
    )
    (put psym 'type (or fill-in-type 'terminal))
  )
; lse-define-fill-in-properties
)

(defun lse-define-fill-in::inner (name dont-separate body)
  (let ((psym (intern-soft (downcase name) lse_fill-in_table))
        new
       )
    (if (not psym)
        (progn
          (setq psym (intern (downcase name) lse_fill-in_table))
          (setq new  t)
        )
      (if (not (symbol-plist psym))
          (setq new t)
      )
    )

    (lse-define-fill-in-properties psym name body dont-separate)

    (if new
        (lse-define:message "Fill-in      `%25s' newly defined" name)
      (lse-define:message "Fill-in      `%25s' redefined" name)
    )
  psym
  )
; lse-define-fill-in::inner
)

(defun lse-define-fill-in (name &rest body)
  (lse-define-fill-in::inner name nil body)
  (lse-add-to-list lse-language:fill-in-defs (downcase name))
)

(defun lse-define-fill-in-token (token-name fill-in)
  (let* ((name (downcase token-name))
         (tsym (intern-soft name lse::token-table))
         (new  nil)
        )
    (if (symbolp fill-in)
        (setq fill-in (symbol-name  fill-in))
    )
    (if (not tsym)
        (progn
          (setq tsym (intern name lse::token-table))
          (setq new  t)
        )
    )
    (set  tsym fill-in)
    (fset tsym 'lse-expand-fill-in-token)
    (if new
        (lse-define:message
             "Token        `%25s' newly defined for fill-in `%s'" name fill-in
        )
      (lse-define:message
             "Token        `%25s' redefined for fill-in `%s'" name fill-in
      )
    )
  )
; lse-define-fill-in-token
)

(defun lse-define-simple-token (token-name expansion)
  (let* ((name (downcase token-name))
         (tsym (intern-soft name lse::token-table))
         new
        )
    (if (not tsym)
        (progn
          (setq tsym (intern name lse::token-table))
          (setq new  t)
        )
    )

    (if (or (stringp expansion) (consp expansion))
        (progn
          (set  tsym expansion)
          (fset tsym nil)
        )
      (message
        "Simple token `%25s': invalid definition `%s' (should be string or list)"
        name expansion
      )
    )

    (if new
        (lse-define:message "Simple token `%25s' newly defined" name)
      (lse-define:message "Simple token `%25s' redefined" name)
    )
  tsym
  )
; lse-define-simple-token
)
