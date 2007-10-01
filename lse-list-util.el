;-*- unibyte: t; coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work
 
;;;;unix_ms_filename_correspondency lse-list-util:el lse_lstu:el
;;;; (c) 1994 Swing Informationssysteme GmbH. All rights reserved.

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
;;;;    lse-list-util
;;;;
;;;; Purpose
;;;;    Provide functions for simple list manipulation
;;;;
;;;; Revision Dates
;;;;    18-Jun-1994 (CT) Creation (of comment)
;;;;     9-Oct-1996 (CT) Define add-to-list if (not fboundp)
;;;;--
(provide 'lse-list-util)

(defmacro lse-add-to-list (the-list the-entry)
  (`(setq (, the-list) (cons (, the-entry) (, the-list))))
)

(defmacro lse-remove-car-from-list (the-list)
  (`(setq (, the-list) (cdr (, the-list))))
)

(defmacro lse-remove-from-list (the-list the-entry &optional replace-by)
  (`(let (pred
          (entry (, the-entry))
         )
      (if entry
          (progn
            (if (equal entry (car (, the-list)))
                (if (, replace-by)
                    (setcar (, the-list) (, replace-by))
                  (setq (, the-list) (cdr (, the-list)))
                )
              (let ((pred (, the-list)))
                (while (and pred (not (equal entry (car (cdr pred)))))
                  (setq pred (cdr pred))
                )
                (if (, replace-by)
                    (setcdr pred (cons (, replace-by) (cdr (cdr pred))))
                  (setcdr pred (cdr (cdr pred)))
                )
              )
            )
          )
      )
    )
  )
)

(if (not (fboundp 'add-to-list))
    ;;;  9-Oct-1996 
    (defun add-to-list (var value)
      "Add to the value of LIST-VAR the element ELEMENT if it isn't there yet.
The test for presence of ELEMENT is done with `equal'.
If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job."
      (or (member value (symbol-value var))
          (set var (cons value (symbol-value var)))
      )
    ; add-to-list
    )
)
