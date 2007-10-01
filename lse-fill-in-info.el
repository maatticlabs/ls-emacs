;-*- unibyte: t; coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work
 
;;;;unix_ms_filename_correspondency lse-fill-in-info:el lse_finf:el
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
;;;;    lse-fill-in-info
;;;;
;;;; Purpose
;;;;    Administration of fill-in information
;;;;
;;;; Description
;;;;    fill-in_info is a cons cell usable for association lists containing
;;;;    information about a single occurrence of the fill-in, with
;;;;    the car:
;;;;        symbol      : the symbol defining the fill-in (used as key in
;;;;                      association lists) 
;;;;    the cdr of the fill-in_info is a vector with the elements:
;;;;        name        : the name of the fill-in as found in the fill-in
;;;;        state       : - 'lse@flat if fill-in is in buffer
;;;;                      - 'lse@deep if fill-in was filled by an expansion
;;;;                        or by a replacement typed by the user  
;;;;                      - 'lse@dead for a killed fill-in 
;;;;        fill-type   : 'lse@expanded or 'lse@replaced, if fill-in was
;;;;                      already filled 
;;;;        range       : range of fill-in       (of type lse-range)
;;;;        inner-range : inner range of fill-in (of type lse-range)
;;;;                      - for state=flat, range of fill-in's name (of type
;;;;                        lse-range) this is only interesting as long as the
;;;;                        fill-in was never expanded 
;;;;                      - for state=deep, range of replacement without
;;;;                        replacement-leading and replacement-trailer
;;;;        complement  : fill-in's value in the complementary state, i.e. 
;;;;                      - for state = deep, complement contains the flat
;;;;                        value 
;;;;                      - for state = flat, complement contains the fill-in's
;;;;                        value before the last execution of the
;;;;                        lse-unexpand-fill-in command (nil, if never filled)
;;;;        duplicate   : lse-range of duplicate fill-in (for list-fill-in's,
;;;;                      nil for non-list-fill-in's)
;;;;        enclosure   : surroundings of killed fill-in
;;;;        ancestor    : head-pos of fill-in which supplied value for
;;;;                      replication  
;;;;        descendants : list of fill-in-infos which were auto-replicated
;;;;                      from this one
;;;;        id          : surrogate id (unambiguous)
;;;;        parent-id   : surrogate id (unambiguous) of parent fill-in
;;;; 
;;;; Revision Dates
;;;;    24-May-1994 (CT) Creation (of comment)
;;;;    24-May-1994 (CT) lse-range:name-range renamed to lse-range:inner-range 
;;;;    12-Jun-1994 (CT) descendants added
;;;;    11-Oct-1996 (CT) `id' added
;;;;    17-Oct-1996 (CT) lse-fill-in:show changed to use princ instead of
;;;;                     insert 
;;;;    17-Oct-1996 (CT) `parent-id' added
;;;;-- 
(provide 'lse-fill-in-info)

(defconst                    lse-fill-in:id_last 0); 11-Oct-1996 
(make-variable-buffer-local 'lse-fill-in:id_last)  ; 11-Oct-1996 

(defun lse-fill-in:new 
    (psym name state fill-type range 
     &optional inner-range complement duplicate enclosure ancestor descendants 
     
    )
  (cons psym (vector name state fill-type range 
              inner-range complement duplicate enclosure ancestor descendants
              nil nil
             )
  )
)

(defun lse-fill-in:symbol (fill-in_info)
  (car fill-in_info)
)

(defun lse-fill-in:name (fill-in_info)
  (and fill-in_info (aref (cdr fill-in_info) 0))
)

(defun lse-fill-in:state (fill-in_info)
  (and fill-in_info (aref (cdr fill-in_info) 1))
)

(defun lse-fill-in:fill-type (fill-in_info)
  (and fill-in_info (aref (cdr fill-in_info) 2))
)

(defun lse-fill-in:range (fill-in_info)
  (and fill-in_info (aref (cdr fill-in_info) 3))
)

(defun lse-fill-in:inner-range (fill-in_info)
  (and fill-in_info (aref (cdr fill-in_info) 4))
)

(defun lse-fill-in:complement (fill-in_info)
  (and fill-in_info (aref (cdr fill-in_info) 5))
)

(defun lse-fill-in:duplicate (fill-in_info)
  (and fill-in_info (aref (cdr fill-in_info) 6))
)

(fset  'lse-fill-in:is-list 'lse-fill-in:duplicate)

(defun lse-fill-in:enclosure (fill-in_info)
  (and fill-in_info (aref (cdr fill-in_info) 7))
)

(defun lse-fill-in:ancestor (fill-in_info)
  (and fill-in_info (aref (cdr fill-in_info) 8))
)

(defun lse-fill-in:descendants (fill-in_info)
  (and fill-in_info (aref (cdr fill-in_info) 9))
)

;;; 11-Oct-1996 
(defun lse-fill-in:id (fill-in_info)
  (and fill-in_info (aref (cdr fill-in_info) 10))
; lse-fill-in:id
)

;;; 17-Oct-1996 
(defun lse-fill-in:parent-id (fill-in_info)
  (and fill-in_info (aref (cdr fill-in_info) 11))
; lse-fill-in:id
)

(defun lse-fill-in:change-symbol (fill-in_info to-value)
  (setcar fill-in_info to-value)
)

(defun lse-fill-in:change-name (fill-in_info to-value)
  (aset (cdr fill-in_info) 0 to-value)
)

(defun lse-fill-in:change-state (fill-in_info to-value)
  (aset (cdr fill-in_info) 1 to-value)
)

(defun lse-fill-in:change-fill-type (fill-in_info to-value)
  (aset (cdr fill-in_info) 2 to-value)
)

(defun lse-fill-in:change-range (fill-in_info to-value)
  (aset (cdr fill-in_info) 3 to-value)
)

(defun lse-fill-in:change-inner-range (fill-in_info to-value)
  (aset (cdr fill-in_info) 4 to-value)
)

(defun lse-fill-in:change-complement (fill-in_info to-value)
  (aset (cdr fill-in_info) 5 to-value)
)

(defun lse-fill-in:change-duplicate (fill-in_info to-value)
  (aset (cdr fill-in_info) 6 to-value)
)

(fset  'lse-fill-in:change-is-list 'lse-fill-in:change-duplicate)

(defun lse-fill-in:change-enclosure (fill-in_info to-value)
  (aset (cdr fill-in_info) 7 to-value)
)

(defun lse-fill-in:change-ancestor (fill-in_info to-value)
  (aset (cdr fill-in_info) 8 to-value)
)

(defun lse-fill-in:change-descendants (fill-in_info to-value)
  (aset (cdr fill-in_info) 9 to-value)
)

;;; 17-Oct-1996 
(defun lse-fill-in:set-id (fill-in_info)
  (or (lse-fill-in:id fill-in_info)
      (aset (cdr fill-in_info) 10
            (setq lse-fill-in:id_last (1+ lse-fill-in:id_last))
      )
  )
; lse-fill-in:set-id
)

;;; 17-Oct-1996 
(defun lse-fill-in:set-parent-id (fill-in_info to-value)
  (aset (cdr fill-in_info) 11 to-value)
; lse-fill-in:set-id
)

(defun lse-fill-in:other-state (state)
  (cond ((equal state 'lse@deep) 'lse@flat)
        ((equal state 'lse@flat) 'lse@deep)
        ((equal state 'lse@dead) 'lse@flat)
  )
; lse-fill-in:other-state
) 

(defun lse-fill-in:equal (fill-in_info psym head tail)
  (and fill-in_info
       (eq    (lse-fill-in:symbol fill-in_info)                     psym)
       (equal (lse-range:head-pos (lse-fill-in:range fill-in_info)) head)
       (equal (lse-range:tail-pos (lse-fill-in:range fill-in_info)) tail)
  )
)

(defun lse-fill-in:show (fill-in_info)
  (if fill-in_info
      (let ((hp (lse-range:head-pos (lse-fill-in:range fill-in_info)))
            (tp (lse-range:tail-pos (lse-fill-in:range fill-in_info)))
           )
        (princ "\n")
        (princ (format "%-25s" (lse-fill-in:name fill-in_info)))
        (princ (format " %4d %5d %5d" 
                       (or (lse-fill-in:id fill-in_info) -1)
                       (if hp (marker-position hp) 0)
                       (if tp (marker-position tp) 0)
               )
        )
        (princ (format " %-10s" (lse-fill-in:state      fill-in_info)))
        (princ (format " %s"    (lse-fill-in:complement fill-in_info)))
      )
  )
; lse-fill-in:show
) 
