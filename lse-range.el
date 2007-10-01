;-*- unibyte: t; coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work
 
;;;;unix_ms_filename_correspondency lse-range:el lse_rnge:el
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
;;;;    lse-range
;;;;
;;;; Purpose
;;;;    Administration of ranges
;;;; 
;;;; Description
;;;;    A lse-range consists of two markers: head and tail. 
;;;; 
;;;;    The functions changing ranges should be used only inside the buffer of
;;;;    the range.
;;;; 
;;;;    All exported functions have names like `lse-range:foo', where foo
;;;;    specifies the function. Never use functions with names like
;;;;    `lse-range@bar' or containing underscores. 
;;;; 
;;;;    Internally tail is offset by +1 to avoid collapsing of the range, when
;;;;    it is deleted. Therefore access to the tail-position must be done
;;;;    using the lse-range function lse-range:tail-pos. 
;;;;
;;;; Revision Dates
;;;;    24-May-1994 (CT) Creation (of comment)
;;;;    24-May-1994 (CT) set-marker used for change of marker
;;;;     7-Oct-1996 (CT) lse-insert:emacs: replaced by insert
;;;;    11-Oct-1996 (CT) lse-range:contents-np and lse-range:clean-np defined
;;;;-- 
(provide 'lse-range)

(defmacro positivep (x)
  (list '> x 0)
)

(defun lse-range:new (head tail)
  (if (< tail head)
      (lse-range@new tail head)
    (lse-range@new head tail)
  )
)

(defun lse-range:new-list (ranges)
  (let* (result)
    (while (and (consp ranges) (cdr ranges))
      (setq result 
            (cons (lse-range:new (car ranges) (car (cdr ranges))) result)
      )
      (setq ranges (cdr (cdr ranges)))
    )
    (reverse result)
  )
)

(defun lse-range@new (head tail)
  (vector (if (markerp head) head (copy-marker head))
          (copy-marker (1+ tail))
  )
)

(defun lse-range:copy (range)
  (vector (copy-marker (lse-range:head range)) 
          (copy-marker (lse-range:tail range)) 
  )
)

(defun lse-range:is-collapsed (range)
  (equal (lse-range:head range) (lse-range:tail range))
)

(defun lse-range:is-empty (range)
  (equal (lse-range:head-pos range) (lse-range:tail-pos range))
)

(defun lse-range:head (range)
  (if range (aref range 0))
)

(fset 'lse-range:head-pos 'lse-range:head)

(defun lse-range:tail (range)
  (if range (aref range 1))
)

(defun lse-range:tail-pos (range)
  (if range 
      (let ((result (make-marker))
            (tail   (lse-range:tail range))
           )
        (set-marker result (1- tail) (marker-buffer tail))
        result 
      )
  )
)

(defun lse-range:contents (range)
  (if (lse-range:is-collapsed range)
      nil
    (buffer-substring
        (lse-range:head-pos range) (lse-range:tail-pos range)
    )
  )
)

;;; 11-Oct-1996 
(defun lse-range:contents-np (range)
  (if (lse-range:is-collapsed range)
      nil
    (buffer-substring-no-properties
        (lse-range:head-pos range) (lse-range:tail-pos range)
    )
  )
)

(defun lse-range:length (range)
  (if (lse-range:is-collapsed range)
      -1
    (- (lse-range:tail-pos range) (lse-range:head-pos range))
  )
)

(defun lse-range:inside (range &optional pos)
  (or pos (setq pos (point)))
  (let ((head-pos (lse-range:head-pos range))
        (tail-pos (lse-range:tail-pos range))
       )
    (and (integer-or-marker-p head-pos) 
         (integer-or-marker-p tail-pos)
         (> tail-pos head-pos)
         (not (or (> head-pos pos) (< tail-pos pos)))
    )
  )
)

(defun lse-range:clean (range)
  (if (lse-range:is-collapsed range)
      nil
    (let ((result (lse-range:contents range)))
      (delete-region (lse-range:head-pos range) (lse-range:tail-pos range))
      result
    )
  )
)

;;; 11-Oct-1996 
(defun lse-range:clean-np (range)
  (if (lse-range:is-collapsed range)
      nil
    (let ((result (lse-range:contents-np range)))
      (delete-region (lse-range:head-pos range) (lse-range:tail-pos range))
      result
    )
  )
)

(defun lse-range:replace-contents (range replacement)
  (if (lse-range:is-collapsed range)
      (error "Cannot replace collapsed region")
    (save-excursion
      (goto-char       (lse-range:head-pos range))
      (lse-range:clean range)
      (insert          replacement)
      (lse-range:change-tail-pos 
           range (+ (lse-range:head-pos range) (length replacement))
      )
    )
  )
)

(defun lse-range:change-head (range to-head)
  (let ((head (aref range 0)))
    (set-marker head to-head
                (marker-buffer (if (markerp to-head) to-head head))
    )
  )
)

(fset 'lse-range:change-head-pos 'lse-range:change-head)

(defun lse-range:change-tail (range to-tail)
  (let ((tail (aref range 1)))
    (set-marker tail to-tail
                (marker-buffer (if (markerp to-tail) to-tail tail))
    )
  )
)

(defun lse-range:change-tail-pos (range to-tail)
  (let ((tail (aref range 1)))
    (set-marker tail (1+ to-tail)
                (marker-buffer (if (markerp to-tail) to-tail tail))
    )
  )
)

