;-*-unibyte: t;-*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work
 
;;;;unix_ms_filename_correspondency lse-deep-fill-in:el lse_dpfi:el
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
;;;;    lse-deep-fill-in
;;;;
;;;; Purpose
;;;;    Functions for manipulation of deep (i.e., expanded or replaced)
;;;;    fill-ins
;;;;
;;;; Revision Dates
;;;;    24-May-1994 (CT) Creation (of comment)
;;;;    24-May-1994 (CT) Clarification of structure
;;;;    26-May-1994 (CT) Interactive functions moved to lse-interactive
;;;;    12-Jun-1994 (CT) Error corrected
;;;;    12-Jun-1994 (CT) Auto-replication added
;;;;    22-Jan-1995 (CT) Error in lse_toggle_fill-in_expansion corrected
;;;;                     (left blank line in some situations)
;;;;     4-Oct-1996 (CT) Call `lse-flat-fill-in:open-replacement-highlight'
;;;;                     in `lse_toggle_fill-in_expansion' when toggling into
;;;;                     the deep state
;;;;     9-Oct-1996 (CT) Added optional parameter `dont-highlight' to
;;;;                     `lse_toggle_fill-in_expansion' 
;;;;-- 
(provide 'lse-deep-fill-in)

;;;++
;;; Internals for un/re-expansion
;;;--
(defun lse_toggle_fill-in_expansion (toggle_fill-in &optional dont-highlight)
  (let* ((psym        (lse-fill-in:symbol        toggle_fill-in))
         (name        (lse-fill-in:name          toggle_fill-in))
         (complement  (lse-fill-in:complement    toggle_fill-in))
         (state       (lse-fill-in:state         toggle_fill-in))
         (other-state (lse-fill-in:other-state   state))
         (range       (lse-fill-in:range         toggle_fill-in))
         (dupl-range  (lse-fill-in:duplicate     toggle_fill-in))
         (head-pos    (lse-range:head-pos        range))
         (tail-pos    (lse-range:tail-pos        range))
        )
    (if (and (vectorp dupl-range) (eq other-state 'lse@flat))
        ;; when switching from deep to flat remove duplication if any
        (progn
          (lse-range:clean dupl-range)
          (if (equal (lse-range:tail range)          ; 22-Jan-1995 
                     (lse-range:head dupl-range)     ; 22-Jan-1995 
              )                                      ; 22-Jan-1995 
              (save-excursion                        ; 22-Jan-1995 
                (goto-char (lse-range:tail range))   ; 22-Jan-1995 
                (lse-tpu:delete-next-char 1)         ; 22-Jan-1995 
              )                                      ; 22-Jan-1995 
          )                                          ; 22-Jan-1995 
        )
    )
    (lse-fill-in:change-state      toggle_fill-in other-state)
    (lse-fill-in:change-complement toggle_fill-in (lse-range:clean range))
    (lse-fill-in:change-duplicate  toggle_fill-in dupl-range)

    (goto-char head-pos)
    (lse-fill-in-insert complement)
    (setq tail-pos (+ head-pos (length complement)))
    (lse-range:change-tail-pos range tail-pos)
    (if (vectorp dupl-range)
        (if (eq state 'lse@flat)
            (save-excursion
              (goto-char tail-pos)
              (lse-fill-in:change-duplicate toggle_fill-in
                   (lse_duplicate_current_fill-in psym name head-pos)
              )
            )
        )
    )
    (if (and (not dont-highlight);  9-Oct-1996 
             (eq other-state 'lse@deep);  4-Oct-1996 
        )
        (lse-flat-fill-in:open-replacement-highlight head-pos tail-pos)
    )
    (lse_goto_first_fill-in_of_range head-pos tail-pos)
    toggle_fill-in
  )
; lse_toggle_fill-in_expansion
)

(defun lse_unfill_fill-in (msg)
  (let ((last_fill-in (lse_fill-in_history:last_expansion))
        )
    (if (not last_fill-in)
        (error "No fill-in to %s" msg)
      (lse_fill-in_history:remove_last_expansion)
      (lse_fill-in_history:add_unexpansion
           (lse_toggle_fill-in_expansion last_fill-in)
      )
      (save-excursion                         ; 12-Jun-1994 auto-replication
        (mapcar 'lse_toggle_fill-in_expansion
                (lse-fill-in:descendants last_fill-in)
        )
      )
    )
  )
)