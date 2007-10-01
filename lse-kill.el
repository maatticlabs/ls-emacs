;-*- unibyte: t; coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work
 
;;;;unix_ms_filename_correspondency lse-kill:el lse_kill:el
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
;;;;    lse-kill
;;;;
;;;; Purpose
;;;;    Functions for killing and unkilling of fill-ins
;;;;
;;;; Revision Dates
;;;;    24-May-1994 (CT) Creation (of comment)
;;;;    24-May-1994 (CT) Clarification
;;;;    26-May-1994 (CT) Interactive functions moved to lse-interactive
;;;;     1-Jun-1994 (CT) Error removed from lse@kill_current_fill-in
;;;;    11-Jun-1994 (CT) Allow pattern in leading
;;;;    22-Jan-1995 (CT) lse_unkill_fill_in put in here 
;;;;                     (was contained inline in lse-unkill-fill-in in
;;;;                     lse-interactive.el)
;;;;    28-Jan-1995 (CT) Error corrected 
;;;;                     (in lse_trailer_tail_position_with_delim)
;;;;    18-Feb-1995 (CT) lse-kill-future-fill-in added
;;;;    19-Feb-1995 (CT) descendants added (to lse-kill-future-fill-in and
;;;;                     lse_unkill-fill-in)
;;;;    24-Feb-1995 (CT) Allow killing of optional no-replacement fill-in's
;;;;    26-Feb-1995 (CT) Accumulate descendents in lse-kill-future-fill-in
;;;;    13-Mar-1995 (CT) Quick and dirty adaptation to changed behavior of
;;;;                     Emacs 19.27 (forward-char suddenly gives an error at
;;;;                     the beginning of the buffer)
;;;;    20-Mar-1995 (CT) Add killed fill-in to expansion history
;;;;    11-Oct-1996 (CT) Use lse-range:contents-np in lse-kill:enclosure
;;;;    31-Dec-1997 (CT) `lse-kill:join-sexp-boundary-maybe' added
;;;;    ««revision-date»»···
;;;;-- 
(provide 'lse-kill)

;;; 22-Jan-1995 
(defun lse_kill:adjust_fill-in_leading (leading_range trailer_range)
  (save-excursion 
    (goto-char (lse-range:head-pos leading_range))
    (if (or (looking-at (concat "[ \t\n]*" lse_comment_head_delim_pattern))
            (bolp)
        )
        (let (needs-adjustment)
          (save-excursion 
            (goto-char (lse-range:head-pos trailer_range))
            (if (looking-at
                  (concat "[ \t]*" (or lse_comment_tail_delim_pattern "$"))
                )
                (setq needs-adjustment nil)
              (setq needs-adjustment t)
            )
          )
          (if needs-adjustment
              (progn
                (lse-skip-whitespace+empty-comments-forward)
                (lse-range:change-head leading_range (point))
              )
          )
        )
    )
  )
  leading_range
; lse_kill:adjust_fill-in_leading
)

;;; 22-Jan-1995 
(defun lse_kill:adjust_fill-in_trailer (leading_range trailer_range)
  (if lse_comment_tail_delim_pattern
      (save-excursion 
        (goto-char (lse-range:head-pos trailer_range))
        (if (looking-at (concat "[ \t\n]*" lse_comment_tail_delim_pattern))
            (let (needs-adjustment)
              (save-excursion 
                (goto-char (lse-range:head-pos leading_range))
                (if (looking-at
                       (concat "[ \t\n]*" lse_comment_head_delim_pattern)
                    )
                    (setq needs-adjustment nil)
                  (setq needs-adjustment t)
                )
              )
              (if needs-adjustment
                  (progn
                    (goto-char (lse-range:tail-pos trailer_range))
                    (lse-skip-whitespace+empty-comments-backward)
                    (lse-range:change-tail trailer_range (point))
                  )
              )
            )
        )
      )
  )
  trailer_range
; lse_kill:adjust_fill-in_leading
)

(defun lse_leading_head_position_sans_delim (fill-in_info)
  (let (result)
    (save-excursion
      (goto-char (lse-range:head-pos (lse-fill-in:range fill-in_info)))
      (if (not (eolp))
          (lse-skip-whitespace+empty-comments-backward 
               (lse-tpu:line-head-pos)
          )
      )
      (setq result (point-marker))
    )
    result
  )
; lse_leading_head_position_sans_delim
)

(defun lse_leading_head_position_with_delim (fill-in_info leading)
  (let (result)
    (save-excursion
      (goto-char (lse-range:head-pos (lse-fill-in:range fill-in_info)))
      (skip-chars-backward " \t\n")
      (setq result (point-marker))
      (if (and (re-search-backward leading nil t)
               (equal (match-end 0) (marker-position result))
          )
          (progn
            (goto-char (match-beginning 0))
            (if (equal leading "")           ; 13-Mar-1995 
                t                            ;     forward-char of Emacs 19.27 gives an error at the beginning of the buffer
              (while (looking-at leading)    ; 11-Jun-1994 
                (lse-tpu:forward-char -1)
              )
            )
            (lse-tpu:forward-char 1)                 ; 11-Jun-1994 
            (lse-skip-whitespace+empty-comments-backward 
                 (lse-tpu:line-head-pos 0)
            )
            (setq result (point-marker))
          )
      )
    )
    result
  )
; lse_leading_head_position_with_delim
)

(defun lse_leading_head_position (fill-in_info leading)
  (if leading
      (lse_leading_head_position_with_delim fill-in_info leading)
    (lse_leading_head_position_sans_delim fill-in_info)
  )
; lse_leading_head_position
)

(defun lse_trailer_tail_position_sans_delim (fill-in_info)
  (let (result)
    (save-excursion
      (goto-char (lse-range:tail-pos (lse-fill-in:range fill-in_info)))
      (if (not (bolp))
        (skip-chars-forward " \t")
      )
      (setq result (point-marker))
      (if (bolp)
          nil
        (if (integerp (lse_comment:trailer_comment_tail_position))
            (setq result (point-marker))
        )
      )
    )
    result
  )
; lse_trailer_tail_position_sans_delim
)

(defun lse_trailer_tail_position_with_delim (fill-in_info trailer)
  (let (result)
    (save-excursion
      (goto-char (lse-range:tail-pos (lse-fill-in:range fill-in_info)))
      (skip-chars-forward " \t\n")
      (if (looking-at trailer)
          (progn
            (goto-char (match-end 0))
            (lse-skip-whitespace+empty-comments-forward 
                 (lse-tpu:line-tail-pos 1)
            )
            (setq result (point-marker))
          )
        ;; 28-Jan-1995 use lse_trailer_tail_position_sans_delim instead of
        ;;             (point-marker)
        (setq result (lse_trailer_tail_position_sans_delim fill-in_info))
      )
    )
    result
  )
; lse_trailer_tail_position_with_delim
)

(defun lse_trailer_tail_position (fill-in_info trailer)
  (if trailer
      (lse_trailer_tail_position_with_delim fill-in_info trailer)
    (lse_trailer_tail_position_sans_delim fill-in_info)
  )
; lse_trailer_tail_position
)

(defun lse_kill:leading_blank_line_range (head)
  (let (p q)
    (save-excursion
      (goto-char head) 
      (setq p (point))
      (lse-skip-whitespace+empty-comments-backward 
           (lse-tpu:line-head-pos (if (eolp) 1 0))
      )
      (if (bolp)
          (setq q (1- (point))); remove leading blank line completely
        (setq q (point))
      )
      (lse-range:new p q)
    )
  )
; lse_kill:leading_blank_line_range
)

(defun lse_kill:trailing_blank_line_range (tail)
  (let (p q)
    (save-excursion
      (goto-char tail) 
      (setq p (point))
      (lse-skip-whitespace+empty-comments-forward 
           (lse-tpu:line-tail-pos (if (bolp) 1 2))
      )
      (if (eolp)
          (setq q (1+ (point)))
        (setq q p)
      )
      (lse-range:new p q)
    )
  )
; lse_kill:trailing_blank_line_range
)

(defun lse_kill:fill-in-leading (psym)
  (lse-range:new
       (lse_leading_head_position lse_current_fill-in (get psym 'leading))
       (lse-range:head (lse-fill-in:range lse_current_fill-in))
  )  
; lse_kill:fill-in-leading
)

(defun lse_kill:fill-in-trailer (psym)
  (lse-range:new
       (lse-range:tail-pos (lse-fill-in:range lse_current_fill-in))
       (lse_trailer_tail_position lse_current_fill-in (get psym 'trailer))
  )
; lse_kill:fill-in-trailer
)

(defun lse_kill:range+contents (range)
  (if (or (not range) (lse-range:is-collapsed range))
      nil
    (cons range (lse-range:contents range))
  )
; lse_kill:range+contents
)

(defun lse-kill:enclosure (psym)
  (let* ((leading_range (lse_kill:fill-in-leading psym))
         (trailer_range (lse_kill:fill-in-trailer psym))
         (head_blank_line_range 
            (lse_kill:leading_blank_line_range (lse-range:head leading_range))
         )
         (tail_blank_line_range (lse_kill:trailing_blank_line_range 
                                     (lse-range:tail-pos trailer_range)
                                )
         )
         (enclosure (make-vector 4 nil)) 
        )
    (setq leading_range
          (lse_kill:adjust_fill-in_leading leading_range trailer_range)
    )
    (setq trailer_range
          (lse_kill:adjust_fill-in_trailer leading_range trailer_range)
    )
    (if (and (>= (lse-tpu:line-head-pos) (lse-range:head-pos leading_range))
             (<= (lse-tpu:line-tail-pos) (lse-range:tail-pos trailer_range))
        )
        ;; both leading and trailer are on different than current line
        (if (string-match lse_comment_head_delim_pattern
                          (lse-range:contents-np head_blank_line_range)
            )
            (aset enclosure 3 
                  (lse_kill:range+contents tail_blank_line_range)
            )
          (aset enclosure 0
                (lse_kill:range+contents head_blank_line_range)
          )
        )
      (if (and (< (lse-tpu:line-head-pos) (lse-range:head-pos leading_range))
               (> (lse-tpu:line-tail-pos) (lse-range:tail-pos trailer_range))
          )
          ;; both leading and trailer are on current line
          (if (equal " " (buffer-substring-no-properties 
                              (1- (lse-range:tail-pos trailer_range))
                                  (lse-range:tail-pos trailer_range)
                         )
              )
              (if (not (lse-range:is-empty leading_range)); 22-Jan-1995 
                  (lse-range:change-tail trailer_range 
                                         (lse-range:tail-pos trailer_range)
                  )
              )
            (if nil ; (bolp); embedded if lets remain a single blank after killing
                (if (equal " " (buffer-substring-no-properties 
                                        (lse-range:head-pos leading_range) 
                                    (1+ (lse-range:head-pos leading_range))
                               )
                    )
                    (lse-range:change-head leading_range 
                                           (1+ (lse-range:head-pos leading_range))
                    )
                )
            )
          )
      )
    )
    (aset enclosure 1 (lse_kill:range+contents leading_range))
    (aset enclosure 2 (lse_kill:range+contents trailer_range))
    enclosure
  )
; lse-kill:enclosure
)

(defun lse@kill_current_fill-in ()
  (save-match-data 
    (let* ((psym         (lse-fill-in:symbol      lse_current_fill-in))
           (range        (lse-fill-in:range       lse_current_fill-in))
           (enclosure    (lse-kill:enclosure      psym))
           (last-fill-in-range
             (lse-fill-in:range (lse_fill-in_history:last_expansion))
           )
           tail-pos dead
          )
      (lse_fill-in_history:purge_unexpansion psym range)

      ;; originally `(mapcar    'lse-range:clean (mapcar 'car enclosure))'
      ;; was used here: in some situations (when range and enclosure[2] are
      ;; adjoining without blanks) this leads to a distortion of
      ;; `(lse-range:tail range)', so we have to do it by hand and in the
      ;; proper order (from front to back in the buffer)  1-Jun-1994 
      (lse-range:clean (car (aref enclosure 0)))
      (lse-range:clean (car (aref enclosure 1)))
      (setq dead       (lse-range:clean range))
      (lse-range:clean (car (aref enclosure 2)))
      (lse-range:clean (car (aref enclosure 3)))

      (setq lse_dead_fill-in    lse_current_fill-in)
      (setq lse_current_fill-in nil)

      (lse-fill-in:change-state          lse_dead_fill-in 'lse@dead)
      (lse-fill-in:change-complement     lse_dead_fill-in dead)
      (lse-fill-in:change-enclosure      lse_dead_fill-in enclosure)
      (lse-fill-in:change-descendants    lse_dead_fill-in nil)

      (lse_fill-in_history:add_expansion lse_dead_fill-in); 20-Mar-1995 

      (if (and last-fill-in-range
            (setq  tail-pos (lse-range:tail last-fill-in-range))
            (equal tail-pos (lse-range:head range))
          ) ;  avoid collapse of that fill-in
          (lse-range:change-tail-pos last-fill-in-range tail-pos)
      )
    )
  )
; lse@kill_current_fill-in
)

(defun lse_kill_current_fill-in ()
  (if (not (lse_current_fill-in_is_optional))
      (if (not (y-or-n-p 
                (concat "fill-in  `"
                        (lse_inside_fill-in)
                        "'  is required! Do you really want to kill it? "
                )
             )
          )
          (error "Fill-In not killed")
      )
    ;; 24-Feb-1995 kill even if not replacement is allowed
  )
  (lse@kill_current_fill-in)
; lse_kill_current_fill-in
)

(defun lse-kill:insert-enclosure (enclosure index head-pos)
  (let* ((result   head-pos)
         (contents (cdr (aref enclosure index)))
        )
    (goto-char result)
    (if contents
        (progn
          (lse-fill-in-insert contents)
          (+ result (length contents))
        )
      result
    )
  )
; lse-kill:insert-enclosure
)

(defun lse_kill_current_fill-in_if_optional ()
  (if (lse_current_fill-in_is_optional)
      (lse@kill_current_fill-in); 24-Feb-1995 kill even no-replacement fill-ins
  )
; lse_kill_current_fill-in_if_optional
)

;;; 18-Feb-1995 
(defun lse-kill-future-fill-in (name how-many)
  (let ((descendants (lse-fill-in:descendants lse_dead_fill-in)); 26-Feb-1995
       )
    (save-excursion 
      (let (lse_current_fill-in
            lse_dead_fill-in
           )
        (while (and (> how-many 0)
                    (lse-goto-next-fill-in t name)
               )
          (lse_kill_current_fill-in_if_optional)
          (lse-add-to-list descendants lse_dead_fill-in)
          (setq how-many (1- how-many))
        )
      )
      (lse-fill-in:change-descendants lse_dead_fill-in descendants)
    )
  )
; lse-kill-future-fill-in
) 

(defun lse@unkill_fill_in (lse_dead_fill-in)
  (let* ((complement               (lse-fill-in:complement lse_dead_fill-in))
         (enclosure                (lse-fill-in:enclosure  lse_dead_fill-in))
         (range                    (lse-fill-in:range      lse_dead_fill-in))
         (head-pos                 (lse-range:head-pos     range))
         next-pos
        )
    (setq next-pos (lse-kill:insert-enclosure enclosure 0 head-pos))
    (setq next-pos (lse-kill:insert-enclosure enclosure 1 next-pos))
    (setq head-pos        next-pos)
    (lse-fill-in-insert   complement)
    (setq next-pos (lse-kill:insert-enclosure enclosure 2 (point)))
    (setq next-pos (lse-kill:insert-enclosure enclosure 3 next-pos))

    (lse-range:change-head-pos range head-pos)
    (lse-range:change-tail-pos range (+ head-pos (length complement)))

    (lse-fill-in:change-state        lse_dead_fill-in 'lse@flat)
    (lse-fill-in:change-complement   lse_dead_fill-in nil)
    (lse-fill-in:change-enclosure    lse_dead_fill-in nil)
    (lse-fill-in:change-descendants  lse_dead_fill-in nil)
  )
  lse_dead_fill-in
; lse@unkill_fill_in
)

(defun lse_unkill_fill_in ()
  (if (not lse_dead_fill-in)
      (error "No fill-in to un-kill")
    (let* ((descendants (lse-fill-in:descendants lse_dead_fill-in)))
      (setq lse_current_fill-in (lse@unkill_fill_in lse_dead_fill-in))
      (setq lse_dead_fill-in    nil)
      (save-excursion 
        (mapcar 'lse@unkill_fill_in descendants)
      )
      (lse-goto-prev-fill-in)
    )
  )
; lse_unkill_fill_in
)

;;; 31-Dec-1997 
(defun lse-kill:join-sexp-boundary-maybe ()
  (if lse_dead_fill-in
      (if (not (save-excursion
                 (skip-chars-forward  " \t\n")
                 (lse_looking_at_fill-in)
               )
          ); 1-Jan-1998 ; don't join if before a flat fill-in
          (let ((s-bound   (lse-join-sexp-boundary-maybe))
                (enclosure (lse-fill-in:enclosure lse_dead_fill-in))
               )
            (if (and
                  (listp s-bound)
                  (not (car s-bound))
                  (nth 1 s-bound)
                  (not (aref enclosure 3))
                )
                (aset enclosure 3 (nth 1 s-bound))
            )
          )
      )
  )
; lse-kill:join-sexp-boundary-maybe
)

