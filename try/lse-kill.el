;;;; (c) 1994 Swing Informationssysteme GmbH. All rights reserved.
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
;;;;-- 
(provide 'lse-kill)

;;; 22-Jan-1995 
(defun lse_kill:adjust_fill-in_leading (leading_range trailer_range)
  (save-excursion 
    (goto-char (lse-range:head-pos leading_range))
    (if (looking-at (concat "[ \t\n]*" lse_comment_head_delim_pattern))
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
            (while (looking-at leading)    ; 11-Jun-1994 
              (forward-char -1)
            )
            (forward-char 1)               ; 11-Jun-1994 
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
        (setq result (point-marker))
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
                          (lse-range:contents head_blank_line_range)
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
          (if (equal " " (buffer-substring 
                              (1- (lse-range:tail-pos trailer_range))
                                  (lse-range:tail-pos trailer_range)
                         )
              )
              (lse-range:change-tail trailer_range 
                                     (lse-range:tail-pos trailer_range)
              )
            (if nil ; (bolp); embedded if lets remain a single blank after killing
                (if (equal " " (buffer-substring 
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

      (lse-fill-in:change-state      lse_dead_fill-in 'lse@dead)
      (lse-fill-in:change-complement lse_dead_fill-in dead)
      (lse-fill-in:change-enclosure  lse_dead_fill-in enclosure)

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
    (if (not (lse-replacement@allowed))
        (error "Cannot kill fill-in `%s'" (lse_inside_fill-in))
    )
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
      (if (lse-replacement@allowed)
          (lse@kill_current_fill-in)
      )
  )
; lse_kill_current_fill-in_if_optional
)

(defun lse_unkill_fill_in ()
  (if (not lse_dead_fill-in)
      (error "No fill-in to un-kill")
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
      
      (lse-fill-in:change-state      lse_dead_fill-in 'lse@flat)
      (lse-fill-in:change-complement lse_dead_fill-in nil)
      (lse-fill-in:change-enclosure  lse_dead_fill-in nil)
      (setq lse_current_fill-in      lse_dead_fill-in)
      (setq lse_dead_fill-in         nil)

      (lse-goto-prev-fill-in)
    )
  )
; lse_unkill_fill_in
)

