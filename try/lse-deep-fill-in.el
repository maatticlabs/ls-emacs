;;;; (c) 1994 Swing Informationssysteme GmbH. All rights reserved.
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
;;;;-- 
(provide 'lse-deep-fill-in)

;;;++
;;; Internals for un/re-expansion
;;;--
(defun lse_toggle_fill-in_expansion (toggle_fill-in)
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
                (lse-fill-in:descendents last_fill-in)
        )
      )
    )
  )
)
