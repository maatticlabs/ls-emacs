;-*- unibyte: t; coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work

;;;;unix_ms_filename_correspondency lse-fill-in--search:el lse_fisr:el
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
;;;;    lse-fill-in--search
;;;;
;;;; Purpose
;;;;    Functions for searching of and moving to fill-ins
;;;;
;;;; Revision Dates
;;;;    24-May-1994 (CT) Creation (of comment)
;;;;    24-May-1994 (CT) lse-range:name-range renamed to lse-range:inner-range
;;;;                     lse_last_position made buffer-local
;;;;    25-May-1994 (CT) buffer-boundary parameter to avoid boundary errors
;;;;    26-May-1994 (CT) Interactive functions moved to lse-interactive
;;;;    27-Jun-1994 (CT) lse_inside_fill-in: take care of
;;;;                     lse-flat-fill-in:open-replacement
;;;;    23-Jan-1995 (CT) Error corrected
;;;;                       (concat lse_comment_delim_char_set " \t\n") instead
;;;;                       of (concat " \t\n" lse_comment_delim_char_set)
;;;;    19-Mar-1995 (CT) Don't call lse-use-lse-self-insert-command and
;;;;                     lse-use-emacs-self-insert-command
;;;;                     lse-flat-fill-in:highlight-current called instead
;;;;    25-Mar-1995 (CT) lse-fill-in:*highlight-current renamed to
;;;;                     lse-flat-fill-in:*highlight-current
;;;;    15-Oct-2007 (CT) Cruft removed
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-fill-in--search)

(defvar                      lse_last_position nil)
(make-variable-buffer-local 'lse_last_position)

(defun lse-skip-whitespace+empty-comments-forward (&optional limit)
  (skip-chars-forward (concat lse_comment_delim_char_set " \t\n") limit)
)

(defun lse-skip-whitespace+empty-comments-backward (&optional limit)
  (skip-chars-backward (concat lse_comment_delim_char_set " \t\n") limit)
)


(defun lse_looking_at_fill-in (&optional name)
  ;; precondition: (point) on first character of a fill-in head delimiter
  (if (looking-at lse_opt_fill-in_head_delim_pattern)
      (looking-at (lse_opt_fill-in_pattern name))
    (looking-at (lse_req_fill-in_pattern name))
  )
)

(defun lse_current_fill-in_is_optional ()
  (let (result)
    (if lse_current_fill-in
        (save-excursion
          (goto-char
             (lse-range:head-pos (lse-fill-in:range lse_current_fill-in))
          )
          (setq result (looking-at lse_opt_fill-in_head_delim_pattern))
        )
    )
    result
  )
)

(defun lse_current_fill-in_repeats ()
  (lse-fill-in:is-list lse_current_fill-in)
)

(defmacro lse_search_fill-in:backward_once (&optional name)
  (skip-chars-backward lse_fill-in-not_head_start_chars)
  (skip-chars-backward lse_fill-in_head_delim_chars)
  (` (lse_looking_at_fill-in (, name)))
)

(defun lse_search_fill-in:backward (&optional name)
  (let (result)
    (while (not (or result (bobp)))
      (setq result (lse_search_fill-in:backward_once name))
      (if (not (bobp)) (lse-tpu:forward-char -1))
    )
    result
   )
)

(defmacro lse_search_fill-in:forward_once (&optional name)
  (skip-chars-forward  lse_fill-in-not_head_start_chars)
  (skip-chars-backward lse_fill-in_head_delim_chars)
  (` (lse_looking_at_fill-in (, name)))
)

(defun lse_search_fill-in:forward (&optional name)
  (let (result)
    (while (not (or result (eobp)))
      (setq result (lse_search_fill-in:forward_once name))
      (skip-chars-forward lse_fill-in_head_delim_chars)
    )
    result
  )
)

(defun lse@check_fill-in ()
  ;; returns a fill-in_info if matched fill-in has a definition
  ;; this function assumes that the current match data are set-up to match
  ;; something complying with lse_@_fill-in_pattern
  (let ((head (match-beginning 1))
        (tail (match-end 1))
        result
        pdef
       )
    (setq result (and head tail (buffer-substring-no-properties head tail)))
    (if (setq pdef (lse_fill-in:definition result))
        (setq result
              (lse-fill-in:new
                pdef                                              ; symbol
                result                                            ; name
                'lse@flat                                         ; state
                nil                                               ; fill-type
                (lse-range:new (match-beginning 0) (match-end 0)) ; range
                (lse-range:new head                tail)          ; inner-range
                nil                                               ; complement
                (if (match-beginning 2)                           ; duplicate
                    (lse-range:new (match-beginning 2) (match-end 2))
                  nil
                )
              )
        )
      (setq result nil)
    )
    result
  )
)

(defun lse@goto_fill-in (search-fct buffer-boundary &optional name)
  ;; may be called by lse_goto_fill-in only !!!
  (let (result
       )
    (while (not (or result (funcall buffer-boundary)))
      (if (setq result (funcall search-fct name))
          (progn
            (setq result (lse@check_fill-in))
            (if result
                (progn
                  (lse-flat-fill-in:unhighlight-current)
                  (setq lse_current_fill-in result)
                  (setq result (lse-fill-in:name result))
                  (goto-char (match-beginning 1))
                  (lse-flat-fill-in:highlight-current)
                )
              ;; match does not contain a valid fill-in
              (if name (error "Undefined fill-in name"))
            )
          )
      )
    )
    result
  )
; lse@goto_fill-in
)

(defun lse_goto_fill-in (search-fct buffer-boundary &optional name)
  ;; search-fct has to be either lse_search_fill-in:backward or
  ;;                             lse_search_fill-in:forward
  ;; buffer-boundary has to be either bobp or eobp
  (let (result
        (last-pos (point-marker))
        (saved_lse_last_position lse_last_position)
       )
    (save-match-data
       (setq lse@active@in@buffer t)
       (setq lse_last_position last-pos)
       (setq result (lse@goto_fill-in search-fct buffer-boundary name))
    )
    ; set self-insert-command if result
    (if result
        t; 19-Mar-1995 ; (lse-use-lse-self-insert-command)
      (goto-char last-pos)
      (setq lse_last_position saved_lse_last_position)
    )
    result
  )
)

(defun lse_inside_fill-in ()
  (let (result
        (cp (point))
       )
    (cond ((eq lse-flat-fill-in:open-replacement 'lse@expanded); 27-Jun-1994
           (setq result nil) ; expansion is pending              27-Jun-1994
          )
          ((eq lse-flat-fill-in:open-replacement 'lse@replaced);  4-Jul-1994
           (setq result nil) ; replacement is pending             4-Jul-1994
          )
          (t
           (lse_shut_fill-in_replacement/if_outside)
           (save-match-data
              (cond
                ((and lse_current_fill-in
                      (lse-range:inside
                          (lse-fill-in:inner-range lse_current_fill-in)
                      )
                 )
                   ;; check if the current fill-in is still valid
                   (save-excursion
                     (goto-char
                          (lse-range:head-pos
                               (lse-fill-in:inner-range lse_current_fill-in)
                          )
                     )
                     (setq result (lse-fill-in:name lse_current_fill-in))
                     (if (looking-at (regexp-quote result))
                         nil; current fill-in is still valid
                       (lse-flat-fill-in:unhighlight-current)
                       (setq lse_current_fill-in nil)
                       (setq result (lse_inside_fill-in))
                     )
                   )
                   ;; we are already inside an flat fill-in
                  )
                (t ;; outside the currently considered flat fill-in
                   (save-excursion
                     (let (found)
                       (save-restriction
                         (narrow-to-region
                              (lse-tpu:line-head-pos) (lse-tpu:line-tail-pos)
                         )
                         (if (lse_search_fill-in:backward_once) (setq found t))
                       )
                       (if found
                           (let ((current (lse@check_fill-in)))
                             (if (not current)
                                 (setq result nil)
                               (lse-flat-fill-in:unhighlight-current)
                               (setq lse_current_fill-in current)
                               (if (or (lse-range:inside
                                          (lse-fill-in:inner-range current) cp
                                       )
                                       (bobp)
                                   )
                                   (setq result (lse-fill-in:name current))
                                 (setq result nil)
                               )
                             )
                           )
                       )
                     )
                   )
                ); t
              ); cond
              (if result
                  (progn
                    (setq lse@active@in@buffer t)
                    (lse-flat-fill-in:highlight-current)
                  )
              )
           )
          )
    ); cond
    result
  )
; lse_inside_fill-in
)


(defun lse-goto-@-fill-in (search-fct buffer-boundary &optional quiet name)
  (lse_shut_fill-in_replacement)
  (let ((result (lse_goto_fill-in search-fct buffer-boundary name)))
    (or quiet result (lse-message "No more fill-ins"))
    result
  )
)

;;; positions to first fill-in of range if any or stays at previous position
(defun lse_goto_first_fill-in_of_range (head tail)
  (if (and (integer-or-marker-p head)
           (integer-or-marker-p tail)
           (> tail head)
      )
      (let ((first-fill-in lse_current_fill-in)
            (cp            (point))
           )
        (save-restriction
          (narrow-to-region head tail)
          (goto-char head)
          (let ((lse_current_fill-in first-fill-in))
            (if (lse-goto-next-fill-in t)
                (setq first-fill-in lse_current_fill-in)
              (goto-char cp)
            )
          )
        )
        (setq lse_current_fill-in first-fill-in)
      )
  )
)


