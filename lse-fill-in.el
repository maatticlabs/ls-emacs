;-*-unibyte: t;-*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work

;;;;unix_ms_filename_correspondency lse-fill-in:el lse_fiin:el
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
;;;;    lse-fill-in
;;;;
;;;; Purpose
;;;;    Provide functions needed for manipulation of flat (i.e., unexpanded),
;;;;    deep (i.e., expanded), and dead (i.e., killed) fill-ins
;;;;
;;;; Revision Dates
;;;;    24-May-1994 (CT) Creation (of comment)
;;;;    25-May-1994 (CT) Clarification
;;;;    24-May-1994 (CT) $$default$$separator used instead of literal
;;;;                     lse-newline-and-indent
;;;;    19-Mar-1995 (CT) lse-fill-in:highlight-current and
;;;;                     lse-fill-in:unhighlight-current added
;;;;    25-Mar-1995 (CT) lse-fill-in:highlight-current and
;;;;                     lse-fill-in:unhighlight-current moved to
;;;;                     lse-flat-fill-in (and renamed correspondingly)
;;;;    25-Mar-1995 (CT) Adapted to highligthing by overlays (no need for
;;;;                     removing text properties)
;;;;    13-Oct-1996 (CT) lse-fill-in:add-text-properties and
;;;;                     lse-fill-in:remove-text-properties defined
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-fill-in)

(defun lse-fill-in:expand-separator (fill-in_separator ref-pos)
  (lse-flat-fill-in:interpret-replacement_in_env
      (lse_fill-in:definition
        (if fill-in_separator
            (symbol-name fill-in_separator)
          "$$default$$separator"
        )
      )
      ref-pos
  )
; lse-fill-in:expand-separator
)

(defun lse_duplicate_current_fill-in (psym name &optional ref-pos)
  (let ((fill-in_separator (or lse@expansion@separator (get psym 'separator)))
        (br                (point-marker))
        dupl-range
       )
    (save-excursion
      (lse-fill-in:expand-separator fill-in_separator ref-pos)
      (lse-fill-in-insert   lse_opt_fill-in_head_delim
                            name
                            lse_opt_fill-in_tail_delim
                            lse_list_fill-in_trailer
      )
      (setq dupl-range (lse-range:new br (point-marker)))
    )
    dupl-range
  )
; lse_duplicate_current_fill-in
)

(defun lse_duplicate_current_fill-in_maybe (psym name &optional ref-pos)
  (if (lse_current_fill-in_repeats)
      (lse_duplicate_current_fill-in psym name ref-pos)
  )
)

(defun lse_convert_current_fill-in_to_duplicate (psym flat-range)
  (let ((fill-in_separator (or lse@expansion@separator(get psym 'separator)))
        (br                (point-marker))
        dupl-range
       )
    (save-excursion
      (lse-fill-in:expand-separator fill-in_separator nil)
      (if (looking-at lse_opt_fill-in_head_delim_pattern)
          nil
        (delete-char (chars-in-string lse_req_fill-in_head_delim))
        (lse-fill-in-insert  lse_opt_fill-in_head_delim)
        (skip-chars-forward  (concat "^" (lse_fill-in_not_in_name_chars)))
        (delete-char (chars-in-string lse_req_fill-in_tail_delim))
        (lse-fill-in-insert  lse_opt_fill-in_tail_delim)
        (skip-chars-forward  lse_fill-in_tail_delim_chars)
      )
      (setq dupl-range
            (lse-range:new br (lse-range:tail-pos flat-range))
      )
    )
    dupl-range
  )
)

;;; 13-Oct-1996
(defun lse-fill-in:add-text-properties (head tail props)
  (let ((buffer-undo-list   t); don't want property change on buffer-undo-list
        (inhibit-read-only t)
        (bmp                (buffer-modified-p))
       )
    (add-text-properties head tail props)
    (set-buffer-modified-p bmp)
  )
; lse-fill-in:add-text-properties
)

;;; 13-Oct-1996
(defun lse-fill-in:remove-text-properties (head tail props)
  (let ((buffer-undo-list   t); don't want property change on buffer-undo-list
        (inhibit-read-only t)
        (bmp                (buffer-modified-p))
       )
    (remove-text-properties head tail props)
    (set-buffer-modified-p bmp)
  )
; lse-fill-in:remove-text-properties
)
