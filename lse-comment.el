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
;;;;    lse-comment
;;;;
;;;; Purpose
;;;;    Define variables and functions for handling of comments
;;;;
;;;; Revision Dates
;;;;     9-Jul-1995 (CT) Creation (of header comment)
;;;;     2-Jan-1998 (CT) `lse-comment:insidep' added
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-comment)

(require 'lse-indent)

(defvar                      lse-comment:head_delim ";")
(defvar                      lse-comment:tail_delim nil)
(defvar                      lse-comment:head_delim_pattern ";+ *")
(defvar                      lse-comment:tail_delim_pattern nil)
(defvar                      lse-comment:delim_char_set ";"); for use in pattern character sets
(make-variable-buffer-local 'lse-comment:head_delim)
(make-variable-buffer-local 'lse-comment:tail_delim)
(make-variable-buffer-local 'lse-comment:head_delim_pattern)
(make-variable-buffer-local 'lse-comment:tail_delim_pattern)
(make-variable-buffer-local 'lse-comment:delim_char_set)

(defun lse-comment:leading_comment_head_position ()
  (let ((search-limit (point)) ; (lse-tpu:line-head-pos) for re-search-backward
       )
    (if (and lse-comment:head_delim_pattern
             ;; 18-May-1994 search forward instead of backward to catch all
             ;; comment characters
             (or (beginning-of-line) t)
             (re-search-forward  lse-comment:head_delim_pattern search-limit t)
        )
        (point)
      nil
    )
  )
)

(defun lse-comment:setup_expansion_leading ()
  (if lse-comment:head_delim_pattern
      (save-match-data
        (save-excursion
          (if (lse-comment:leading_comment_head_position)
              (progn
                (setq lse::expansion-line-leading
                      (buffer-substring-no-properties
                          (match-beginning 0) (match-end 0)
                      )
                )
                (goto-char (match-beginning 0))
                (setq lse::expansion-line-leading-indent (current-column))
              )
          )
        )
      )
  )
)

(defun lse-comment:trailer_comment_tail_position ()
  (if (and lse-comment:tail_delim_pattern
           (re-search-forward lse-comment:tail_delim_pattern
                              (lse-tpu:line-tail-pos)
                              t
           )
      )
      (point)
    nil
  )
)

(defun lse-comment:setup_expansion_trailer ()
  (if lse-comment:tail_delim_pattern
      (save-match-data
        (save-excursion
          (if (lse-comment:trailer_comment_tail_position)
              (progn
                (setq lse::expansion-line-trailer
                      (buffer-substring-no-properties
                          (match-beginning 0) (match-end 0)
                      )
                )
                (setq lse::expansion-line-trailer-indent
                      (- (point) (match-beginning 0))
                )
              )
          )
        )
      )
  )
)

;;;  2-Jan-1998
(defun lse-comment:insidep (start)
  "Returns true if point is inside a comment.
START is the point to start parsing."
  (let ((state (parse-partial-sexp start (point))))
    (if (nth 4 state)
        (if (nth 7 state)
            'secondary
          'primary
        )
    )
  )
; lse-comment:insidep
)
