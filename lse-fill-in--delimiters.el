;-*-unibyte: t;-*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work
 
;;;;unix_ms_filename_correspondency lse-fill-in--delimiters:el lse_fidl:el
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
;;;;    lse-fill-in--delimiters
;;;;
;;;; Purpose
;;;;    Define variables and functions for dealing with fill-in delimiters
;;;;
;;;; Revision Dates
;;;;    12-Jun-1994 (CT) Creation (of comment)
;;;;                     Error in lse_opt_fill-in_pattern and 
;;;;                     lse_req_fill-in_pattern corrected
;;;;    27-Jul-1994 (CT) 7-bit delimiters added as comment
;;;;-- 
(provide 'lse-fill-in--delimiters)

;;; required and optional head delimiters must start with the same character!!!
(defconst lse_fill-in-not_head_start_chars   "^«")

(defconst lse_fill-in_head_delim_chars       "«¦")
(defconst lse_fill-in_tail_delim_chars       "¦»·")
(defconst lse_fill-in_name_delim_chars       "«»¦")
(defconst lse_fill-in_delim_chars            "«¦»·")

(defconst lse_req_fill-in_head_delim         "«")  
(defconst lse_opt_fill-in_head_delim         "««")
(defconst lse_req_fill-in_tail_delim         "»")
(defconst lse_opt_fill-in_tail_delim         "»»")

(defconst lse_no_replacement_fill-in_marker  "¦")
(defconst lse_list_fill-in_trailer           "···")

;;; take care of characters which must be quoted inside Emacs patterns!!!
(defconst lse_req_fill-in_head_delim_pattern "«¦?")
(defconst lse_req_fill-in_tail_delim_pattern "¦?»")
(defconst lse_opt_fill-in_head_delim_pattern "««¦?")
(defconst lse_opt_fill-in_tail_delim_pattern "¦?»»")
(defconst lse_list_fill-in_trailer_pattern   "\\(···\\)?")

;;;+
;;; If you want to use 7-bit delimiters, uncomment the following lines
;;;-
;; (defconst lse_fill-in_head_delim_chars       ":<|")
;; (defconst lse_fill-in_tail_delim_chars       "|>:.")
;; (defconst lse_fill-in_name_delim_chars       "<>|")
;; (defconst lse_fill-in_delim_chars            ":<|>.")

;; (defconst lse_req_fill-in_head_delim         ":<")  
;; (defconst lse_opt_fill-in_head_delim         ":<<")
;; (defconst lse_req_fill-in_tail_delim         ">:")
;; (defconst lse_opt_fill-in_tail_delim         ">>:")

;; (defconst lse_list_fill-in_trailer           "...")

;;; take care of characters which must be quoted inside Emacs patterns!!!
;; (defconst lse_req_fill-in_head_delim_pattern ":<|?")
;; (defconst lse_req_fill-in_tail_delim_pattern "|?>:")
;; (defconst lse_opt_fill-in_head_delim_pattern ":<<|?")
;; (defconst lse_opt_fill-in_tail_delim_pattern "|?>>:")
;; (defconst lse_list_fill-in_trailer_pattern   "\\(\\.\\.\\.\\)?")
;;;+
;;; end of 7-bit delimiters
;;;-

;; remove `;;' if you want to define langauge-specific fill-in delimiters
;; but you shouldn't do that!!
;; (make-variable-buffer-local 'lse_fill-in-not_head_start_chars)
;; (make-variable-buffer-local 'lse_fill-in_head_delim_chars)
;; (make-variable-buffer-local 'lse_fill-in_delim_chars)
;; (make-variable-buffer-local 'lse_req_fill-in_head_delim) 
;; (make-variable-buffer-local 'lse_opt_fill-in_head_delim)
;; (make-variable-buffer-local 'lse_req_fill-in_tail_delim)
;; (make-variable-buffer-local 'lse_opt_fill-in_tail_delim)
;; (make-variable-buffer-local 'lse_list_fill-in_trailer)
;; (make-variable-buffer-local 'lse_req_fill-in_head_delim_pattern)
;; (make-variable-buffer-local 'lse_req_fill-in_tail_delim_pattern)
;; (make-variable-buffer-local 'lse_opt_fill-in_head_delim_pattern)
;; (make-variable-buffer-local 'lse_opt_fill-in_tail_delim_pattern)
;; (make-variable-buffer-local 'lse_list_fill-in_trailer_pattern)
 
(defun lse_fill-in_not_in_name_chars ()
  (concat 
     "\000-\037\177"                      ; control characters
     lse_fill-in_name_delim_chars         ; fill-in delimiters
  )
)

(defun lse_fill-in_name_pattern ()
  (concat
     "\\([^"                              ; any characters except
     (lse_fill-in_not_in_name_chars)      ; those not allowed in fill-in names
     "]+\\)"                              ; at least 1 time!
  )
)

(defun lse_opt_fill-in_pattern (&optional name)
  (if name
      (setq name (concat "\\(" (regexp-quote name) "\\)")) ; 12-Jun-1994 \\(\\)
    (setq name (lse_fill-in_name_pattern))
  )
  (concat
     lse_opt_fill-in_head_delim_pattern
     name 
     lse_opt_fill-in_tail_delim_pattern
     lse_list_fill-in_trailer_pattern
  )
) 

(defun lse_req_fill-in_pattern (&optional name)
  (if name
      (setq name (concat "\\(" (regexp-quote name) "\\)")) ; 12-Jun-1994 \\(\\)
    (setq name (lse_fill-in_name_pattern))
  )
  (concat
     lse_req_fill-in_head_delim_pattern
     name 
     lse_req_fill-in_tail_delim_pattern
     lse_list_fill-in_trailer_pattern
  )
) 
