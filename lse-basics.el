;-*- unibyte: t; coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work

;;;;unix_ms_filename_correspondency lse-basics:el lse_bscs:el
;;;; Copyright (C) 1995-2007 Mag. Christian Tanzer. All rights reserved.
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
;;;;    lse-basics
;;;;
;;;; Purpose
;;;;    Functions for insertion into flat fill-in's
;;;;
;;;; Revision Dates
;;;;    19-Mar-1995 (CT) Creation (of comment)
;;;;    19-Mar-1995 (CT) lse-use-lse-self-insert-command and
;;;;                     lse-use-emacs-self-insert-command defined as no-op's
;;;;     4-Oct-1996 (CT) Define buffer-substring-no-properties if not fboundp
;;;;                     (accommodate older Emacs versions)
;;;;     7-Oct-1996 (CT) Don't redefine 'insert (hooks take care of that)
;;;;     7-Oct-1996 (CT) lse-self-insert-command commented out
;;;;     7-Oct-1996 (CT) lse-insert              commented out
;;;;    13-Dec-1997 (CT) chars-in-string added for compatibility with 19.n
;;;;    29-Dec-1997 (CT) defsubst        added
;;;;    30-Dec-1997 (CT) lse-safe        added
;;;;    15-Oct-2007 (CT) Cruft removed (lse-insert, ...)
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-basics)

(defvar lse@emacs-insert@replaced nil)

(defun lse-initialization ()
  (if (not lse@emacs-insert@replaced)
      (progn
        (setq lse@emacs-insert@replaced t)
        (fset 'lse-fill-in-insert   (symbol-function 'insert))
        ;;  7-Oct-1996 ;; not needed anymore ;; (fset 'insert 'lse-insert)
      )
  )
)
(lse-initialization)

(if (fboundp 'buffer-substring-no-properties)
    nil
 (fset 'buffer-substring-no-properties (symbol-function 'buffer-substring))
)

(if (fboundp 'chars-in-string)
    nil
  ;;; 13-Dec-1997
  (fset 'chars-in-string (symbol-function 'length))
)

(if (fboundp 'defsubst)
    nil
  ;;; 29-Dec-1997
  (fset 'defsubst (symbol-function 'defun))
)

;;; 30-Dec-1997 ;;; stolen from cc-defs.el
(defmacro lse-safe (&rest body)
  ;; safely execute BODY, return nil if an error occurred
  (` (condition-case nil
	 (progn (,@ body))
       (error nil)
     )
  )
)

