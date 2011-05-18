;-*- coding: iso-8859-1; -*-

;;;;unix_ms_filename_correspondency swing-keys.el swi_keys.el
;;;; Copyright (C) 1994 Mag. Christian Tanzer. All rights reserved.
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
;;;;    swing-keys
;;;;
;;;; Purpose
;;;;    Key definitions for swing emacs lse.
;;;;
;;;; Revision Dates
;;;;    26-May-1994 (CT) Creation (of comment)
;;;;    26-May-1994 (CT) Bindings for lse-expand-or-tabulator and lse-open-line added
;;;;    29-May-1994 (CT) Require-List moved to swing-default
;;;;    29-May-1994 (CT) Split into swing-keys-v18 and swing-keys-v19
;;;;    11-Jun-1994 (CT) swing-describe-key-briefly  added
;;;;    12-Jun-1994 (CT) swing-insert-key-definition added
;;;;    31-Aug-2002 (CT) `swing-redefine-std-emacs-keys` removed
;;;;    ««revision-date»»···
;;;;--

(if lse-emacs19-p
    (load "swing-keys-v19")
  (load "swing-keynames")
  (load "swing-keys-v18")
)

(swing-define-gold-keys)
(swing-define-red-keys)
(swing-define-blue-tab-keys)

(defun swing-describe-key-briefly (prefix key)
  "Show definition of key in message window. With prefix the definition is inserted into current buffer"
  (interactive "P\nkKey to describe ")
  (if prefix
      (swing-insert-key-definition key)
    (describe-key-briefly key)
  )
; swing-describe-key-briefly
)
