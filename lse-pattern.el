;-*- coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work

;;;;unix_ms_filename_correspondency lse-pattern:el lse_ptrn:el

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

(defconst lse-pattern:args-of-defun  "\\((.*)\\)")

(defconst lse-pattern:head-of-defun
  (concat "\\(^(defun +"                ; \\1 entire head of defun
             "\\([^ \t\n]+\\) +\n? *"   ; \\2   name      of defun
             lse-pattern:args-of-defun  ; \\3   arguments of defun
             " *\n"
          "\\)"
  )
)

(defconst lse-pattern:body-of-defun
  (concat "\\("                        ; \\1 entire body
            "\\("                      ;     \\2
            "^[^)].*\n"                ;        a single line
            "\\)+"                     ;   at least one line
          "\\)"
  )
)

(defconst lse-pattern:tail-of-defun
  "\\(^).*\\)"                         ; \\1 the entire line
)

; (lse-tpu:search-forward lse-pattern:args-of-defun)
; (lse-tpu:search-forward lse-pattern:head-of-defun)
; (lse-tpu:search-forward lse-pattern:tail-of-defun)

