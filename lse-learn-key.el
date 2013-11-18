;-*- coding: utf-8 -*-
 
;;;;unix_ms_filename_correspondency lse-learn-key:el lse_lrnk:el
;;;; Copyright (C) 1995 Mag. Christian Tanzer. All rights reserved.
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
;;;;    lse-learn-key
;;;;
;;;; Purpose
;;;;    Provide functions for definition of keys bound to learn sequences
;;;;
;;;; Revision Dates
;;;;     9-Jul-1995 (CT) Creation (of header comment)
;;;;--
(provide 'lse-learn-key)

(defvar lse@key-currently-learned      nil)
(defvar lse@key-currently-learned@name nil)
(defvar lse@key-currently-learned@info nil)

(defun lse-learn-key (key)
  "Define a new key in learn mode. "
  (interactive "kKey to define: ")
  (lse-learn-named-key key "")
)

(defun lse-learn-named-key (key name)
  "Define a new key in learn mode. "
  (interactive "kKey to define: \nS(quoted) Name of macro to define: ")
  (if defining-kbd-macro
      (progn 
        (message "Learn mode for key `%s' aborted" 
                 (lse-key-name lse@key-currently-learned)
        )
        (global-set-key lse@key-currently-learned nil)
        (setq lse@key-currently-learned      nil)
        (setq lse@key-currently-learned@name nil)
        (setq lse@key-currently-learned@info nil)
        (end-kbd-macro nil)
      )
  )
  (global-set-key key 'lse@end-learn-key)
  (setq lse@key-currently-learned      key)
  (setq lse@key-currently-learned@name name)
  (setq lse@key-currently-learned@info 
        (format " Learn-%s" (lse-key-name key))
  )
  (start-kbd-macro nil)
  (message "Press key `%s' to end definition" (lse-key-name key))
)

(defun lse@end-learn-key ()
  "End definition of key in learn mode."
  (interactive)
  (end-kbd-macro nil)
  (global-set-key lse@key-currently-learned last-kbd-macro)
  (if (and lse@key-currently-learned@name 
           (not (string= lse@key-currently-learned@name ""))
      )
      (name-last-kbd-macro lse@key-currently-learned@name)
  )
  (message "Key `%s' successfully defined" 
           (lse-key-name lse@key-currently-learned)
  )
  (setq lse@key-currently-learned      nil)
  (setq lse@key-currently-learned@name nil)
  (setq lse@key-currently-learned@info nil)
)
