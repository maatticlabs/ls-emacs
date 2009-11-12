;-*- unibyte: t; coding: iso-8859-1; -*-

;;;; Copyright (C) 2009 Mag. Christian Tanzer. All rights reserved
;;;; Glasauergasse 32, A--1130 Wien, Austria. tanzer@swing.cluster
;;;; ****************************************************************************
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;; ****************************************************************************
;;;;
;;;;++
;;;; Name
;;;;    lse-site-info
;;;;
;;;; Purpose
;;;;    Define site-specific info for LSE
;;;;
;;;;    You can override this file by placing a modified copy at the front of
;;;;    load-path
;;;;
;;;; Revision Dates
;;;;    12-Nov-2009 (CT) Creation
;;;;    ««revision-date»»···
;;;;--

;;; 12-Nov-2009
(defvar lse-site-info
  (lse-new-site-info
    (list
      ( cons 'company-address "Glasauergasse 32, A--1130 Wien, Austria")
      ( cons 'company-e-mail  "tanzer@swing.co.at")
      ( cons 'company-name    "Mag. Christian Tanzer")
      ( cons 'user-map:initials
          (lse-new-site-info
            '( ("froelich"    . "EFE")
               ("rainer"      . "RH")
             )
          )
      )
      ( cons 'user-map:name
          (lse-new-site-info
            '( ("root"        . "Christian R. Tanzer")
             )
          )
      )
      ( cons 'system-domain   "swing.co.at")
    )
  )
)

;;; __END__ lse-site-info.el
