;-*- coding: utf-8 -*-

;;;; Copyright (C) 2009-2014 Mag. Christian Tanzer. All rights reserved
;;;; Glasauergasse 32, A--1130 Wien, Austria. tanzer@swing.co.at
;;;; ****************************************************************************
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the Free
;;;; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;; ****************************************************************************
;;;;
;;;;++
;;;; Name
;;;;    lse-macosx
;;;;
;;;; Purpose
;;;;    Customizations of LS-Emacs for Mac OSX
;;;;
;;;; Revision Dates
;;;;    14-May-2009 (CT) Creation
;;;;     3-Jun-2011 (CT) `provide` added
;;;;     7-Nov-2014 (CT) Add guard
;;;;    12-Dec-2014 (CT) Remove Aquamacs specific code
;;;;    12-Dec-2014 (CT) Add `lse-macosx:define-keys:german-keyboard`
;;;;    ««revision-date»»···
;;;;--

(provide 'lse-macosx)

;;; http://emacsformacosx.com/tips

(setq
  mac-command-modifier 'meta
  mac-option-modifier  'alt
)

(defvar lse-keys:function-key-map-bindings
  '(
    ([f11]        [blue])
    ([f12]        [gold])
   )
)

;;; 12-Dec-2014
(defvar lse-macosx:german-keyboard:bindings
  '(
    ([?\A-l] [?\@])
    ([?\A-5] [?\[])
    ([?\A-6] [?\]])
    ([?\A-7] [?\|])
    ([?\A-8] [?\{])
    ([?\A-9] [?\}])
    ([?\A-/] [?\\])
   )
)

(defun lse-macosx:define-keys:german-keyboard ()
  (lse-keys/define 'lse-key/define-in-function-key-map
    lse-macosx:german-keyboard:bindings
  )
; lse-macosx:define-keys:german-keyboard
)

;;;; __END__ lse-macosx.el
