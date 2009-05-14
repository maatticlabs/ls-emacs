;-*- unibyte: t; coding: iso-8859-1; -*-

;;;; Copyright (C) 2009 Mag. Christian Tanzer. All rights reserved
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
;;;;    ««text»»···
;;;;
;;;; Revision Dates
;;;;    14-May-2009 (CT) Creation
;;;;    ««revision-date»»···
;;;;--
(osx-key-mode nil)

(defvar lse-keys:function-key-map-bindings
   '( ([f11]        [blue])
      ([f12]        [gold])
    )
)

(setq mac-command-modifier 'meta)
(setq mac-option-modifier  'alt)
(setq  osx-key-mode-map (make-osx-key-mode-map))

(defvar lse-tpu:electric-inserts-p t)
(defvar lse-tpu:use-control-keys-p t)

(setq load-path
  (append (list "/Developer/Tools/ls-emacs" "/Developer/Tools/ls-emacs/lsc") load-path)
)

(setq lse-directory        "/Developer/Tools/ls-emacs/lse")
(setq lse-src-directory    "/Developer/Tools/ls-emacs/lse")
(setq lse-script-directory "/Developer/Tools/ls-emacs/scripts")

;;;; __END__ lse-config.el
