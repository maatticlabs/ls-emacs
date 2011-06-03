;-*- coding: iso-8859-15; -*-

;;;; Copyright (C) 2011 Mag. Christian Tanzer All rights reserved
;;;; Glasauergasse 32, A--1130 Wien, Austria. tanzer@swing.co.at
;;;; #*** <License> ************************************************************#
;;;; This library is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this library. If not, see <http://www.gnu.org/licenses/>.
;;;; #*** </License> ***********************************************************#
;;;;
;;;;++
;;;; Name
;;;;    lse-w32
;;;;
;;;; Purpose
;;;;    Customize LS-Emacs for Win32 system
;;;;
;;;; Revision Dates
;;;;     3-Jun-2011 (CT) Creation (factored from ls-emacs.el)
;;;;    ««revision-date»»···
;;;;--

(provide 'lse-w32)

(setq w32-scroll-lock-modifier nil)
(setq w32-lwindow-modifier 'super)
(setq w32-rwindow-modifier 'meta)
(setq w32-alt-is-meta nil)
(setq w32-apps-modifier 'meta)

;;;; __END__ lse-w32
