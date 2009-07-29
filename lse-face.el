;-*- unibyte: t; coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work

;;;;unix_ms_filename_correspondency lse-face:el lse_face:el
;;;; Copyright (C) 1995-2009 Mag. Christian Tanzer. All rights reserved.
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
;;;;    lse-face
;;;;
;;;; Purpose
;;;;    Functions and variables for dealing with faces (vulgo fonts)
;;;;
;;;; Revision Dates
;;;;    19-Mar-1995 (CT) Creation
;;;;    20-Mar-1995 (CT) Color of lse-face:search-match changed to `light blue'
;;;;     1-Apr-1996 (CT) lse-face:search-match-bg added
;;;;     9-Apr-1996 (CT) lse-face:search-match-bg changed from `yellow' to
;;;;                     `magenta' (otherwise clash with completion foreground)
;;;;    10-Apr-1996 (CT) lse-face:search-match-bg changed back from `magenta'
;;;;                     to `yellow'
;;;;                     Background and foreground colors for
;;;;                     lse-face:completion interchanged to avoid clash of
;;;;                     yellow foreground with yellow background
;;;;    27-Sep-1996 (CT) Check for color display
;;;;     2-Oct-1996 (CT) lse-face:search-match changed from `light blue' to
;;;;                     `cyan'
;;;;    13-Oct-1996 (CT) lse-face:current-fill-in-«type» added
;;;;    16-Oct-1996 (CT) lse-face:completion-m added
;;;;    28-Dec-1997 (CT) Background color for modeline changed (from default
;;;;                     `black' to `Grey')
;;;;     3-Apr-2003 (CT) Faces for lse-cal added
;;;;     4-Apr-2003 (CT) `lse-face:define` factored and used
;;;;     5-Apr-2003 (CT) Esthetics
;;;;    20-Apr-2003 (CT) `lse-face:cal:holiday` added
;;;;    21-Apr-2003 (CT) Some colors of lse-face:cal:-faces changed
;;;;    20-Nov-2003 (CT) `lse-face:current-line-hl` added
;;;;     3-Mar-2008 (CT) Background for `trailing-whitespace` made less
;;;;                     ghastly (`grey80` instead of `red1`)
;;;;    29-Jul-2009 (CT) `lse-face:line-too-long` too long
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-face)

;;;  4-Apr-2003
(defun lse-face:define (f &optional background foreground)
  "Define new face `f` and set `background` and `foreground` is specified."
  (make-face f)
  (and background (set-face-background f background))
  (and foreground (set-face-foreground f foreground))
; lse-face:define
)

(if (not lse-emacsX-p)
    t
  (lse-face:define 'lse-face:open-replacement            "Yellow" "Red")
  (lse-face:define 'lse-face:current-fill-in             "Yellow")
  (lse-face:define 'lse-face:current-fill-in-terminal    nil      "Orange")
  (lse-face:define 'lse-face:current-fill-in-replacement nil      "Magenta")
  (lse-face:define 'lse-face:current-fill-in-menu        nil      "Blue")
  (lse-face:define 'lse-face:current-fill-in-function    nil      "Brown")

  (lse-face:define 'lse-face:search-match    "Cyan")
  (lse-face:define 'lse-face:search-match-bg "Yellow")
  (lse-face:define 'lse-face:completion      "Yellow"       "Red")
  (lse-face:define 'lse-face:completion-m    "LightGray"    "Red")

  (lse-face:define 'lse-face:current-line-hl "Gray95")

  (lse-face:define 'lse-face:line-too-long   "Red"          "Yellow")

  (lse-face:define 'lse-face:cal:current    "LightPink"     nil)
  (lse-face:define 'lse-face:cal:day-line   "Gray88"        "Gray50")
  (lse-face:define 'lse-face:cal:desc       nil             "Royal Blue")
  (lse-face:define 'lse-face:cal:even       "Gray85"        nil)
  (lse-face:define 'lse-face:cal:fri        nil             "Gray40")
  (lse-face:define 'lse-face:cal:holiday    "Deep Sky Blue" "Gray95")
  (lse-face:define 'lse-face:cal:mon        nil             "Gray40")
  (lse-face:define 'lse-face:cal:month-name nil             "Royal Blue")
  (lse-face:define 'lse-face:cal:month-num  nil             "Orange")
  (lse-face:define 'lse-face:cal:odd        "Gray80"        nil)
  (lse-face:define 'lse-face:cal:sat        nil             "Gray60")
  (lse-face:define 'lse-face:cal:sun        nil             "Gray60")
  (lse-face:define 'lse-face:cal:text       nil             "Gray40")
  (lse-face:define 'lse-face:cal:this-month "Yellow"        nil)
  (lse-face:define 'lse-face:cal:this-week  "Orange"        "Royal Blue")
  (lse-face:define 'lse-face:cal:thu        nil             "Gray40")
  (lse-face:define 'lse-face:cal:time-field nil             "Orange")
  (lse-face:define 'lse-face:cal:today      "Yellow"        "Red")
  (lse-face:define 'lse-face:cal:tue        nil             "Gray40")
  (lse-face:define 'lse-face:cal:wed        nil             "Gray40")
  (lse-face:define 'lse-face:cal:week-end   "Gray60"        "Gray88")
  (lse-face:define 'lse-face:cal:week-field nil             "Orange")

  (set-face-background 'modeline            "Grey")         ; 28-Dec-1997
  (set-face-foreground 'modeline            "Light Yellow") ; 28-Dec-1997

  (set-face-background 'trailing-whitespace "gray80")       ;  3-Mar-2008
)
