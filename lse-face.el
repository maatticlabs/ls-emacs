;-*- coding: utf-8 -*-

;;;; Copyright (C) 1995-2017 Mag. Christian Tanzer. All rights reserved.
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
;;;;     3-Mar-2008 (CT) Background for `trailing-whitespace` made less ghastly
;;;;    29-Jul-2009 (CT) `lse-face:line-too-long` added
;;;;     5-Aug-2009 (CT) `lse-face:line-too-long` made less conspicuous
;;;;     8-Dec-2009 (CT) `lse-face:fl:` faces added
;;;;                     (`:frame`, `:buffer`, and `:current`)
;;;;    16-May-2013 (CT) Use 'mode-line, not 'modeline
;;;;                     (the latter gives error in emacs-24.3-r1)
;;;;    16-May-2013 (CT) Set background of 'region to "lightgoldenrod2"
;;;;    22-Nov-2013 (CT) Add `lse-face:figure-space`, `lse-face:narrow-nbsp`,
;;;;                     `lse-face:thin-space`
;;;;     5-Jul-2014 (CT) Set foreground of 'region to "Grey15"
;;;;                     (used to be dark, today suddenly was unreadable white)
;;;;     5-Jul-2014 (CT) Add `group` argument to `defface`
;;;;    22-Oct-2014 (CT) Add `lse-face:font:6x13` and `lse-face:font:7x13`
;;;;     7-Nov-2014 (CT) Change font definitions to `75-75-`; add more fonts
;;;;                     (`100-100-` doesn't work on all Gentoo machines)
;;;;    15-Sep-2015 (CT) Add `lse-face:font:hack-12`
;;;;     4-Jan-2016 (CT) Add `lse-face:ml:highlight`, `:mouse`, `:overwrite`
;;;;    21-Feb-2016 (CT) Add `'lse-face:regexp-highlight`
;;;;    23-Nov-2016 (CT) Change `font` of `mode-line-inactive`
;;;;                     * Fix atrocity committed by Emacs 25
;;;;    29-Aug-2017 (CT) Don't use XLFD font names for windows-nt
;;;;    10-Dec-2017 (CT) Add support for macOS (darwin)
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
  (defface lse-face:figure-space
    '((t (:background "dark magenta" :inherit escape-glyph :underline t))
     )
    "Face for `figure space` character"
    :group 'whitespace
  )
  (defface lse-face:narrow-nbsp
    '((t (:background "coral" :inherit escape-glyph :underline t))
     )
    "Face for `narrow non breaking space` character"
    :group 'whitespace
  )
  (defface lse-face:thin-space
    '((t (:background "magenta" :inherit escape-glyph :underline t))
     )
    "Face for `thin space` character"
    :group 'whitespace
  )
  (lse-face:define 'lse-face:open-replacement            "Yellow" "Red")
  (lse-face:define 'lse-face:current-fill-in             "Yellow")
  (lse-face:define 'lse-face:current-fill-in-terminal    nil      "Orange")
  (lse-face:define 'lse-face:current-fill-in-replacement nil      "Magenta")
  (lse-face:define 'lse-face:current-fill-in-menu        nil      "Blue")
  (lse-face:define 'lse-face:current-fill-in-function    nil      "Brown")

  (lse-face:define 'lse-face:regexp-highlight            "Gray88"       "blue")
  (lse-face:define 'lse-face:search-match                "Cyan")
  (lse-face:define 'lse-face:search-match-bg             "Yellow")
  (lse-face:define 'lse-face:completion                  "Yellow"       "Red")
  (lse-face:define 'lse-face:completion-m                "LightGray"    "Red")

  (lse-face:define 'lse-face:current-line-hl "Gray95")

  (lse-face:define 'lse-face:line-too-long   "Gray90"       nil)

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
  (lse-face:define 'lse-face:fl:frame       "Deep Sky Blue" "Gray95")
  (lse-face:define 'lse-face:fl:buffer      "Gray88"        "Gray30")
  (lse-face:define 'lse-face:fl:current     "Yellow"        "Red")
  (lse-face:define 'lse-face:ml:highlight   "Deep Sky Blue" "Light Yellow")
  (lse-face:define 'lse-face:ml:mouse       "Yellow"        "Red")
  (lse-face:define 'lse-face:ml:overwrite   "Red"           "Yellow")

  (set-face-background 'mode-line            "Grey")           ; 28-Dec-1997
  (set-face-foreground 'mode-line            "Light Yellow")   ; 28-Dec-1997

  (set-face-background 'region               "lightgoldenrod2"); 16-May-2013
  (set-face-foreground 'region               "Grey15")         ;  5-Jul-2014

  (set-face-background 'trailing-whitespace  "Gray80")         ;  3-Mar-2008
)

;;; 10-Dec-2017
;;; https://www.emacswiki.org/emacs/SetFonts
;;; Use `M-x describe-font` to find name of default font on macOS
(when (eq system-type 'darwin)
  (defconst lse-face:font:6x13
    "-*-Menlo-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1"
  )
  (defconst lse-face:font:7x13
    "-*-Menlo-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"
  )
  (defconst lse-face:font:hack-10
    "-*-Menlo-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1"
  )
  (defconst lse-face:font:hack-12
    "-*-Menlo-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"
  )
)

;;; 22-Oct-2014
;;; Used `xlsfonts` + experimentation to find the XLFD names of fixed fonts
;;; /usr/share/fonts/misc/fonts.alias is also interesting
;;;
;;; xlsfonts -fn "-misc-fixed-medium-r-*iso10646-1" | pcregrep -v -- '-(ko|ja)-'
;;;    -misc-fixed-medium-r-normal--0-0-100-100-c-0-iso10646-1
;;;    -misc-fixed-medium-r-normal--0-0-75-75-c-0-iso10646-1
;;;    -misc-fixed-medium-r-normal--10-100-75-75-c-60-iso10646-1
;;;    -misc-fixed-medium-r-normal--13-120-75-75-c-70-iso10646-1
;;;    -misc-fixed-medium-r-normal--13-120-75-75-c-80-iso10646-1
;;;    -misc-fixed-medium-r-normal--14-130-75-75-c-70-iso10646-1
;;;    -misc-fixed-medium-r-normal--15-140-75-75-c-90-iso10646-1
;;;    -misc-fixed-medium-r-normal--18-120-100-100-c-90-iso10646-1
;;;    -misc-fixed-medium-r-normal--20-200-75-75-c-100-iso10646-1
;;;    -misc-fixed-medium-r-normal--6-60-75-75-c-40-iso10646-1
;;;    -misc-fixed-medium-r-normal--7-70-75-75-c-50-iso10646-1
;;;    -misc-fixed-medium-r-normal--8-80-75-75-c-50-iso10646-1
;;;    -misc-fixed-medium-r-normal--9-90-75-75-c-60-iso10646-1
;;;    -misc-fixed-medium-r-semicondensed--0-0-75-75-c-0-iso10646-1
;;;    -misc-fixed-medium-r-semicondensed--12-110-75-75-c-60-iso10646-1
;;;    -misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso10646-1
(when (eq window-system 'x)
  (defconst lse-face:font:6x10
    "-misc-fixed-medium-r-normal--10-100-75-75-c-60-iso10646-1"
    "Standard X-Window fixed width font with old style designation 6x10"
  )

  (defconst lse-face:font:6x12
    "-misc-fixed-medium-r-semicondensed--12-110-75-75-c-60-iso10646-1"
    "Standard X-Window fixed width font with old style designation 6x12"
  )

  (defconst lse-face:font:6x13
    "-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso10646-1"
    "Standard X-Window fixed width font with old style designation 6x13"
  )

  (defconst lse-face:font:7x13
    "-misc-fixed-medium-r-normal--13-120-75-75-c-70-iso10646-1"
    "Standard X-Window fixed width font with old style designation 7x13"
  )

  (defconst lse-face:font:7x14
    "-misc-fixed-medium-r-normal--14-130-75-75-c-70-iso10646-1"
    "Standard X-Window fixed width font with old style designation 7x14"
  )

  (defconst lse-face:font:8x13
    "-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso10646-1"
    "Standard X-Window fixed width font with old style designation 8x13"
  )

  (defconst lse-face:font:8x16
    "-misc-fixed-medium-r-normal--16-120-75-75-c-80-iso10646-1"
    "Standard X-Window fixed width font with old style designation 8x16"
  )

  (defconst lse-face:font:9x15
    "-misc-fixed-medium-r-normal--15-140-75-75-c-90-iso10646-1"
    "Standard X-Window fixed width font with old style designation 9x15"
  )

  (defconst lse-face:font:10x20
    "-misc-fixed-medium-r-normal--20-200-75-75-c-100-iso10646-1"
    "Standard X-Window fixed width font with old style designation 10x20"
  )

  ;;; 15-Sep-2015
  (defconst lse-face:font:hack-10
    "-*-Hack-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1"
    "Hack font http://sourcefoundry.org/hack/,  https://github.com/chrissimpkins/Hack"
  )

  (defconst lse-face:font:hack-12
    "-*-Hack-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"
    "Hack font http://sourcefoundry.org/hack/,  https://github.com/chrissimpkins/Hack"
  )
); window-system == x

(defconst lse-face:font:small   lse-face:font:6x13)
(defconst lse-face:font:default lse-face:font:7x13)

;;; 23-Nov-2016
;;; if the `font` of `mode-line-inactive` is larger than the font used by
;;; the buffer, the mode line is totally broken in Emacs 25.1
(when (eq system-type 'x)
  (when (and lse-emacsX-p (symbolp 'mode-line-inactive))
    (custom-set-faces
      '(mode-line-inactive
        ((t
          (:foreground "Grey30" :background "Gray90"
           :font       "-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso10646-1"
          )
         )
         (((class color) (min-colors 88) (background light))
          (:foreground "Grey30" :background "Gray90"
           :font       "-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso10646-1"
          )
         )
         (((class color) (min-colors 88) (background dark))
          (:foreground "Grey90" :background "Gray50"
           :font       "-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso10646-1"
          )
         )
        )
        t
      )
    )
  )
)
;;; __END__ lse-face.el
