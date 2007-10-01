;-*- unibyte: t; coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work

;;;;unix_ms_filename_correspondency lse-frame:el lse_fram:el
;;;; Copyright (C) 1996-2007 Mag. Christian Tanzer. All rights reserved
;;;; Glasauergasse 32, A--1130 Wien, Austria. tanzer@swing.co.at

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
;;;;    lse-frame
;;;;
;;;; Purpose
;;;;    Provide frame related functions
;;;;
;;;; Revision Dates
;;;;    12-Aug-1996 (CT) Creation
;;;;    27-Aug-1996 (CT) Use `lse-user-initials-r' instead of
;;;;                     `lse-user-initials'
;;;;    27-Sep-1996 (CT) Use 'title for emacs versions newer than 19.30
;;;;     2-Oct-1996 (CT) Use numeric comparison for version
;;;;     2-Oct-1996 (CT) Don't use 'name for versions newer than 19.30
;;;;     3-Oct-1996 (CT) frame-title-format redefined
;;;;     3-Oct-1996 (CT) lse-frame:make added
;;;;    11-Oct-1996 (CT) icon-title-format  redefined
;;;;     5-Mar-1997 (CT) lse-frame:set-width and lse-frame:set-height added
;;;;     9-Apr-1998 (CT) lse-frame:make-small added
;;;;     1-May-1998 (CT) lse-frame:wbc-ht changed to 22
;;;;                     `max' used to restrict parameter of
;;;;                           set-frame-position for y-position
;;;;     3-Jan-2000 (CT) `lse-frame:make' returns the newly made frame
;;;;     7-Sep-2002 (CT) `lse-frame:set-width:<nnn>` added
;;;;     8-Sep-2002 (CT) `lse-frame:set-height:<nnn>` added
;;;;    28-Mar-2007 (CT) `lse-frame:large-height` changed from 72 to 66
;;;;    28-Mar-2007 (CT) `lse-frame:*-menu-bar` added
;;;;    ««revision-date»»···
;;;;--
;;;;
(provide 'lse-frame)

;;;  8-Sep-2002
(defvar lse-frame:std-width 80
  "Standard width of frames (in characters) created by LS-Emacs"
)

;;;  8-Sep-2002
(defvar lse-frame:wide-width 132
  "Width of wide frames (in characters) created by LS-Emacs"
)

;;;  8-Sep-2002
(defvar lse-frame:double-width 162
  "Width of double wide frames (in characters) created by LS-Emacs"
)

;;;  8-Sep-2002
(defvar lse-frame:std-height 48
  "Standard height of frames (in lines) created by LS-Emacs"
)

;;;  8-Sep-2002
(defvar lse-frame:small-height 30
  "Height of small frames (in lines) created by LS-Emacs"
)

;;;  8-Sep-2002
(defvar lse-frame:large-height 66 ;; 28-Mar-2007 s/72/66/
  "Height of large frames (in lines) created by LS-Emacs"
)

;;; 12-Aug-1996
(defun lse-set-frame-title (nam &optional fram)
  (if lse-emacsX-p
    (modify-frame-parameters
        (or fram (selected-frame))
        (if (< emacs-minor-version 31)
            (list (cons 'name nam))
          (list (cons 'title nam))
        )
    )
  )
; lse-set-frame-title
)

;;; 12-Aug-1996
(defun lse-set-hosted-frame-title (&optional nam fram)
  (interactive)
  (or nam (setq nam "LSE"))
  (lse-set-frame-title
      (concat nam " " (lse-system-name) "::" (lse-user-initials-r))
      fram
  )
; lse-set-hosted-frame-title
)

;;; 12-Aug-1996
(defun lse-set-shorthosted-frame-title (&optional nam fram)
  (interactive)
  (or nam (setq nam "LSE"))
  (lse-set-frame-title
      (concat nam
              " "
              (substring (lse-system-name) 0 1)
              (substring (lse-system-name) -1 )
              "::"
              (lse-user-initials-r)
      )
      fram
  )
; lse-set-shorthosted-frame-title
)

;;;  3-Oct-1996
(defun lse-frame:n ()
  (length (frame-list))
; lse-frame:n
)

;;;  3-Oct-1996
(defun lse-frame:make (&optional nam pos siz alist)
  "Make a frame named `nam' at position `pos' with size `siz'. `alist is passed to make-frame'"
  (let ((fram (make-frame alist)))
    (and (consp   pos) (set-frame-position fram (car pos) (cdr pos)))
    (and (consp   siz) (set-frame-size     fram (car siz) (cdr siz)))
    (and (stringp nam) (lse-set-shorthosted-frame-title nam fram))
    fram
  )
; lse-frame:make
)

;;;  9-Apr-1998
(defun lse-frame:make-small (&optional ht nam)
  "Make a small frame with height `ht' (default: 30)"
  (interactive "p")
  (if (eq ht 1) (setq ht lse-frame:small-height))
  (lse-frame:make nil nil (cons lse-frame:std-width ht))
; lse-frame:make-small
)

(setq frame-title-format (list "LSE %b"))
(setq icon-title-format  (list "LSE %b")); 11-Oct-1996

;;;  5-Mar-1997
(defun lse-frame:parameter (nsym &optional fram)
  (or fram (setq fram (selected-frame)))
  (cdr (assoc nsym (frame-parameters fram)))
; lse-frame:parameter
)

;;;  5-Mar-1997
(defvar lse-frame:wbc-wd -5)    ; window border correction for width
(defvar lse-frame:wbc-ht +22)   ; window border correction for height
                                ;    1-May-1998 `22' ; instead of `12'

(defvar lse-frame:save-pos-table-w nil)
(defvar lse-frame:save-wd-table nil)
;;; (setq   lse-frame:save-pos-table-w nil)

;;;  5-Mar-1997
(defun lse-frame:set-width (wd &optional fram)
  "Set width of frame to `wd'"
  (interactive "NFrame-width: ")
  (or fram (setq fram (selected-frame)))
  (let ((old-wd (frame-width fram))
        (saved-wd (if (assoc fram lse-frame:save-wd-table)
                      (cdr (assoc fram lse-frame:save-wd-table))
                    0
                  )
        )
       )
    (or (> wd 0)  (setq wd saved-wd))
    (if (eq wd old-wd)
        t
      (set-frame-size fram wd (frame-height fram))
      (or (assoc fram lse-frame:save-wd-table)
          (setq lse-frame:save-wd-table
                (cons (cons fram old-wd) lse-frame:save-wd-table)
          )
      )
      (let ((delta (- (x-display-pixel-width) (frame-pixel-width fram)))
           )
        (if (> wd old-wd)
            (if (> (lse-frame:parameter 'left fram) delta)
                (progn
                  (or (assoc fram lse-frame:save-pos-table-w)
                      (setq lse-frame:save-pos-table-w
                            (cons (cons fram (lse-frame:parameter 'left fram))
                                  lse-frame:save-pos-table-w
                            )
                      )
                  )
                  (set-frame-position fram
                      (- delta
                         (or (lse-frame:parameter 'scroll-bar-width fram) 0)
                      )
                      (max (- (lse-frame:parameter 'top fram) lse-frame:wbc-ht)
                           lse-frame:wbc-ht
                      )
                  )
                )
            )
          (let ((saved (assoc fram lse-frame:save-pos-table-w))
               )
            (if saved
                (progn
                  (set-frame-position fram (cdr saved)
                      (max (- (lse-frame:parameter 'top fram) lse-frame:wbc-ht)
                           lse-frame:wbc-ht
                      )
                  )
                  (delete saved lse-frame:save-pos-table-w)
                )
            )
          )
        )
      )
    )
  )
; lse-frame:set-width
)

;;;  7-Sep-2002
(defun lse-frame:set-width:80 (wd &optional fram)
  "Set width of frame `fram` to prefix argument or 80 (standard width)"
  (interactive "P")
  (lse-frame:set-width (or wd lse-frame:std-width) fram)
; lse-frame:set-width:80
)

;;;  7-Sep-2002
(defun lse-frame:set-width:132 (wd &optional fram)
  "Set width of frame `fram` to prefix argument or 132 (wide width)"
  (interactive "P")
  (lse-frame:set-width (or wd lse-frame:wide-width) fram)
; lse-frame:set-width:132
)

;;;  7-Sep-2002
(defun lse-frame:set-width:162 (wd &optional fram)
  "Set width of frame `fram` to prefix argument or 162 (double width)"
  (interactive "P")
  (lse-frame:set-width (or wd lse-frame:double-width) fram)
; lse-frame:set-width:162
)

;;;  5-Mar-1997
(defvar lse-frame:save-pos-table-h nil)
(defvar lse-frame:save-ht-table    nil)

;;;  5-Mar-1997
(defun lse-frame:set-height (ht &optional fram)
  "Set height of frame to `ht'"
  (interactive "NFrame-height: ")
  (or fram (setq fram (selected-frame)))
;;;  8-Sep-2002 (or (< ht lse-frame:large-height) (setq ht lse-frame:large-height))
  (let ((old-ht   (frame-height fram))
        (saved-ht (if (assoc fram lse-frame:save-ht-table)
                      (cdr (assoc fram lse-frame:save-ht-table))
                    0
                  )
        )
       )
    (or (> ht 0) (setq ht saved-ht))
    (if (eq ht old-ht)
        t
      (set-frame-size fram (frame-width fram) ht)
      (or (assoc fram lse-frame:save-ht-table)
          (setq lse-frame:save-ht-table
                (cons (cons fram old-ht) lse-frame:save-ht-table)
          )
      )
      (let ((delta (- (x-display-pixel-height) (frame-pixel-height fram)))
           )
        (if (> ht old-ht)
            (if (> (lse-frame:parameter 'top fram) delta)
                (progn
                  (or (assoc fram lse-frame:save-pos-table-h)
                      (setq lse-frame:save-pos-table-h
                            (cons (cons fram (lse-frame:parameter 'top fram))
                                  lse-frame:save-pos-table-h
                            )
                      )
                  )
                  (set-frame-position fram
                      (lse-frame:parameter 'left fram)
                      (- delta lse-frame:wbc-wd)
                  )
                )
            )
          (let ((saved (assoc fram lse-frame:save-pos-table-h))
               )
            (if saved
                (progn
                  (set-frame-position fram
                                      (lse-frame:parameter 'left fram)
                                      (cdr saved)
                  )
                  (delete saved lse-frame:save-pos-table-h)
                )
            )
          )
        )
      )
    )
  )
; lse-frame:set-height
)

;;;  8-Sep-2002
(defun lse-frame:set-height:48 (ht &optional fram)
  "Set height of frame `fram` to prefix argument or 48 (standard height)"
  (interactive "P")
  (lse-frame:set-height (or ht lse-frame:std-height) fram)
; lse-frame:set-height:48
)

;;;  8-Sep-2002
(defun lse-frame:set-height:30 (ht &optional fram)
  "Set height of frame `fram` to prefix argument or 30 (small height)"
  (interactive "P")
  (lse-frame:set-height (or ht lse-frame:small-height) fram)
; lse-frame:set-height:30
)

;;;  8-Sep-2002
(defun lse-frame:set-height:72 (ht &optional fram)
  "Set height of frame `fram` to prefix argument or 72 (large height)"
  (interactive "P")
  (lse-frame:set-height (or ht lse-frame:large-height) fram)
; lse-frame:set-height:72
)

;;; 28-Mar-2007
(defun lse-frame@change_menu_bar (status &optional fram)
  (or fram (setq fram (selected-frame)))
  (modify-frame-parameters fram (list (cons 'menu-bar-lines status)))
; lse-frame@change_menu_bar
)

;;; 28-Mar-2007
(defun lse-frame:disable-menu-bar (&optional fram)
  "Disable menu bar for frame"
  (interactive)
  (lse-frame@change_menu_bar 0)
; lse-frame:disable-menu-bar
)

;;; 28-Mar-2007
(defun lse-frame:enable-menu-bar (&optional fram)
  "Disable menu bar for frame"
  (interactive)
  (lse-frame@change_menu_bar 1)
; lse-frame:enable-menu-bar
)

;;; 28-Mar-2007
(defun lse-frame:toggle-menu-bar (&optional fram)
  "Toggle menu bar for frame"
  (interactive)
  (lse-frame@change_menu_bar
    (- 1 (frame-parameter fram 'menu-bar-lines))
  )
; lse-frame:toggle-menu-bar
)

;;; __END__ lse-frame.el
