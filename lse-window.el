;-*- coding: utf-8 -*-

;;;; Copyright (C) 1994-2016 Mag. Christian Tanzer. All rights reserved.
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
;;;;    lse-window
;;;;
;;;; Purpose
;;;;    Provide basic window and screen management functions
;;;;
;;;; Revision Dates
;;;;    18-Jun-1994 (CT) Creation (of comment)
;;;;    18-Jun-1994 (CT) Merged lse-screen with stuff from swing-window
;;;;                     and swing-buffer
;;;;    27-Jun-1994 (CT) lse-goto-home-mark-window implemented properly
;;;;     3-Nov-1994 (CT) Write message in lse-set-home-mark-window
;;;;     6-Jan-1995 (CT) Let lse-show-windows show the home buffer
;;;;                     Set home buffer inside lse-split-window
;;;;    23-Feb-1995 (CT) Error corrected in lse-window:save-temp-hidden
;;;;    19-Mar-1995 (CT) Optional argument for lse-window:restore-temp-hidden
;;;;                     added
;;;;     3-Oct-1996 (CT) lse-window:display-buffer added and installed as
;;;;                     display-buffer-function
;;;;    11-Oct-1996 (CT) `lse-next-window-all-frames' and
;;;;                     `lse-previous-window-all-frames' added
;;;;    26-Feb-1997 (CT) Patched `lse-scroll-window-forw' and
;;;;                     `lse-scroll-window-back' to avoid change of position
;;;;                     in buffer (Emacs behavior changed somewhere between
;;;;                     versions 19.28 and 19.34)
;;;;    22-May-1997 (CT) Use `balance-windows' if available (works also for
;;;;                     horizontal splitting)
;;;;    18-Dec-1997 (CT) `lse-(un)?set-selective-display' added
;;;;    29-Dec-1997 (CT) `lse-next-screen-2' and `lse-previous-screen-2' added
;;;;    28-Dec-1999 (CT) `lse-window:display-buffer' changed to use
;;;;                     `special-display-popup-frame' for -buffers even if
;;;;                     if not `ntw'
;;;;    28-Dec-1999 (CT) `lse-scroll-to-top' added
;;;;    28-Dec-1999 (CT) Regular expression `" *\\*.+\\*"' corrected (double
;;;;                     backslashes instead of single ones)
;;;;     3-Jan-2000 (CT) Use `special-display-regexps' instead of setting
;;;;                     `display-buffer-function' to
;;;;                     `lse-window:display-buffer'
;;;;     1-Sep-2002 (CT) `lse-scroll-to-top` changed to allow prefix argument
;;;;    21-Nov-2002 (CT) Python buffers added to `special-display-regexps`
;;;;     7-Apr-2003 (CT) `size` added to `lse-split-window`
;;;;    14-Oct-2007 (CT) `show-eob` added to `lse-scroll-to-top`
;;;;    15-Oct-2007 (CT) `lse-scroll-to-top` changed to scroll end-of-buffer
;;;;                     to bottom of screen unconditionally if visible
;;;;     8-Dec-2007 (CT) `lse-scroll-to-top` changed to scroll end-of-buffer
;;;;                     just past bottom of screen if it was visible
;;;;    19-Dec-2007 (CT) `lse-scroll-to-top` changed to use
;;;;                     `lse-tpu:backward-char` instead of
;;;;                     `lse-tpu:previous-line` (which breaks
;;;;                     `lse-cal:plan:sync-to-view` [due to a bug in Emacs?!])
;;;;     3-Apr-2008 (CT) Added `lse-scroll-to-bottom`
;;;;    10-Nov-2010 (CT) Use `mapc` instead of `mapcar` where appropriate
;;;;     6-Nov-2014 (CT) Remove legacy `lse-window:display-buffer`
;;;;     6-Nov-2014 (CT) Add `lse-scroll-to-{bottom,top}` to `lse-split-window`
;;;;     6-Nov-2014 (CT) Remove compiler warnings
;;;;     6-Nov-2014 (CT) Add advice `lse-scroll-to-top:after` to
;;;;                     `imenu-default-goto-function`
;;;;     7-Nov-2014 (CT) Add `unsplittable` to lse-window:special-display-alist
;;;;                     Set `special-display-frame-alist` for Emacs 24.3+, too
;;;;    12-Nov-2014 (CT) Remove support for ancient Emacs versions
;;;;    16-Nov-2014 (CT) Add to `imenu-after-jump-hook`,
;;;;                     instead of advicing `imenu-default-goto-function`
;;;;    20-Nov-2014 (CT) Add `^` to `interactive` spec of movement commands
;;;;    ????revision-date??????????
;;;;--
(provide 'lse-window)

(defun lse-iterate-windows (apply)
  (walk-windows apply 'no-minibuf)
; lse-iterate-windows
)

;;;+
;;; Functions to restore a buffer-window association which is temporarily
;;; hidden by with-output-to-temp-buffer
;;;-
(defvar lse-window:temp-hidden nil)
(defvar lse-window:wb-assoc    nil)

(defun lse-window:one-wb (w)
  (lse-add-to-list lse-window:wb-assoc (cons w (window-buffer w)))
; lse-window:one-wb
)

(defun lse-window:make-wb-list ()
  (setq lse-window:wb-assoc nil)
  (lse-iterate-windows 'lse-window:one-wb)
; lse-window:make-wb-list
)

(defun lse-window:temp-buffer-show-function (buf)
  (let (w wb-assoc)
    (lse-window:make-wb-list)
    (setq w (display-buffer buf t))
    (setq wb-assoc (assoc w lse-window:wb-assoc))
    (if (equal (get-buffer buf) (cdr wb-assoc))
        t; don't stack the temp-buffer!
      (lse-add-to-list lse-window:temp-hidden wb-assoc)
    )
  )
; lse-window:temp-buffer-show-function
)

(setq temp-buffer-show-function 'lse-window:temp-buffer-show-function)

(defun lse-window:save-temp-hidden ()
  (mapc
    (function
      (lambda (wb-assoc)
        (lse-add-to-list lse-window:temp-hidden wb-assoc)
      )
    )
    lse-window:wb-assoc
  )
; lse-window:save-temp-hidden
)

(defun lse-window@restore-temp-hidden ()
  (condition-case nil
      (let ((wb-assoc (car lse-window:temp-hidden)))
        (set-window-buffer (car wb-assoc) (cdr wb-assoc))
        (lse-remove-car-from-list lse-window:temp-hidden)
      )
    (error
      (setq lse-window:temp-hidden nil)
    )
  )
; lse-window@restore-temp-hidden
)

(defun lse-window:restore-temp-hidden (&optional arg)
  "Restore a buffer which was temporarily hidden by with-output-to-temp-buffer to its window"
  (interactive "P")
  (if arg
      (while lse-window:temp-hidden (lse-window@restore-temp-hidden))
    (if lse-window:temp-hidden
        (lse-window@restore-temp-hidden)
      (lse-message "Sorry, no temporarily hidden buffer to restore")
    )
  )
; lse-window:restore-temp-hidden
)

(defvar lse-max-scroll-amount 12)

(defun lse-scroll-vertically (num &optional times)
  "Scroll `num' lines and repeat that `times'"
  (let ((lines (if (integerp times) (* num times) num)))
    (lse-tpu:next-line-internal lines)
  )
) ; lse-scroll-vertically

(defmacro lse-scroll-unit ()
  '(min lse-max-scroll-amount (/ (window-height) 5))
)

(defun lse-next-screen (&optional arg)
  "Scroll forward"
  (interactive "^p")
  (lse-scroll-vertically (lse-scroll-unit) arg)
) ; lse-next-screen

;;; 29-Dec-1997
(defsubst lse-next-screen-2 () (interactive "^") (lse-next-screen 2))

(defun lse-previous-screen (&optional arg)
  "Scroll backward"
  (interactive "^p")
  (lse-scroll-vertically (- (lse-scroll-unit)) arg)
) ; lse-previous-screen

;;; 29-Dec-1997
(defsubst lse-previous-screen-2 () (interactive "^") (lse-previous-screen 2))

(defvar lse-window::number 0)
(defvar lse-window::length 0)

;;; 11-Oct-1996
(defun lse-next-window-all-frames ()
  "Selects next window in cyclic order, considering all windows in all visible frames."
  (interactive)
  (let* ((nw (next-window  nil nil 'visible))
         (fr (window-frame nw))
        )
    (if (eq (selected-frame) fr)
        t
      (redirect-frame-focus (selected-frame) fr)
      (select-frame         fr)
      (raise-frame          fr)
    )
    (select-window nw)
  )
; lse-next-window-all-frames
)

;;; 11-Oct-1996
(defun lse-previous-window-all-frames ()
  "Selects previous window in cyclic order, considering all windows in all visible frames."
  (interactive)
  (let* ((nw (previous-window  nil nil 'visible))
         (fr (window-frame nw))
        )
    (if (eq (selected-frame) fr)
        t
      (redirect-frame-focus (selected-frame) fr)
      (select-frame fr)
      (raise-frame  fr)
    )
    (select-window nw)
  )
; lse-previous-window-all-frames
)

(defun lse-next-window ()
  (interactive)
  (other-window +1)
; lse-next-window
)

(defun lse-previous-window ()
  (interactive)
  (other-window -1)
; lse-previous-window
)

(defun lse-scroll-window-forw ()
  (interactive)
  (save-mark-and-excursion
    (scroll-down (/ (window-height) 3))
  )
  (if (pos-visible-in-window-p (point)); 26-Feb-1997
      t
    (recenter -2)
  )
; lse-scroll-other-window-forw
)

(defun lse-scroll-window-back ()
  (interactive)
  (save-mark-and-excursion
    (scroll-up   (/ (window-height) 3))
  )
  (if (pos-visible-in-window-p (point)); 26-Feb-1997
      t
    (recenter 2)
  )
; lse-scroll-other-window-back
)

;;; 28-Dec-1999
(defun lse-scroll-to-top (&optional arg)
  "Scroll current line to top of window."
  (interactive "P")
  (let ((pos-1 (point))
        pos-2
       )
    (recenter (or arg 0))
    (if (pos-visible-in-window-p (point-max))
        (save-mark-and-excursion
          (lse-tpu:move-to-end)
          (lse-tpu:backward-char 1)
          (setq pos-2 (point))
          (recenter -1)
        )
    )
    (if pos-2 (goto-char (min pos-1 pos-2)))
  )
; lse-scroll-to-top
)

;;;  6-Nov-2014
(defun lse-scroll-to-top:after (&rest arg)
  (lse-scroll-to-top)
; lse-scroll-to-top:after
)

;;;  3-Apr-2008
(defun lse-scroll-to-bottom (&optional arg)
  "Scroll current line to bottom of window."
  (interactive "P")
  (recenter (or arg -1))
; lse-scroll-to-bottom
)

(defun lse-scroll-other-window-forw ()
  (interactive)
  (scroll-other-window (/ (window-height) 3))
; lse-scroll-other-window-forw
)

(defun lse-scroll-other-window-back ()
  (interactive)
  (scroll-other-window (- (/ (window-height) 3)))
; lse-scroll-other-window-back
)

(defun lse-split-window (&optional wdw horizontally size)
  (interactive)
  (unless horizontally
    (lse-scroll-to-bottom)
  )
  (lse-window:mark-stack:initialize (split-window wdw size horizontally))
  (if (not size) (lse-balance-windows));  7-Apr-2003
  (lse-set-home-mark-window (point-marker));  6-Jan-1995
  (lse-next-window)
  (unless horizontally
    (lse-scroll-to-top)
  )
; lse-split-window
)

(defun lse-split-window-horizontally (&optional wdw size)
  (interactive)
  (lse-split-window wdw t size)
; lse-split-window-horizontally
)

(defun lse-delete-window (&optional wdw)
  (interactive)
  (lse-window:mark-stack:remove (or wdw (selected-window)))
  (delete-window wdw)
  (lse-balance-windows)
; lse-delete-window
)

(defun lse-delete-other-windows (&optional wdw)
  (interactive)
  (let ((w (or wdw (selected-window))))
    (lse-window::list:delete)
    (lse-window:mark-stack:initialize w)
    (delete-other-windows w)
  )
; lse-delete-other-windows
)

;;; 18-Dec-1997
(defun lse-set-selective-display (&optional num)
  "Hide all lines indented more than the current position of point in line."
  (interactive "*p")
  (set-selective-display (if (and num (> num 1)) num (1+ (current-column))))
; lse-set-selective-display
)

;;; 18-Dec-1997
(defun lse-unset-selective-display ()
  "Show all lines no matter what the indentation."
  (interactive)
  (set-selective-display nil)
; lse-unset-selective-display
)

(defun lse-window::find-number (w)
  (setq lse-window::number (1+ lse-window::number))
; lse-window::find-number
)

(defun lse-window::find-length (w)
  (setq lse-window::length (+ lse-window::length (window-height w)))
; lse-window::find-length
)

(defun lse-window::balance (w)
  (select-window w)
  (cond ((> lse-window::length (window-height w))
         (enlarge-window (- lse-window::length (window-height w)))
        )
        ((< lse-window::length (window-height w))
         (shrink-window (- (window-height w) lse-window::length))
        )
  )
; lse-window::balance
)

(if (fboundp 'balance-windows) ;; 22-May-1997
    (fset 'lse-balance-windows 'balance-windows)
  (defun lse-balance-windows ()
    (interactive)
    (setq lse-window::number 0)
        (lse-iterate-windows 'lse-window::find-number)
    (setq lse-window::length 0)
        (lse-iterate-windows 'lse-window::find-length)
    (setq lse-window::length (/ lse-window::length lse-window::number))
    (let ((w (selected-window)))
        (lse-iterate-windows 'lse-window::balance)
        (select-window w)
    )
  ; lse-balance-windows
  )
)

(defun lse-show-windows ()
  (interactive)
  (with-output-to-temp-buffer " *Window List*"
    (princ (format "%-40s"  "  Buffer"))
    (princ (format "%6s "   "height"))
    (princ (format "%6s   " "top"))
    (princ "home buffer")
    (princ "\n")
    (princ (make-string 79 ?=))
    (princ "\n")
    (lse-iterate-windows 'lse-show-window)
  )
; lse-show-windows
)

(defun lse-show-window (w)
  (princ (if (eq w (selected-window)) "* " "  "))
  (princ (format "%-38s"  (buffer-name (window-buffer w))))
  (princ (format "%6d "   (window-height w)))
  (princ (format "%6d   " (nth 1 (window-edges w))))
  (save-window-excursion
    (select-window w)
    (let ((hmark (lse-home-mark (lse-window-mark@stack)))
         )
      (if (markerp hmark)
          (save-mark-and-excursion
            (lse-goto-position hmark)
            (princ (format "%s (%d, %d)"
                           (buffer-name (marker-buffer hmark))
                           (lse-line-number) (1+ (current-column))
                   )
            )
          )
        (princ "no home buffer")
      )
    )
  )
  (princ "\n")
; lse-show-window
)

;;;;++
;;; Window-mark-stack
;;;;--
(defvar lse-window::list nil
  ;; this is an association list, which pairs all windows with their
  ;; corresponding mark-stacks
)

(defun lse-window::list:delete ()
  (mapc
    (function (lambda (ms) (lse-delete-mark-stack (cdr ms))))
    lse-window::list
  )
  (setq lse-window::list nil)
; lse-window::list:delete
)

(defun lse-window-mark@stack (&optional no-create)
  (let ((entry (assq (selected-window) lse-window::list)))
    (if entry
        (cdr entry)
      (if no-create
          lse-mark-stack:global
        (lse-window:mark-stack:initialize)
        (lse-window-mark@stack t)
      )
    )
  )
; lse-window-mark@stack
)

(defun lse-window:mark-stack:initialize (&optional wdw)
  (let ((w (or wdw (selected-window))))
    (or (assq w lse-window::list)
        (save-window-excursion
          (if (not (eq w (selected-window)))
              (select-window w)
          )
          (lse-add-to-list lse-window::list (cons w (lse-new-mark-stack)))
        )
    )
  )
; lse-window:mark-stack:initialize
)

(defun lse-window:mark-stack:remove (&optional wdw)
  (let* ((w     (or wdw (selected-window)))
         (entry (assq w lse-window::list))
        )
    (if entry
        (progn
          (lse-remove-from-list lse-window::list entry)
          (lse-delete-mark-stack (cdr entry))
        )
    )
  )
; lse-window:mark-stack:remove
)

(defun lse-goto-last-mark-window ()
  "Goto window last mark."
  (interactive)
  (lse-goto-last-mark (lse-window-mark@stack))
; lse-goto-last-mark-window
)

(defun lse-goto-home-mark-window ()
  "Goto window home mark."
  (interactive)
  (lse-goto-home-mark (lse-window-mark@stack))
; lse-goto-home-mark-window
)

(defun lse-set-last-mark-window (&optional to-mark)
  "Set window last mark to 'to-mark'."
  (interactive "d")
  (lse-set-last-mark (lse-window-mark@stack)
                       (if (integerp to-mark) (copy-marker to-mark) to-mark)
  )
; lse-set-last-mark-window
)

(defun lse-set-home-mark-window (&optional to-mark)
  "Set window last mark to 'to-mark'."
  (interactive "d")
  (lse-set-home-mark (lse-window-mark@stack)
                       (if (integerp to-mark) (copy-marker to-mark) to-mark)
  )
  (message (format "Home mark of current window set to buffer %s (%d, %d)"
                   (buffer-name (current-buffer))
                   (lse-line-number) (1+ (current-column))
           )
  );  3-Nov-1994
; lse-set-home-mark-window
)

(defun lse-push-mark-window ()
  "Push mark onto window mark stack."
  (interactive)
  (lse-push-mark (lse-window-mark@stack))
; lse-push-mark-window
)

(defun lse-goto-mark-and-pop-window ()
  "Goto top mark of window mark stack and remove it."
  (interactive)
  (lse-goto-mark-and-pop (lse-window-mark@stack))
; lse-goto-mark-and-pop-window
)

(defun lse-toggle-mark-window ()
  "Goto top mark of window mark-stack and replace it by previous position."
  (interactive)
  (lse-toggle-mark (lse-window-mark@stack))
; lse-toggle-mark-window
)

;;;;++
;;; Window configuration list
;;;;--
(defvar lse-window::configuration-list nil
  ;; window configurations stored by the user (stack-wise)
)

(defun lse-push-window-configuration ()
  (interactive)
  (lse-add-to-list lse-window::configuration-list
                     (current-window-configuration)
  )
  (message "Window configuration %d pushed"
           (length lse-window::configuration-list)
  )
; lse-push-window-configuration
)

(defun lse-pop+restore-window-configuration ()
  (interactive)
  (if lse-window::configuration-list
      (progn
        (set-window-configuration (car lse-window::configuration-list))
        (lse-remove-car-from-list lse-window::configuration-list)
      )
  )
  (message "%d window configurations remaining"
           (length lse-window::configuration-list)
  )
; lse-pop+restore-window-configuration
)

;;;  6-Nov-2014
(defvar lse-window:special-display-alist
  '((width . 80) (height . 30) (unsplittable . nil))
)

;;;  6-Nov-2014
(defvar lse-window:special-display-regexp
  (concat
    " *\\*\\("
      "Finder\\|Help\\|mail\\|shell\\|info\\|.*-Log\\|VC\\|vc"
      "\\|Text Properties\\|Faces\\|Colors\\|Python\\( Output\\)?"
      "\\|Apropos\\|Shadows"
    "\\).*\\*"
  )
)

(if lse-emacs24.3-p
    (setq display-buffer-alist
      (list
        (cons
          lse-window:special-display-regexp
            (cons
              'special-display-popup-frame lse-window:special-display-alist
            )
        )
      )
    )
  (with-no-warnings
    ;; obsolete variable as of Emacs 24.3, disable compiler warnings
    (setq special-display-regexps (list lse-window:special-display-regexp))
  )
)

(with-no-warnings
  ;; obsolete variable as of Emacs 24.3, disable compiler warnings
  (setq special-display-frame-alist lse-window:special-display-alist)
)

(setq same-window-buffer-names nil);  3-Jan-2000
;;; (setq same-window-regexps (list "\\*[Cc]ompletions?\\*"));  6-Jan-2000;  3-Jan-2000

(require 'imenu)
(when (boundp 'imenu-after-jump-hook)
  (add-hook 'imenu-after-jump-hook #'lse-scroll-to-top:after)
)

;;; __END__ lse-window.el
