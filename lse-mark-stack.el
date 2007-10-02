;-*- unibyte: t; coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work
 
;;;;unix_ms_filename_correspondency lse-mark-stack:el lse_mark:el
;;;; Copyright (C) 1994 Mag. Christian Tanzer. All rights reserved.
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
;;;;    Swing-Mark-Stack
;;;; 
;;;; Purpose
;;;;    A mark-stack comprises two lists:
;;;;         - the first list contains the last-mark (last position saved 
;;;;           implicitely) and the home-mark
;;;;         - the second list contains the marks stacked on explicit request
;;;;           by the user (the element at position zero points to the
;;;;           current mark of the stack)
;;;; 
;;;;    It's possible to use several independent mark stacks. The internal
;;;;    core functions (identified by '@'s as name separators) work on the
;;;;    mark-stack bound to 'lse@mark@stack'. Do not use these directly!
;;;; 
;;;;    The interface functions take the mark-stack as parameter. Use the
;;;;    commands implementing the global mark-stack as documentation how to
;;;;    use them.
;;;;    
;;;; Revision Dates
;;;;    12-Dec-1993 (CT) Creation
;;;;     9-Oct-1996 (CT) Replaced "'(lambda" by "(function (lambda"
;;;;--
(provide 'lse-mark-stack)

(defun lse-new-mark-stack ()
  ;; Creates a new mark-stack. Last-mark and home-mark are initialized to the
  ;; current position of the current buffer.
  (list (list (point-marker) (point-marker)) (list 1))
)

(defun lse-delete-mark-stack (lse@mark@stack)
  (let ((l (append nil lse@mark@stack)))
    (mapcar (function (lambda (m) (if (markerp m) (setq m nil)))) l)
  )
)

(defvar lse@mark@stack nil 
  "Bound by global-mark, buffer-mark and window-mark routines."
)

;;;;+ 
;;;; Internal core functions working on mark-stack bound to 'lse@mark@stack'.
;;;; 
;;;; !!!!! DO NOT USE THESE FROM OUTSIDE THIS LIBRARY !!!!!
;;;;-
(defun lse@implicit@mark (access)
  (or (funcall access (car lse@mark@stack))
      (point-marker)
  )
)

(defun lse@set@implicit@mark (access to-mark)
  (if (consp lse@mark@stack)
      (funcall access (car lse@mark@stack) to-mark)
  )
)

(defun lse@top@mark ()
  (or (nth 1 (car (cdr lse@mark@stack)))
      (point-marker)
  )
)

(defun lse@pop@mark ()
  (setcdr (car (cdr lse@mark@stack))
          (cdr (cdr (car (cdr lse@mark@stack))))
  )
)

(defun lse@push@mark ()
  (setcdr (car (cdr lse@mark@stack))
          (cons (point-marker) (cdr (car (cdr lse@mark@stack))))
  )
  (message "Mark %d pushed" (lse@mark@stack@depth))
)

(defun lse@mark@stack@depth ()
  (1- (length (car (cdr lse@mark@stack))))
)

(defun lse@last@mark ()
  (lse@implicit@mark 'car)
)

(defun lse@home@mark ()
  (or (lse@implicit@mark 'cdr)
      (let ((lse@mark@stack lse-global-mark@stack)
           )
        (lse@implicit@mark 'cdr)
      )
  )
)

(defun lse@set@last@mark (&optional to-mark)
  (lse@set@implicit@mark 'setcar (or to-mark (point-marker)))
)

(defun lse@set@home@mark (&optional to-mark)
  (lse@set@implicit@mark 'setcdr (or to-mark (point-marker)))
)

;;;;+
;;;; Auxiliary functions for mark handling independent of mark-stacks.
;;;;-
(defun lse-goto-mark (to-mark do-before)
  ;; Save TO-MARK, execute DO-BEFORE and goto TO-MARK.
  (let ((target (copy-marker (if (consp to-mark) (car to-mark) to-mark))))
    (funcall           do-before)
    (lse-goto-position target)
    (set-marker        target nil)
  )
)

(defun lse-goto-position (position)
  ;; POSITION can be a mark or plain position
  (if position
      (progn
        (if (markerp position)
            (let ((buf (marker-buffer position)))
              (or (equal (current-buffer) buf)
                  (switch-to-buffer     buf t)
              )
              (setq position (marker-position position))
            )
        )
        (if (integerp position)
            (goto-char position)
        )
      )
  )
)

;;;;+
;;;; Interface functions of 'mark-stack's. The parameter specifies the
;;;; mark-stack to work on.
;;;;-
;;;  6-Jan-1995 lse-mark-stack.el
(defun lse-home-mark (lse@mark@stack)
  (lse@home@mark)
; lse-home-mark
)

(defun lse-goto-last-mark (lse@mark@stack)
  (lse-goto-mark (lse@last@mark) 'lse@set@last@mark)
)

(defun lse-goto-home-mark (lse@mark@stack)
  (lse-goto-mark (lse@home@mark) 'lse@set@last@mark)
)

(defun lse-set-last-mark (lse@mark@stack &optional to-mark)
  (lse@set@last@mark to-mark)
)

(defun lse-set-home-mark (lse@mark@stack &optional to-mark)
  (lse@set@home@mark to-mark)
)

(defun lse-push-mark (lse@mark@stack)
  (lse@push@mark)
)

(defun lse-goto-mark-and-pop (lse@mark@stack)
  (lse-goto-position (lse@top@mark))
  (lse@pop@mark)
  (message "%d marks remaining" (lse@mark@stack@depth))
)

(defun lse-toggle-mark (lse@mark@stack)
  (lse-goto-mark (lse@top@mark)
                 (function (lambda () (lse@pop@mark) (lse@push@mark)))
  )
)

;;;;+
;;;; Commands implementing the global-mark-stack.
;;;;- 

;;; (setq lse-global-mark@stack (lse-new-mark-stack))
(defvar lse-global-mark@stack (lse-new-mark-stack)
  ;; List of marks defined globally (i.e., neither buffer- nor window-specific)
  ;; by user.
)

(defun lse-goto-last-mark-global ()
  "Goto global last mark."
  (interactive)
  (lse-goto-last-mark lse-global-mark@stack)
)

(defun lse-goto-home-mark-global ()
  "Goto global home mark."
  (interactive)
  (lse-goto-buffer (lse-buffer:main))
)

(defun lse-set-last-mark-global (&optional to-mark)
  "Set global last mark to 'to-mark'."
  (interactive "d")
  (lse-set-last-mark lse-global-mark@stack 
                       (if (integerp to-mark) (copy-marker to-mark) to-mark)
  )
)

(defun lse-set-home-mark-global (to-mark)
  "Set global last mark to 'to-mark'."
  (interactive "d")
  (lse-set-home-mark lse-global-mark@stack 
                       (if (integerp to-mark) (copy-marker to-mark) to-mark)
  )
  (message (format "Global home mark set to buffer %s"
                   (buffer-name (current-buffer))
           )
  );  3-Nov-1994 
)

(defun lse-push-mark-global ()
  "Push mark onto global mark stack."
  (interactive)
  (lse-push-mark lse-global-mark@stack)
)

(defun lse-goto-mark-and-pop-global ()
  "Goto top mark of global mark stack and remove it."
  (interactive)
  (lse-goto-mark-and-pop lse-global-mark@stack)
)

(defun lse-toggle-mark-global ()
  "Goto top mark of global mark-stack and replace it by previous position."
  (interactive)
  (lse-toggle-mark lse-global-mark@stack)
)

(defun lse-set-last-mark-all ()
  (lse-set-last-mark-global) 
  (lse-set-last-mark-buffer)
  (lse-set-last-mark-window)
)
