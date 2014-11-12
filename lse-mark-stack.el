;-*- coding: utf-8 -*-

;;;; Copyright (C) 1994-2014 Mag. Christian Tanzer. All rights reserved.
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
;;;;    lse-mark-stack
;;;;
;;;; Purpose
;;;;    A mark-stack comprises two lists:
;;;;
;;;;    - the first list contains the last-mark (last position saved
;;;;      implicitly) and the home-mark
;;;;    - the second list contains the marks stacked on explicit request
;;;;      by the user (the element at position zero points to the
;;;;      current mark of the stack)
;;;;
;;;;    It's possible to use several independent mark stacks. The internal
;;;;    core functions (identified by '@'s as name separators) work on the
;;;;    mark-stack bound to 'lse-mark-stack::instance'.
;;;;    Do not use these directly!
;;;;
;;;;    The interface functions take the mark-stack as parameter. Use the
;;;;    commands implementing the global mark-stack as documentation how to
;;;;    use them.
;;;;
;;;; Revision Dates
;;;;    12-Dec-1993 (CT) Creation
;;;;     9-Oct-1996 (CT) Replaced "'(lambda" by "(function (lambda"
;;;;    17-Nov-2009 (CT) `lse-global-home-mark-initialized` and
;;;;                     `lse-home-mark-global` added
;;;;    10-Nov-2010 (CT) Use `mapc` instead of `mapcar` where appropriate
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-mark-stack)

(defun lse-new-mark-stack ()
  ;; Creates a new mark-stack. Last-mark and home-mark are initialized to the
  ;; current position of the current buffer.
  (list (list (point-marker) (point-marker)) (list 1))
)

(defun lse-delete-mark-stack (lse-mark-stack::instance)
  (let ((l (append nil lse-mark-stack::instance)))
    (mapc (function (lambda (m) (if (markerp m) (setq m nil)))) l)
  )
)

(defvar lse-mark-stack::instance nil
  "Bound by global-mark, buffer-mark and window-mark routines."
)

(defvar lse-mark-stack:global (lse-new-mark-stack)
  "List of globally user-defined marks,
 i.e., neither buffer- nor window-specific"
)

(defvar lse-global-home-mark-initialized nil)


;;;;+
;;;; Internal core functions working on mark-stack bound to
;;;; 'lse-mark-stack::instance'.
;;;;
(defun lse-mark-stack::implicit (access)
  (or (funcall access (car lse-mark-stack::instance))
      (point-marker)
  )
)

(defun lse-mark-stack::implicit:set (access to-mark)
  (if (consp lse-mark-stack::instance)
      (funcall access (car lse-mark-stack::instance) to-mark)
  )
)

(defun lse-mark-stack::implicit:top ()
  (or (nth 1 (car (cdr lse-mark-stack::instance)))
      (point-marker)
  )
)

(defun lse-mark-stack::implicit:pop ()
  (setcdr (car (cdr lse-mark-stack::instance))
          (cdr (cdr (car (cdr lse-mark-stack::instance))))
  )
)

(defun lse-mark-stack::implicit:push ()
  (setcdr
    (car (cdr lse-mark-stack::instance))
    (cons (point-marker) (cdr (car (cdr lse-mark-stack::instance))))
  )
  (message "Mark %d pushed" (lse-mark-stack::implicit:depth))
)

(defun lse-mark-stack::implicit:depth ()
  (1- (length (car (cdr lse-mark-stack::instance))))
)

(defun lse-mark-stack::implicit:last ()
  (lse-mark-stack::implicit 'car)
)

(defun lse-mark-stack::implicit:home ()
  (or (lse-mark-stack::implicit 'cdr)
      (let ((lse-mark-stack::instance lse-mark-stack:global)
           )
        (lse-mark-stack::implicit 'cdr)
      )
  )
)

(defun lse-mark-stack::implicit:set-last (&optional to-mark)
  (lse-mark-stack::implicit:set 'setcar (or to-mark (point-marker)))
)

(defun lse-mark-stack::implicit:set-home (&optional to-mark)
  (lse-mark-stack::implicit:set 'setcdr (or to-mark (point-marker)))
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
(defun lse-home-mark (lse-mark-stack::instance)
  (lse-mark-stack::implicit:home)
; lse-home-mark
)

(defun lse-goto-last-mark (lse-mark-stack::instance)
  (lse-goto-mark
    (lse-mark-stack::implicit:last) 'lse-mark-stack::implicit:set-last
  )
)

(defun lse-goto-home-mark (lse-mark-stack::instance)
  (lse-goto-mark
    (lse-mark-stack::implicit:home) 'lse-mark-stack::implicit:set-last
  )
)

(defun lse-set-last-mark (lse-mark-stack::instance &optional to-mark)
  (lse-mark-stack::implicit:set-last to-mark)
)

(defun lse-set-home-mark (lse-mark-stack::instance &optional to-mark)
  (lse-mark-stack::implicit:set-home to-mark)
)

(defun lse-push-mark (lse-mark-stack::instance)
  (lse-mark-stack::implicit:push)
)

(defun lse-goto-mark-and-pop (lse-mark-stack::instance)
  (lse-goto-position (lse-mark-stack::implicit:top))
  (lse-mark-stack::implicit:pop)
  (message "%d marks remaining" (lse-mark-stack::implicit:depth))
)

(defun lse-toggle-mark (lse-mark-stack::instance)
  (lse-goto-mark
    (lse-mark-stack::implicit:top)
    (function (lambda () (lse-mark-stack::implicit:pop) (lse-mark-stack::implicit:push)))
  )
)

;;;;+
;;;; Commands implementing the global-mark-stack.
;;;;-

;;; 17-Nov-2009
(defun lse-goto-last-mark-global ()
  "Goto global last mark."
  (interactive)
  (lse-goto-last-mark lse-mark-stack:global)
)

(defun lse-goto-home-mark-global ()
  "Goto global home mark."
  (interactive)
  (lse-goto-position (lse-home-mark-global))
)

;;; 17-Nov-2009
(defun lse-home-mark-global ()
  (lse-home-mark lse-mark-stack:global)
; lse-home-mark-global
)

(defun lse-set-last-mark-global (&optional to-mark)
  "Set global last mark to 'to-mark'."
  (interactive "d")
  (lse-set-last-mark lse-mark-stack:global
                       (if (integerp to-mark) (copy-marker to-mark) to-mark)
  )
)

(defun lse-set-home-mark-global (to-mark)
  "Set global last mark to 'to-mark'."
  (interactive "d")
  (setq lse-global-home-mark-initialized t)
  (lse-set-home-mark lse-mark-stack:global
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
  (lse-push-mark lse-mark-stack:global)
)

(defun lse-goto-mark-and-pop-global ()
  "Goto top mark of global mark stack and remove it."
  (interactive)
  (lse-goto-mark-and-pop lse-mark-stack:global)
)

(defun lse-toggle-mark-global ()
  "Goto top mark of global mark-stack and replace it by previous position."
  (interactive)
  (lse-toggle-mark lse-mark-stack:global)
)

(defun lse-set-last-mark-all ()
  (lse-set-last-mark-global)
  (lse-set-last-mark-buffer)
  (lse-set-last-mark-window)
)

;;; __END__ lse-mark-stack.el
