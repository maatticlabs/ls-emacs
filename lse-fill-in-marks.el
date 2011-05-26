;-*- coding: iso-8859-15; -*-

;;;;unix_ms_filename_correspondency lse-fill-in-marks:el lse_fmar:el
;;;; Copyright (C) 1997-2010 Mag. Christian Tanzer. All rights reserved.
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
;;;;    lse-fill-in-marks
;;;;
;;;; Purpose
;;;;    Administration of fill-in marks
;;;;
;;;; Revision Dates
;;;;    29-Dec-1997 (CT) Creation (based on functions from tempo.el by
;;;;                     David K}gedal <davidk@lysator.liu.se>)
;;;;    18-Nov-2009 (CT) `lse-fill-in-marks:goto-open-head` and
;;;;                     `lse-fill-in-marks:goto-open-tail` added
;;;;    10-Nov-2010 (CT) Use `mapc` instead of `mapcar` where appropriate
;;;;    ««revision-date»»···
;;;;--

;;; 29-Dec-1997
(defvar                      lse-fill-in-marks:head nil)
(make-variable-buffer-local 'lse-fill-in-marks:head)

;;; 29-Dec-1997
(defvar                      lse-fill-in-marks:tail nil)
(make-variable-buffer-local 'lse-fill-in-marks:tail)

;;; 29-Dec-1997
(defun lse-fill-in-marks:insert-mark (m marks)
  "Insert mark `m' into mark-list `marks' while keeping it sorted"
  (let ((mrk (copy-marker m)))
    (cond ((null (symbol-value marks)) (set marks (list mrk)))
          ((< mrk (car (symbol-value marks)))
           (set marks (cons mrk (symbol-value marks)))
          )
          (t
           (let ((lp (symbol-value marks)))
             (while (and (cdr lp)
                         (<= (car (cdr lp)) mrk)
                    )
               (setq lp (cdr lp))
             )
             (if (not (= mrk (car lp)))
                 (setcdr lp (cons mrk (cdr lp)))
             )
           )
          )
    )
  )
; lse-fill-in-marks:insert-mark
)

;;; 29-Dec-1997
(defun lse-fill-in-marks:insert-head (m)
  (lse-fill-in-marks:insert-mark m 'lse-fill-in-marks:head)
; lse-fill-in-marks:insert-head
)

;;; 29-Dec-1997
(defun lse-fill-in-marks:insert-tail (m)
  (lse-fill-in-marks:insert-mark m 'lse-fill-in-marks:tail)
; lse-fill-in-marks:insert-tail
)

;;; 29-Dec-1997
(defun lse-fill-in-marks:goto-next (marks)
  "Jump to the next mark in `marks'."
  (let ((next-mark
          (catch 'found
            (mapc
              (function
                (lambda (mark) (if (< (point) mark) (throw 'found mark)))
              )
              (symbol-value marks)
            )
            ;; return nil if not found
            nil
          )
        )
       )
    (if next-mark (goto-char next-mark))
  )
; lse-fill-in-marks:goto-next
)

;;; 29-Dec-1997
(defun lse-fill-in-marks:goto-prev (marks)
  "Jump to the prev mark in `marks'."
  (let ((prev-mark
          (catch 'found
            (let (last)
              (mapc
                (function
                  (lambda (mark)
                    (if (<= (point) mark) (throw 'found last))
                    (setq last mark)
                  )
                )
                (symbol-value marks)
              )
              last
            )
          )
        )
       )
    (if prev-mark (goto-char prev-mark))
  )
; lse-fill-in-marks:goto-prev
)

;;; 29-Dec-1997
(defun lse-fill-in-marks:goto-next-head ()
  "Goto head-mark of next fill-in in buffer."
  (interactive)
  (lse-fill-in-marks:goto-next 'lse-fill-in-marks:head)
; lse-fill-in-marks:goto-next-head
)

;;; 29-Dec-1997
(defun lse-fill-in-marks:goto-next-tail ()
  "Goto tail-mark of next fill-in in buffer."
  (interactive)
  (lse-fill-in-marks:goto-next 'lse-fill-in-marks:tail)
; lse-fill-in-marks:goto-next-tail
)

;;; 29-Dec-1997
(defun lse-fill-in-marks:goto-prev-head ()
  "Goto head-mark of prev fill-in in buffer."
  (interactive)
  (lse-fill-in-marks:goto-prev 'lse-fill-in-marks:head)
; lse-fill-in-marks:goto-prev-head
)

;;; 29-Dec-1997
(defun lse-fill-in-marks:goto-prev-tail ()
  "Goto tail-mark of prev fill-in in buffer."
  (interactive)
  (lse-fill-in-marks:goto-prev 'lse-fill-in-marks:tail)
; lse-fill-in-marks:goto-prev-tail
)

;;; 18-Nov-2009
(defun lse-fill-in-marks:goto-open-head ()
  "Goto head of currently open replacement."
  (interactive)
  (if lse_replaced_fill-in
      (progn
        (let* ((range      (lse-fill-in:range      lse_replaced_fill-in))
               (head-pos   (lse-range:head-pos     range))
              )
          (goto-char head-pos)
        )
      )
  )
; lse-fill-in-marks:goto-open-head
)

;;; 18-Nov-2009
(defun lse-fill-in-marks:goto-open-tail ()
  "Goto tail of currently open replacement."
  (interactive)
  (if lse_replaced_fill-in
      (progn
        (let* ((range      (lse-fill-in:range      lse_replaced_fill-in))
               (tail-pos   (lse-range:tail-pos     range))
              )
          (goto-char tail-pos)
        )
      )
  )
; lse-fill-in-marks:goto-open-tail
)

(provide 'lse-fill-in-marks)

;;; lse-fill-in-marks.el ends here
