;-*- coding: iso-8859-15; -*-

;;;; Copyright (C) 2003-2012 Mag. Christian Tanzer. All rights reserved
;;;; Glasauergasse 32, A--1130 Wien, Austria. tanzer@swing.co.at
;;;; ****************************************************************************
;;;; This file is part of LS-Emacs, a package built on top of GNU Emacs.
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
;;;;    lse-cal
;;;;
;;;; Purpose
;;;;    Functions for dealing with a simple calendar
;;;;
;;;; Revision Dates
;;;;     3-Apr-2003 (CT) Creation
;;;;     4-Apr-2003 (CT) Creation continued
;;;;     5-Apr-2003 (CT) Creation continued...
;;;;     6-Apr-2003 (CT) Creation continued....
;;;;     7-Apr-2003 (CT) Creation continued.....
;;;;     9-Apr-2003 (CT) Creation continued......
;;;;     9-Apr-2003 (CT) lse-cal:setup-diary added
;;;;    10-Apr-2003 (CT) Guards added to `lse-cal:plan:sync-to-view` and
;;;;                     `lse-cal:view:sync-to-plan`
;;;;    10-Apr-2003 (CT) `lse-cal:plan:write` added
;;;;    20-Apr-2003 (CT) `lse-cal:plan:setup-font-lock` changed to properly
;;;;                     deal with holidays
;;;;    21-Apr-2003 (CT) Redefinitions for `lse-cal:plan:search` and friends
;;;;                     added
;;;;    18-May-2003 (CT) `lse-cal:plan:write` removed (after fixing
;;;;                     lse-buffer)
;;;;     3-Jul-2003 (CT) Redefinition for `dabbrev-expand` added
;;;;     9-Feb-2007 (CT) `lse-cal:plan:highlight-today` changed to put cursor
;;;;                     at the line after the day-pattern (to be consistent
;;;;                     with `lse-cal:plan:goto-day-forward` and friends)
;;;;     9-Feb-2007 (CT) `lse-cal:view:add-appointment` added and bound to
;;;;                     `[?\A-e]` of view-buffer
;;;;    28-Mar-2007 (CT) `lse-frame:disable-menu-bar` called
;;;;    10-Apr-2007 (CT) Default geometry for diary and calendar frames changed
;;;;     5-Oct-2007 (CT) Replace `search` by `search-forward` and
;;;;                     `search-reverse`
;;;;     9-Oct-2007 (CT) s/lse-tpu:regexp-prompt/lse-tpu:search-prompt-read/g
;;;;    11-Oct-2007 (CT) `lse-cal:plan:highlight-today` changed to delete old
;;;;                     highlight, if any
;;;;    11-Oct-2007 (CT) `lse-cal:switch-diary` factored
;;;;    12-Oct-2007 (CT) `lse-cal:diary:next-day` and friends added
;;;;    15-Oct-2007 (CT) Guard `lse-cal:plan-buffer` added to
;;;;                     `lse-cal:view:goto-month`
;;;;    15-Oct-2007 (CT) `current-year-p` added to `lse-cal:setup-year`
;;;;     5-Jun-2008 (CT) `lse-cal:plan:highlight-today` changed to use
;;;;                     `lse-cal:view:goto-month` instead of
;;;;                     `lse-cal:plan:sync-to-view`
;;;;    17-Nov-2009 (CT) `lse-cal:setup-year-frame` factored
;;;;                     (and used for `frame-setup` frame property)
;;;;    25-Feb-2012 (CT) Use `lse-tpu:next-line-internal` instead of
;;;;                     `next-line-internal`
;;;;    26-Nov-2012 (CT) Change `mode-line-format` of `lse-cal:view:mode`
;;;;    ««revision-date»»···
;;;;--

;;;
;;; This module provides functions for managing two coupled buffers which
;;; together present a calendar with appointments. The view buffer displays
;;; the weeks of a year, the plan buffer contains an entry per day. Each
;;; day-entry in the plan comprises a header line, lines for appointments,
;;; comments, other stuff and an empty trailer line.
;;;
;;; The plan and view buffers are linked (by `lse-cal:setup-year`) so that
;;; movements of point in one buffer result in approbriate movements of point
;;; in the other -- both buffers should always show the same selected day.
;;; The point linkage is provided by binding `lse-cal:plan:sync-to-view` and
;;; `lse-cal:view:sync-to-plan` to `point-entered` text-properties.
;;;
;;; To avoid spurious linkage, many functions of this module must temporarily
;;; let `inhibit-point-motion-hooks` before moving point in one of the
;;; buffers. As the view buffer is completely read-only, and the day-header
;;; lines of the plan buffer are also read-only, some functions also need to
;;; let `inhibit-read-only` before trying to do changes.
;;;

(provide 'lse-cal)
(require 'lse-face)
(require 'lse-tpu)
(require 'lse-frame)

;;;  3-Apr-2003
(defvar lse-cal:day-pat
        (concat "^# "
                "\\([0-9]\\{4\\}/[0-9]\\{2\\}/[0-9]\\{2\\}\\)"
                "\\s +"
                "\\([A-Z][a-z]\\{2\\}\\)"
                "#"
                "\\([0-9]\\{2\\}\\)"
                ".*$"
        )
)

(defvar lse-cal:current-day-overlay             nil)
(defvar lse-cal:plan-buffer                     nil)
(defvar lse-cal:plan:today-overlay              nil)
(defvar lse-cal:view-buffer                     nil)
(defvar lse-cal:view:today-month-overlay        nil)
(defvar lse-cal:view:today-overlay              nil)
(defvar lse-cal:view:today-week-overlay         nil)
(make-variable-buffer-local 'lse-cal:current-day-overlay)
(make-variable-buffer-local 'lse-cal:plan-buffer)
(make-variable-buffer-local 'lse-cal:plan:today-overlay)
(make-variable-buffer-local 'lse-cal:view-buffer)
(make-variable-buffer-local 'lse-cal:view:today-month-overlay)
(make-variable-buffer-local 'lse-cal:view:today-overlay)
(make-variable-buffer-local 'lse-cal:view:today-week-overlay)

;;;  4-Apr-2003
(defun lse-cal:highlight (head tail face)
  (let ((overlay (make-overlay head tail)))
    (overlay-put overlay 'face face)
    overlay
  )
; lse-cal:highlight
)

;;;  3-Apr-2003
(defun lse-cal:plan:goto-day-backward (num)
  (interactive "p")
  (save-match-data
    (and (looking-at lse-cal:day-pat) (lse-tpu:forward-line 1))
    (and lse-cal:current-day-overlay
         (delete-overlay lse-cal:current-day-overlay)
    )
    (let (head tail
          (inhibit-point-motion-hooks t)
         )
      (while (and (not (eobp)) (> num 0))
        (setq tail (progn (lse-cal:prev-day) (lse-tpu:line-head-pos 0)))
        (setq num  (1- num))
      )
      (setq head (lse-cal:prev-day))
      (setq lse-cal:current-day-overlay
            (lse-cal:highlight head tail 'lse-face:cal:current)
      )
      (lse-tpu:next-line 1)
    )
  )
; lse-cal:plan:goto-day-backward
)

;;;  3-Apr-2003
(defun lse-cal:plan:goto-day-forward (num &optional stay-on-day-pat)
  (interactive "p")
  (let (head tail
        (inhibit-point-motion-hooks t)
       )
    (save-match-data
      (and (looking-at lse-cal:day-pat) (lse-tpu:forward-char 1))
      (if lse-cal:current-day-overlay
          (delete-overlay lse-cal:current-day-overlay)
      )
      (while (and (not (eobp)) (> num 0))
        (setq head (lse-cal:next-day))
        (setq num  (1- num))
        (lse-tpu:forward-char 1)
      )
      (setq tail (progn (lse-cal:next-day) (lse-tpu:line-head-pos 0)))
      (goto-char head)
      (setq lse-cal:current-day-overlay
            (lse-cal:highlight head tail 'lse-face:cal:current)
      )
      (or stay-on-day-pat (lse-tpu:next-line 1))
    )
    (cons head tail)
  )
; lse-cal:plan:goto-day-forward
)

;;;  5-Apr-2003
(defun lse-cal:next-match (pat &optional n)
  (let ((inhibit-point-motion-hooks t))
    (if (re-search-forward pat nil t)
        (goto-char (match-beginning (or n 0)))
      (goto-char (point-max))
    )
  )
  (point)
; lse-cal:next-match
)

;;;  5-Apr-2003
(defun lse-cal:prev-match (pat)
  (let ((inhibit-point-motion-hooks t))
    (if (re-search-backward pat nil t)
        (goto-char (match-beginning 0))
      (goto-char (point-min))
    )
  )
  (point)
; lse-cal:prev-match
)

;;;  3-Apr-2003
(defun lse-cal:next-day ()
  (interactive)
  (lse-cal:next-match lse-cal:day-pat)
; lse-cal:next-day
)

;;;  3-Apr-2003
(defun lse-cal:prev-day ()
  (interactive)
  (lse-cal:prev-match lse-cal:day-pat)
; lse-cal:prev-day
)

;;;  6-Apr-2003
(defun lse-cal:plan:sync-to-view (old-p new-p)
  (if lse-cal:view-buffer
      (let* ((w  (get-text-property new-p 'week))
             (_d (get-text-property new-p 'date))
             (d  (and _d (substring _d 8 10)))
             (vw (get-buffer-window lse-cal:view-buffer))
             (pw (selected-window))
             (inhibit-point-motion-hooks t)
            )
        (if (and vw w d)
            (progn
              (if (string= (substring d 0 1) "0")
                  (setq d (concat " " (substring d 1 2)))
              )
              (select-window vw)
              (let (lse-cal:plan-buffer ;  break recursion
                    (inhibit-point-motion-hooks t)
                   )
                (lse-tpu:move-to-beginning)
                (lse-cal:next-match
                  (concat "^" w "\\( [ 0-3][0-9]\\)*\\( " d " \\)") 2
                )
                (lse-tpu:forward-char 1)
                (lse-cal:view:highlight-current-day)
                (lse-scroll-to-top 3)
                (select-window pw)
              )
            )
        )
      )
  )
; lse-cal:plan:sync-to-view
)

;;;  3-Apr-2003
(defun lse-cal:plan:setup-font-lock ()
  (interactive)
  (let ((inhibit-read-only t)
        (i 0)
       )
    (setq font-lock-defaults
      '(( ( "^\\(> [-:0-9 A-Za-z]+ <\\) \\(.*\\)$"
           (1 'lse-face:cal:time-field nil t)
           (2 'lse-face:cal:text       nil t)
          )
          ( "^  +.*$"                 . 'lse-face:cal:desc)
          ( "^#.*=.*= *\n"            . 'lse-face:cal:holiday)
          ( "^#.*\\(Sat\\|Sun\\).*\n" . 'lse-face:cal:week-end)
          ( "^#.*\n"                  . 'lse-face:cal:day-line)
        )
        nil
        nil
        nil
        nil
       )
    )
    (font-lock-mode 0)
    (font-lock-mode 1)
    (lse-tpu:move-to-beginning)
    (while (not (eobp))
      (let* ((day (lse-cal:plan:goto-day-forward 1 't)); advances to next day
             (eol (lse-tpu:line-tail-pos 1))
             head tail
            )
        (if (looking-at lse-cal:day-pat); updates match data
            (progn
              (setq head (car day))
              (setq tail
                    (save-excursion
                      (goto-char (or (cdr day) (point-max)))
                      (lse-tpu:line-head-pos 2)
                    )
              )
              (add-text-properties head tail
                (list 'point-entered 'lse-cal:plan:sync-to-view
                      'day-of-year   i
                      'date          (match-string-no-properties 1)
                      'weekday       (match-string-no-properties 2)
                      'week          (match-string-no-properties 3)
                )
              )
              (add-text-properties head eol (list 'intangible i))
              (setq i (1+ i))
            )
          (lse-tpu:move-to-end); out of here
        )
      )
    )
  )
; lse-cal:plan:setup-font-lock
)

;;;  5-Apr-2003
(defun lse-cal:plan:highlight-today ()
  (interactive)
  (let ((old-p (point))
        (inhibit-point-motion-hooks t)
       )
    (with-current-buffer lse-cal:view-buffer
      (lse-cal:view:goto-month)
    )
    (if lse-cal:plan:today-overlay
        (delete-overlay lse-cal:plan:today-overlay)
    )
    (lse-tpu:move-to-beginning)
    (if (re-search-forward (lse-yyyy/mm/dd) nil t)
        (let (head tail)
          (goto-char (match-beginning 0))
          (beginning-of-line)
          (setq head (point))
          (lse-tpu:forward-char 1)
          (setq tail (progn (lse-cal:next-day) (lse-tpu:line-head-pos 0)))
          (setq lse-cal:plan:today-overlay
              (lse-cal:highlight head tail 'lse-face:cal:today)
          )
          (goto-char head)
          (lse-scroll-to-top 3)
          (lse-tpu:next-line 1);  9-Feb-2007
        )
    )
  )
; lse-cal:plan:highlight-today
)

;;; 21-Apr-2003
(defun lse-cal:plan:search-forward ()
  (interactive)
  (let ((inhibit-point-motion-hooks t))
    (call-interactively 'lse-tpu:search-forward)
  )
; lse-cal:plan:search-forward
)

;;;  5-Oct-2007
(defun lse-cal:plan:search-reverse ()
  (interactive)
  (let ((inhibit-point-motion-hooks t))
    (call-interactively 'lse-tpu:search-reverse)
  )
; lse-cal:plan:search-reverse
)
;;; 21-Apr-2003
(defun lse-cal:plan:search-again-forward ()
  (interactive)
  (let ((inhibit-point-motion-hooks t))
    (call-interactively 'lse-tpu:search-again-forward)
  )
; lse-cal:plan:search-again-forward
)

;;; 21-Apr-2003
(defun lse-cal:plan:search-again-reverse ()
  (interactive)
  (let ((inhibit-point-motion-hooks t))
    (call-interactively 'lse-tpu:search-again-reverse)
  )
; lse-cal:plan:search-again-reverse
)

;;; 21-Apr-2003
(defun lse-cal:plan:replace (from to &optional head-limit tail-limit)
  (interactive (list (lse-tpu:search-prompt-read "replace: ")
                     (lse-tpu:search-prompt-read "by: ")
               )
  )
  (let ((inhibit-point-motion-hooks t))
    (lse-tpu:replace from to head-limit tail-limit)
  )
; lse-cal:plan:replace
)

;;;  3-Jul-2003
(defun lse-cal:dabbrev-expand ()
  (interactive)
  (let ((inhibit-point-motion-hooks t))
    (call-interactively 'dabbrev-expand)
  )
; lse-cal:dabbrev-expand
)

;;;  9-Mar-2003
(define-derived-mode lse-cal:plan:mode text-mode "Cal"
  "Major mode for calendar"
  (lse-language:use "cal")
  (lse-cal:plan:setup-font-lock)
  (lse-key-template-tab-l); 10-Mar-2003
  (local-set-key [M-down]  'lse-cal:plan:goto-day-forward)
  (local-set-key [M-up]    'lse-cal:plan:goto-day-backward)
  (local-set-key [M-home]  'lse-cal:plan:highlight-today)
  (local-set-key [?\C-f]   'lse-cal:plan:search-forward);  5-Oct-2007
  (local-set-key [?\s-f]   'lse-cal:plan:search-reverse);  5-Oct-2007
  (local-set-key [?\C-n]   'lse-cal:plan:search-again-forward)
  (local-set-key [?\C-p]   'lse-cal:plan:search-again-reverse)
  (local-set-key [?\C-:]   'lse-cal:plan:replace)
  (local-set-key [?\A-d]   'lse-cal:dabbrev-expand)
  (set-buffer-modified-p nil)
  (lse-cal:plan:highlight-today)
)

;;;  5-Apr-2003
(defun lse-cal:view:next-month ()
  (interactive)
  (let (head tail
        (pat "  1")
        (inhibit-point-motion-hooks t)
       )
    (setq head (1+ (lse-cal:next-match pat)))
    (lse-tpu:forward-char 1)
    (setq tail (1+ (lse-cal:next-match pat)))
    (cons head tail)
  )
; lse-cal:view:next-month
)

;;; 15-Apr-2003
(defun lse-cal:view:goto-next-week (num)
  "Move to next week"
  (interactive "p")
  (let ((old-p (point))
        (inhibit-point-motion-hooks t)
        (goal-column (current-column))
       )
    (while (> num 0)
      (let ((op (point)))
        (lse-tpu:next-line-internal 1)
        (if (eobp)
            (progn
              (goto-char op)
              (setq num 0)
            )
          (setq num (1- num))
        )
      )
    )
    (lse-cal:view:sync-to-plan old-p (point))
  )
; lse-cal:view:goto-next-week
)

;;; 15-Apr-2003
(defun lse-cal:view:goto-prev-week (num)
  "Move to prev week"
  (interactive "p")
  (let ((old-p (point))
        (inhibit-point-motion-hooks t)
        (goal-column (current-column))
       )
    (while (> num 0)
      (let ((op (point)))
        (lse-tpu:next-line-internal -1)
        (if (bobp)
            (progn
              (goto-char op)
              (setq num 0)
            )
          (setq num (1- num))
        )
      )
    )
    (lse-cal:view:sync-to-plan old-p (point))
  )
; lse-cal:view:goto-prev-week
)

;;;  5-Apr-2003
(defun lse-cal:view:goto-month (&optional m d)
  (interactive)
  (if (not m) (setq m (lse-date-month)))
  (if (not d) (setq d (lse-date-day)))
  (if lse-cal:view:today-month-overlay
      (delete-overlay lse-cal:view:today-month-overlay)
  )
  (if lse-cal:view:today-overlay
      (delete-overlay lse-cal:view:today-overlay)
  )
  (if lse-cal:view:today-week-overlay
      (delete-overlay lse-cal:view:today-week-overlay)
  )
  (if lse-cal:plan-buffer
      (let ((pat m)
            (inhibit-point-motion-hooks t)
            (old-p (point))
            month
            p q
           )
        (lse-tpu:move-to-beginning)
        (lse-cal:next-match pat)
        (beginning-of-line)
        (setq month (lse-cal:view:next-month))
        (setq lse-cal:view:today-month-overlay
          (lse-cal:highlight (car month) (cdr month) 'lse-face:cal:this-month)
        )
        (goto-char (car month))
        (if d
            (progn
              (setq p (lse-cal:next-match (concat " \\(" d "\\) ") 1))
              (setq lse-cal:view:today-overlay
                (lse-cal:highlight
                  (match-beginning 0) (match-end 0) 'lse-face:cal:today
                )
              )
              (setq q (lse-tpu:line-head-pos 1))
              (setq lse-cal:view:today-week-overlay
                (lse-cal:highlight q (+ q 3) 'lse-face:cal:this-week)
              )
            )
        )
        (lse-scroll-to-top 3)
        (lse-cal:view:sync-to-plan old-p p)
      )
  )
; lse-cal:view:goto-month
)

;;;  9-Feb-2007
(defun lse-cal:view:add-appointment ()
  (interactive)
  (lse-next-window)
  (or (looking-at "^ *$") (lse-open-line))
  (lse-expand-token)
; lse-cal:view:add-appointment
)

;;;  7-Apr-2003
(defun lse-cal:view:highlight-current-day ()
  (interactive)
  (and  lse-cal:current-day-overlay
        (delete-overlay lse-cal:current-day-overlay)
  )
  (setq lse-cal:current-day-overlay
        (lse-cal:highlight (- (point) 1) (+ (point) 3) 'lse-face:cal:current)
  )
; lse-cal:view:highlight-current-day
)

;;;  6-Apr-2003
(defun lse-cal:view:sync-to-plan (old-p new-p)
  (if lse-cal:plan-buffer
      (let ((w  (get-text-property new-p 'week))
            (d  (get-text-property new-p 'weekday))
            (pw (get-buffer-window lse-cal:plan-buffer))
            (vw (selected-window))
            (inhibit-point-motion-hooks t)
           )
        (lse-cal:view:highlight-current-day)
        (if (and pw w d)
            (progn
              (select-window pw)
              (let (lse-cal:view-buffer ;  break recursion
                    (inhibit-point-motion-hooks t)
                   )
                (lse-tpu:move-to-beginning)
                (lse-cal:next-match (concat d "#" w))
                (lse-tpu:next-beginning-of-line 2)
                (lse-cal:plan:goto-day-forward 1)
                (lse-scroll-to-top 3)
                (select-window vw)
              )
            )
        )
      )
  )
; lse-cal:view:sync-to-plan
)

;;;  3-Apr-2003
(defun lse-cal:view:setup-font-lock ()
  (interactive)
  (let ((inhibit-read-only t))
    (setq font-lock-defaults
      '(( ( "^\\([0-5][0-9]\\) \\(..\\) \\(..\\) \\(..\\) \\(..\\) \\(..\\) \\(..\\) \\(..\\) \\([A-Z][a-z][a-z]\\) *$"
           (1 'lse-face:cal:week-field nil t)
           (2 'lse-face:cal:mon        nil t)
           (3 'lse-face:cal:tue        nil t)
           (4 'lse-face:cal:wed        nil t)
           (5 'lse-face:cal:thu        nil t)
           (6 'lse-face:cal:fri        nil t)
           (7 'lse-face:cal:sat        nil t)
           (8 'lse-face:cal:sun        nil t)
           (9 'lse-face:cal:month-name nil t)
          )
        )
        nil
        nil
        nil
        nil
       )
    )
    (font-lock-mode 0)
    (font-lock-mode 1)
    (lse-tpu:move-to-beginning)
    (let (month
          (i 1)
         )
      (while (< i 12)
        (setq month (lse-cal:view:next-month))
        (lse-cal:highlight (car month) (cdr month) 'lse-face:cal:odd)
        (setq month (lse-cal:view:next-month))
        (lse-cal:highlight (car month) (cdr month) 'lse-face:cal:even)
        (setq i (+ i 2))
      )
    )
    (lse-tpu:move-to-beginning)
    (while (not (eobp))
      (beginning-of-line)
      (if (looking-at "[0-5]")
          (let ((i    0)
                (p    (point))
                (tail (lse-tpu:line-tail-pos 1))
                (days '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
               )
            (add-text-properties p (+ p 3) '(intangible 6))
            (add-text-properties p tail
              (list 'week (buffer-substring-no-properties p (+ p 2)))
            )
            (forward-char 3)
            (setq p (point))
            (while (< i 7)
              (add-text-properties p (+ p 3)
                (list 'intangible i 'weekday (nth i days)
                      'point-entered 'lse-cal:view:sync-to-plan
                )
              )
              (forward-char 3)
              (setq p (point))
              (setq i (1+ i))
            )
            (forward-char -1)
            (add-text-properties p (1+ tail) '(intangible 6))
          )
      )
      (lse-tpu:forward-line 1)
    )
    (lse-tpu:move-to-beginning)
    (add-text-properties (point-min) (point-max) '(read-only t))
  )
; lse-cal:view:setup-font-lock
)

;;;  5-Apr-2003
(define-derived-mode lse-cal:view:mode text-mode "Cal-View"
  "Major mode for calendar"
  (setq lse-cal:plan-buffer nil)
  (lse-cal:view:setup-font-lock)
  (local-set-key [M-home] 'lse-cal:view:goto-month)
  (local-set-key [down]   'lse-cal:view:goto-next-week)
  (local-set-key [up]     'lse-cal:view:goto-prev-week)
  (local-set-key [?\A-e]  'lse-cal:view:add-appointment)
  ; XXX setup key bindings for calendar navigation
  (setq mode-line-format (list (purecopy " Wk  Mo Tu We Th Fr Sa Su Month")))
  (set-buffer-modified-p nil)
  (lse-cal:view:goto-month)
)

;;; 17-Nov-2009
(defun lse-cal:setup-year-frame (fram &optional year)
  (or year (setq year (lse-date-year)))
  (let ((current-year-p (string= year (lse-date-year))))
    (select-frame fram)
    (lse-frame:set-parameter
      'frame-setup (list 'lse-cal:setup-year-frame 'frame year) fram
    )
    (lse-frame:disable-menu-bar fram); 28-Mar-2007
    (let (pbuf vbuf)
      (lse-split-window-horizontally nil 34)
      (lse-goto-buffer
        (or (get-buffer (concat year ":plan"))
            (find-file  (concat "~/diary/" year "/plan"))
        )
      )
      (setq pbuf (current-buffer))
      (lse-next-window)
      (lse-goto-buffer
        (or (get-buffer (concat year ":view"))
            (find-file  (concat "~/diary/" year "/view"))
        )
      )
      (setq vbuf (current-buffer))
      (setq lse-cal:plan-buffer pbuf)
      (lse-tpu:move-to-beginning)
      (if current-year-p
          (lse-cal:view:goto-month)
        (lse-cal:view:goto-month "1" "1")
      )
      (lse-next-window)
      (lse-tpu:move-to-beginning)
      (setq lse-cal:view-buffer vbuf)
      (and current-year-p (lse-cal:plan:highlight-today))
    )
  )
; lse-cal:setup-year-frame
)

;;;  7-Apr-2003
(defun lse-cal:setup-year (year &optional x y ht wd)
  (interactive "sYear to setup (YYYY) \n")
  (let ((fram (lse-frame:make
               (concat (lse-user-initials) "'s calendar " year)
               (cons (or x 284) (or y 341))
               (cons (or wd 115) (or ht 24))
               '((font . "6x13") (Font . "6x13"))
              )
        )
       )
    (lse-cal:setup-year-frame fram year)
  )
; lse-cal:setup-year
)

;;;  9-Apr-2003
(defun lse-cal:setup-diary (&optional x y ht wd)
  (interactive)
  (let* ((d    (lse-yyyy/mm/dd))
         (fram (lse-frame:make
                nil
                (cons (or x 0) (or y 460))
                (cons (or wd 65) (or ht 11)); 11-Apr-2007 s/9/11/ for ht
                '((font . "6x13") (Font . "6x13"))
               )
         )
        )
    (select-frame fram)
    (lse-frame:disable-menu-bar fram); 28-Mar-2007
    (lse-cal:switch-diary d)
  )
; lse-cal:setup-diary
)

;;;  9-Apr-2003
(defun lse-cal:setup-year-and-diary (&optional x y ht wd)
  (interactive)
  (lse-cal:setup-year (lse-date-year) x y ht wd)
  (lse-cal:setup-diary)
; lse-cal:setup-year-and-diary
)

;;; 11-Oct-2007
(defun lse-cal:switch-diary (&optional d quiet)
  (interactive)
  (if (not d) (setq d (lse-yyyy/mm/dd)))
  (let ((df (lse-file:expanded-name (concat "~/diary/" d ".diary"))))
    (lse-goto-buffer (or (get-file-buffer df) (find-file df)))
    (unless quiet
      (lse-set-shorthosted-frame-title
        (concat (lse-user-initials) "'s Diary " d)
      )
    )
  )
; lse-cal:switch-diary
)

;;; 12-Oct-2007
(defun lse-cal:diary:process-buffer ()
  (save-excursion
    (let ((b (get-buffer-create " *Diary Process Buffer*")))
      (set-buffer b)
      (erase-buffer)
      b
    )
  )
; lse-cal:diary:process-buffer
)

;;; 12-Oct-2007
(defun lse-cal:diary:next-day (&optional n)
  (interactive "p")
  (save-match-data
    (let* ((fname    (buffer-file-name))
           (date_pat "\\([12][0-9]\\{3\\}/[01][0-9]/[0-3][0-9]\\)")
           (file_pat (concat "diary/" date_pat ".diary"))
           (dbuf     (lse-cal:diary:process-buffer))
           date new_d
         )
      (if (string-match file_pat fname)
          (progn
            (setq date (match-string 1 fname))
            (if (eq 0
                  (call-process "python"
                    nil                                           ; infile
                    dbuf                                          ; destination
                    nil                                           ; display
                    "/swing/python/Date.py"                          ; args
                    "-format=%Y/%m/%d" (format "-offset=%d" n) date  ; args
                  )
                )
                (progn
                  (save-current-buffer
                    (set-buffer dbuf)
                    (setq new_d (buffer-substring 1 (1- (point))))
                  )
                  (if (string-match date_pat new_d)
                      (lse-cal:switch-diary new_d t)
                    (message "Can't switch to %s" new_d)
                  )
                )
              (message "/swing/python/Date.py failed for date %s [%s]"
                date fname
              )
            )
          )
        (message "Not in a diary buffer: %s [%s]." fname file_pat)
      )
    )
  )
; lse-cal:diary:next-day
)

;;; 12-Oct-2007
(defun lse-cal:diary:prev-day (&optional n)
  (interactive "p")
  (lse-cal:diary:next-day (- n))
; lse-cal:diary:prev-day
)

;;; A view of half a year needs an emacs window with 29 lines
;;; View and plan combined need 115x45 for a 80 column plan and 40 week view
;;; __END__
