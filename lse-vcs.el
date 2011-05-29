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
;;;;    lse-vcs
;;;;
;;;; Purpose
;;;;    Provide support for version control
;;;;
;;;; Revision Dates
;;;;    29-May-2011 (CT) Creation
;;;;    ��revision-date�����
;;;;--

(provide 'lse-vcs)

(eval-when-compile
  (require 'lse-face)
  (require 'lse-fill-in)
  (require 'lse-range)
)

(defconst lse-vcs:conflict:head-pattern
  (concat
    "^"
      "<<<<<+"
      " *"
      "\\([^ \t\n]*\\)"
      " *"
    "$"
  )
)

(defconst lse-vcs:conflict:midd-pattern
  (concat
    "^"
      "=====+"
      ".*"
    "$"
  )
)

(defconst lse-vcs:conflict:tail-pattern
  (concat
    "^"
      ">>>>>+"
      " *"
      "\\([^ \t\n]*\\)"
      " *"
    "$"
  )
)

(defvar   lse-vcs:conflict:keymap (make-keymap))
(defconst lse-vcs:conflict:text-props (list 'local-map lse-vcs:conflict:keymap))

(make-variable-buffer-local (defvar lse-vcs:conflict:range:a nil))
(make-variable-buffer-local (defvar lse-vcs:conflict:range:b nil))
(make-variable-buffer-local (defvar lse-vcs:conflict:range:h nil))
(make-variable-buffer-local (defvar lse-vcs:conflict:range:m nil))
(make-variable-buffer-local (defvar lse-vcs:conflict:range:t nil))

(make-variable-buffer-local (defvar lse-vcs:conflict:overlay:a nil))
(make-variable-buffer-local (defvar lse-vcs:conflict:overlay:b nil))
(make-variable-buffer-local (defvar lse-vcs:conflict:overlay:h nil))
(make-variable-buffer-local (defvar lse-vcs:conflict:overlay:m nil))
(make-variable-buffer-local (defvar lse-vcs:conflict:overlay:t nil))

(make-variable-buffer-local (defvar lse-vcs:conflict:saved_pos nil))

(lse-face:define 'lse-vcs:conflict:face:a "Blue"   "Yellow")
(lse-face:define 'lse-vcs:conflict:face:b "Yellow" "Blue")
(lse-face:define 'lse-vcs:conflict:face:x "Grey90" "Grey10")

;;; 29-May-2011
(defun lse-vcs:conflict:add-text-properties ()
  (if lse-vcs:conflict:range:h
      (lse-fill-in:add-text-properties
        (lse-range:head-pos lse-vcs:conflict:range:h)
        (lse-range:tail-pos lse-vcs:conflict:range:h)
        lse-vcs:conflict:text-props
      )
  )
  (if lse-vcs:conflict:range:t
      (lse-fill-in:add-text-properties
        (lse-range:head-pos lse-vcs:conflict:range:t)
        (lse-range:tail-pos lse-vcs:conflict:range:t)
        lse-vcs:conflict:text-props
      )
  )
; lse-vcs:conflict:add-text-properties
)

;;; 29-May-2011
(defun lse-vcs:conflict:choose-a ()
  "Choose variant 'a' of conflict"
  (interactive)
  (lse-vcs:conflict:delete-range lse-vcs:conflict:range:b 0 +1)
  (lse-vcs:conflict:resolved)
; lse-vcs:conflict:choose-a
)

;;; 29-May-2011
(defun lse-vcs:conflict:choose-a+b ()
  "Choose variant 'a' followed by variant 'b' of conflict"
  (interactive)
  (lse-vcs:conflict:resolved)
; lse-vcs:conflict:choose-a+b
)

;;; 29-May-2011
(defun lse-vcs:conflict:choose-b ()
  "Choose variant 'b' of conflict"
  (interactive)
  (lse-vcs:conflict:delete-range lse-vcs:conflict:range:a 0 +1)
  (lse-vcs:conflict:resolved)
; lse-vcs:conflict:choose-b
)

;;; 29-May-2011
(defun lse-vcs:conflict:choose-b+a ()
  "Choose variant 'b' followed by variant 'a' of conflict"
  (interactive)
  (unless (lse-range:is-collapsed lse-vcs:conflict:range:a)
    (save-excursion
      (goto-char (lse-range:head-pos lse-vcs:conflict:range:a))
      (insert (lse-range:contents-np lse-vcs:conflict:range:b) "\n")
    )
    (lse-vcs:conflict:delete-range lse-vcs:conflict:range:b 0 +1)
  )
  (lse-vcs:conflict:resolved)
; lse-vcs:conflict:choose-b+a
)

;;; 29-May-2011
(defun lse-vcs:conflict:define-keys ()
  (define-key lse-vcs:conflict:keymap [?<]        'lse-vcs:conflict:choose-a)
  (define-key lse-vcs:conflict:keymap [?>]        'lse-vcs:conflict:choose-b)
  (define-key lse-vcs:conflict:keymap [?c]        'lse-vcs:conflict:choose-a+b)
  (define-key lse-vcs:conflict:keymap [?r]        'lse-vcs:conflict:resolved)
  (define-key lse-vcs:conflict:keymap [?q]        'lse-vcs:conflict:reset)
  (define-key lse-vcs:conflict:keymap [?s]        'lse-vcs:conflict:choose-b+a)
  (define-key lse-vcs:conflict:keymap [?\A-n]     'lse-vcs:conflict:goto-next)
  (define-key lse-vcs:conflict:keymap [?\C-n]     'lse-vcs:conflict:goto-next)
  (global-set-key                     [red ?\A-n] 'lse-vcs:conflict:goto-next)
  (global-set-key                     [red ?\C-n] 'lse-vcs:conflict:goto-next)
; lse-vcs:conflict:define-keys
)

;;; 29-May-2011
(defun lse-vcs:conflict:delete-range (range &optional hd td)
  (or
    (lse-range:is-empty      range)
    (lse-range:is-collapsed  range)
    (delete-region
      (+ (lse-range:head-pos range) (or hd 0))
      (+ (lse-range:tail-pos range) (or td 0))
    )
  )
; lse-vcs:conflict:delete-range
)

;;; 29-May-2011
(defun lse-vcs:conflict:highlight (range face)
  (let ((result
            (make-overlay (lse-range:head range) (lse-range:tail range))
          )
         )
    (overlay-put result 'face face)
    result
  )
; lse-vcs:conflict:highlight
)

;;; 29-May-2011
(defun lse-vcs:conflict:goto-next ()
  "Goto next conflict"
  (interactive)
  (if lse-vcs:conflict:range:t
      (progn
        (goto-char (lse-range:tail lse-vcs:conflict:range:t))
        (lse-vcs:conflict:remove-text-properties)
        (lse-vcs:conflict:reset-overlays)
      )
    (setq lse-vcs:conflict:saved_pos lse_last_position)
    (setq lse_last_position (point-marker))
  )
  (save-match-data
    (let (p)
      (if (setq p (re-search-forward lse-vcs:conflict:head-pattern nil t))
          (progn
            (setq lse-vcs:conflict:range:h
              (lse-range:new-x (match-beginning 0) (match-end 0))
            )
            (setq p (re-search-forward lse-vcs:conflict:midd-pattern))
            (setq lse-vcs:conflict:range:m
              (lse-range:new-x (match-beginning 0) (match-end 0))
            )
            (setq p (re-search-forward lse-vcs:conflict:tail-pattern))
            (setq lse-vcs:conflict:range:t
              (lse-range:new-x (match-beginning 0) (match-end 0))
            )
            (setq lse-vcs:conflict:range:a
              (lse-range:new-x
                (1+ (lse-range:tail-pos lse-vcs:conflict:range:h))
                (1- (lse-range:head-pos lse-vcs:conflict:range:m))
              )
            )
            (setq lse-vcs:conflict:range:b
              (lse-range:new-x
                (1+ (lse-range:tail-pos lse-vcs:conflict:range:m))
                (1- (lse-range:head-pos lse-vcs:conflict:range:t))
              )
            )
            (setq lse-vcs:conflict:overlay:a
              (lse-vcs:conflict:highlight
                lse-vcs:conflict:range:a 'lse-vcs:conflict:face:a
              )
            )
            (setq lse-vcs:conflict:overlay:b
              (lse-vcs:conflict:highlight
                lse-vcs:conflict:range:b 'lse-vcs:conflict:face:b
              )
            )
            (setq lse-vcs:conflict:overlay:h
              (lse-vcs:conflict:highlight
                lse-vcs:conflict:range:h 'lse-vcs:conflict:face:x
              )
            )
            (setq lse-vcs:conflict:overlay:m
              (lse-vcs:conflict:highlight
                lse-vcs:conflict:range:m 'lse-vcs:conflict:face:x
              )
            )
            (setq lse-vcs:conflict:overlay:t
              (lse-vcs:conflict:highlight
                lse-vcs:conflict:range:t 'lse-vcs:conflict:face:x
              )
            )
            (lse-vcs:conflict:add-text-properties)
          )
        (lse-vcs:conflict:reset)
      )
    )
  )
; lse-vcs:conflict:goto-next
)

;;; 29-May-2011
(defun lse-vcs:conflict:p ()
  (save-excursion
    (save-match-data
      (goto-char 1)
      (re-search-forward lse-vcs:conflict:head-pattern nil t)
    )
  )
; lse-vcs:conflict:p
)

;;; 29-May-2011
(defun lse-vcs:conflict:remove-text-properties ()
  (if lse-vcs:conflict:range:h
      (lse-fill-in:remove-text-properties
        (lse-range:head-pos lse-vcs:conflict:range:h)
        (lse-range:tail-pos lse-vcs:conflict:range:t)
        lse-vcs:conflict:text-props
      )

  )
; lse-vcs:conflict:remove-text-properties
)

;;; 29-May-2011
(defun lse-vcs:conflict:reset ()
  (interactive)
  (let ((last-pos (point-marker)))
    (lse-vcs:conflict:reset-overlays)
    (setq lse-vcs:conflict:range:a nil)
    (setq lse-vcs:conflict:range:b nil)
    (setq lse-vcs:conflict:range:h nil)
    (setq lse-vcs:conflict:range:m nil)
    (setq lse-vcs:conflict:range:t nil)
    (when lse-vcs:conflict:saved_pos
      (setq lse_last_position lse-vcs:conflict:saved_pos)
      (lse-goto-last-position)
    )

  )
; lse-vcs:conflict:reset
)

;;; 29-May-2011
(defun lse-vcs:conflict:reset-overlays ()
  (interactive)
  (when (and lse-vcs:conflict:overlay:a
          (overlay-buffer lse-vcs:conflict:overlay:a)
        )
    (delete-overlay lse-vcs:conflict:overlay:a)
    (delete-overlay lse-vcs:conflict:overlay:b)
    (delete-overlay lse-vcs:conflict:overlay:h)
    (delete-overlay lse-vcs:conflict:overlay:m)
    (delete-overlay lse-vcs:conflict:overlay:t)
  )
; lse-vcs:conflict:reset-overlays
)

;;; 29-May-2011
(defun lse-vcs:conflict:resolved ()
  "Resolve current conflict with the state of the buffer as is it."
  (interactive)
  (lse-vcs:conflict:delete-range lse-vcs:conflict:range:h -1)
  (lse-vcs:conflict:delete-range lse-vcs:conflict:range:m -1)
  (lse-vcs:conflict:delete-range lse-vcs:conflict:range:t 0 +1)
  (lse-vcs:conflict:reset-overlays)
; lse-vcs:conflict:resolved
)

(lse-vcs:conflict:define-keys)

;;; some text for testing
(defvar test "
<<<<<<< HEAD
My version
of the
conflict
=======
Their version
...
conflicts with mine
>>>>>>> branch

non conflicting part of the file

<<<<<<< HEAD
=======
Theirs has contents, mine doesn't
>>>>>>> branch

"
)
;;;; __END__ lse-vcs.el