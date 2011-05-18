;-*- coding: iso-8859-1; -*-

;;;;unix_ms_filename_correspondency lse-indent:el lse_indt:el
;;;; Copyright (C) 1994-2011 Mag. Christian Tanzer. All rights reserved.
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
;;;;    lse-indent
;;;;
;;;; Purpose
;;;;    Functions for controlling indentation
;;;;
;;;; Revision Dates
;;;;    26-May-1994 (CT) Creation (of comment)
;;;;    26-May-1994 (CT) lse-expand-or-tabulator added
;;;;    26-May-1994 (CT) lse-expand-or-tabulator and lse-split-line moved to
;;;;                     lse-interactive
;;;;    12-Apr-1995 (CT) lse-anchor-indent added
;;;;     9-Jun-1995 (CT) lse-reindent used instead of
;;;;                     `(lse-indent:remove-leading-indentation) (lse-indent)'
;;;;     9-Jun-1995 (CT) lse-indent:goto-indent-pos defined
;;;;     2-Jan-1998 (CT) `lse-indent:add-end-of-defun-comment' and
;;;;                     `lse-indent:format-defun*' added
;;;;     4-Jan-1998 (CT) `lse-indent:>' and `lse-indent:<' added
;;;;    11-Sep-2002 (CT) `lse@hanging-indent` and `lse-hang-indent` added
;;;;    18-Jan-2011 (CT) `lse-prev-indent` added
;;;;    20-Jan-2011 (CT) `lse-indent:level`, `lse-indent:set` and friends
;;;;                     factored
;;;;    20-Jan-2011 (CT) `lse-indent-to-pattern` added
;;;;    25-Jan-2011 (CT) `lse-indent:set:curr` added
;;;;    28-Jan-2011 (CT) `lse-newline-and-indent-to` added
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-indent)

(defvar                     lse-language:tab-increment 2
                            "Amount of indentation per indentation-level")
(make-variable-buffer-local 'lse-language:tab-increment)

;;; set-up by fill-in expansion-functions
(defvar                      lse@current-expansion-indent      nil)
(defvar                      lse@original-expansion-indent     nil)
(defvar                      lse@environment-expansion-indent  nil)
(defvar                      lse@expansion-line-leading        nil)
(defvar                      lse@expansion-line-leading-indent nil)
(defvar                      lse@expansion-line-trailer        nil)
(defvar                      lse@expansion-line-trailer-indent nil)
(defvar                      lse@hanging-indent                nil)
(make-variable-buffer-local 'lse@current-expansion-indent)
(make-variable-buffer-local 'lse@original-expansion-indent)
(make-variable-buffer-local 'lse@environment-expansion-indent)
(make-variable-buffer-local 'lse@expansion-line-leading)
(make-variable-buffer-local 'lse@expansion-line-leading-indent)
(make-variable-buffer-local 'lse@expansion-line-trailer)
(make-variable-buffer-local 'lse@expansion-line-trailer-indent)
(make-variable-buffer-local 'lse@hanging-indent)

(defun lse-indent:remove-leading-indentation ()
  ;; used instead of fixup-whitespace or delete-horizontal-space, because
  ;; these two play havoc with markers if used near the buffer end (even if
  ;; there is a comment line between the current position and the buffer end!)
  ;; bloody emacs 19.22! 26-May-1994 (this took me 2 hours painful debugging)
  (let ((cp (point)))
    (skip-chars-backward " \t" (lse-tpu:line-head-pos))
    (delete-region (point) cp)
  )
; lse-indent:remove-leading-indentation
)

;;; 20-Jan-2011
(defun lse-indent@shift (shift default)
  (if (integerp shift)
      (* shift lse-language:tab-increment)
    (if (integerp default) default lse-language:tab-increment)
  )
; lse-indent@shift
)

;;; 20-Jan-2011
(defun lse-indent:level ()
  lse@current-expansion-indent
; lse-indent:level
)

;;; 20-Jan-2011
(defun lse-indent:level:curr ()
  (save-excursion
    (lse-indent:goto-indent-pos 0)
    (if (and (bolp) (integerp lse@current-expansion-indent))
        lse@current-expansion-indent
      (current-column)
    )
  )
; lse-indent:level:curr
)

;;; 20-Jan-2011
(defun lse-indent:level:current-column (&optional delta)
  (+ (current-column) (if (integerp delta) delta 0))
; lse-indent:level:current-column
)

;;; 20-Jan-2011
(defun lse-indent:level:environment ()
  lse@environment-expansion-indent
; lse-indent:level:environment
)

;;; 20-Jan-2011
(defun lse-indent:level:expansion ()
  lse@original-expansion-indent
; lse-indent:level:expansion
)

;;; 20-Jan-2011
(defun lse-indent:level:outer-environment ()
  (max (- lse@environment-expansion-indent lse-language:tab-increment) 0)
; lse-indent:level:outer-environment
)

;;; 20-Jan-2011
(defun lse-indent:level:of-pattern (pat &optional default dir)
  (or dir (setq dir -1))
  (let ((result (lse-find-pattern-alignment pat dir)))
    (if (integerp result)
        result
      (if (integerp default)
          default
        lse@current-expansion-indent
      )
    )
  )
; lse-indent:level:of-pattern
)

;;; 20-Jan-2011
(defun lse-indent:level:prev (&optional delta)
  (save-excursion
    (lse-indent:goto-indent-pos (if (integerp delta) delta 1))
    (if (bolp)
        (if (and
              (integerp lse@current-expansion-indent)
              (> lse@current-expansion-indent 0)
            )
            (- lse@current-expansion-indent (lse-indent@shift 1 nil))
          0
        )
      (current-column)
    )
  )
; lse-indent:level:prev
)

;;; 20-Jan-2011
(defun lse-indent:set (indent)
  (setq lse@current-expansion-indent indent)
; lse-indent:set
)

;;; 20-Jan-2011
(defun lse-indent:set:curr (&optional shift)
  (lse-indent:set (+ (lse-indent:level:curr) (lse-indent@shift shift 0)))
; lse-indent:set:curr
)

;;; 20-Jan-2011
(defun lse-indent:set:prev (&optional delta)
  (lse-indent:set (lse-indent:level:prev delta))
; lse-indent:set:prev
)

;;;  4-Jan-1998
(defun lse-indent:> (&optional shift default)
  (lse-indent:set
    (+ lse@current-expansion-indent (lse-indent@shift shift default))
  )
; lse-indent:>
)

(defun lse-indent:< (&optional shift default)
  (lse-indent:set
    (- lse@current-expansion-indent (lse-indent@shift shift default))
  )
; lse-indent:<
)

;;;  9-Jun-1995
(defun lse-indent:goto-indent-pos (&optional delta)
  "Moves point to indentation-point of current line"
  (interactive)
  (if (not (bolp))
      (lse-tpu:next-beginning-of-line 1)
  )
  (if (integerp delta)
      (lse-tpu:next-beginning-of-line delta)
  )
  (lse-skip-whitespace+empty-comments-forward (lse-tpu:line-tail-pos))
; lse-indent:goto-indent-pos
)

(defun lse-indent (&optional shift)
  "Indent current line; the indentation is modified by `shift' tab-increments."
  (interactive "P")
  (cond ((integerp lse@current-expansion-indent) ; expanding a fill-in
         (lse-indent:> shift 0)
         (indent-to lse@current-expansion-indent)
        )
        (t ; not expanding a fill-in
         (setq shift (lse-indent@shift shift 0))
         (funcall indent-line-function)
         (let ((ci (current-indentation)))
           (if (< shift 0)
               (lse-indent:remove-leading-indentation)
           )
           (indent-to (+ ci shift))
         )
        )
  )
; lse-indent
)

;;; 20-Jan-2011
(defun lse-indent-to-pattern (pat &optional shift default dir)
  (lse-indent:set (lse-indent:level:of-pattern pat default dir))
  (lse-reindent)
; lse-indent-to-pattern
)

;;;  9-Jun-1995
(defun lse-reindent (&optional shift)
  (let ((op (point))
        np
       )
    ;; save excursion and restoration of position to point-marker don't work,
    ;; so we have to do it by repositioning relatively by the delta of the
    ;; old position and the indentation-point
    (lse-indent:goto-indent-pos)
    (setq np (point))
    (lse-indent:remove-leading-indentation)
    (lse-indent shift)
    (lse-tpu:forward-char (- op np))
  )
; lse-reindent
)

(defun lse-no-indent ()
  (lse-indent:set 0)
  (lse-indent:remove-leading-indentation)
)

(defun lse-anchor-indent (&optional delta)
  (lse-indent:set (lse-indent:level:current-column delta))
; lse-anchor-indent
)

;;; 11-Sep-2002
(defun lse-hang-indent (&optional delta)
  (setq lse@hanging-indent (current-column))
  (if (integerp delta)
      (setq lse@hanging-indent (+ lse@hanging-indent delta))
  )
; lse-hang-indent
)

;;; 11-Sep-2002
(defun lse-nohang-indent ()
  (setq lse@hanging-indent nil)
; lse-nohang-indent
)

(defun lse-expansion-indent ()
  (lse-indent:set (lse-indent:level:expansion))
  (lse-reindent)
)

(defun lse-environment-indent ()
  (lse-indent:set (lse-indent:level:environment))
  (lse-reindent)
)

(defun lse-outer-environment-indent ()
  (lse-indent:set (lse-indent:level:outer-environment))
  (lse-reindent)
)

(defun lse-prev-indent (&optional shift delta)
  (lse-indent:set:prev delta)
  (lse-reindent shift)
; lse-prev-indent
)

(defun lse-indent+1 ()
  (lse-indent 1)
)

(defun lse-indent-1 ()
  (lse-reindent -1)
)

(defun lse-newline (&optional arg)
  (interactive "P")
  (or arg (setq arg 1))
  (while (> arg 0)
    (setq arg (1- arg))
    (if (stringp lse@expansion-line-trailer)
        (progn
          (if lse@expansion-line-trailer-indent
              (indent-to lse@expansion-line-trailer-indent 1)
          )
          (lse-fill-in-insert lse@expansion-line-trailer)
        )
    )
    ;; (newline 1) plays havoc with markers: a marker at point is shifted
    ;; instead of staying in front of the inserted newline as expected
    (lse-fill-in-insert "\n")
    (if (stringp lse@expansion-line-leading)
        (progn
          (if lse@expansion-line-leading-indent
              (indent-to lse@expansion-line-leading-indent)
          )
          (lse-fill-in-insert lse@expansion-line-leading)
        )
    )
  )
)

(defun lse-newline-and-indent (&optional shift)
  (interactive "P")
  (lse-newline 1)
  (lse-indent  shift)
); lse-newline-and-indent

(defun lse-newline-and-indent+1 ()
  (lse-newline-and-indent +1)
); lse-newline-and-indent+1

(defun lse-newline-and-indent-1 (&optional shift)
  (lse-newline-and-indent -1)
); lse-newline-and-indent-1

;;; 28-Jan-2011
(defun lse-newline-and-indent-to (pat &optional shift default dir)
  (lse-newline)
  (lse-indent-to-pattern pat shift default dir)
; lse-newline-and-indent-to
)

;;; 19-Jan-2011
(defun lse-newline-and-indent-unless (bol-pat &optional shift)
  (unless (lse-line-startswith bol-pat)
    (lse-newline-and-indent shift)
  )
; lse-newline-and-indent-unless
)

;;;  2-Jan-1998
(defun lse-indent:add-end-of-defun-comment ()
  "Add comment with defun-name to end of current defun"
  (interactive "*")
  (save-excursion
    (beginning-of-defun)
    (let* ((lse-tpu:word-chars lse-tpu:blank-sep-word-chars)
           (d-name
             (buffer-substring-no-properties
               (lse-tpu:next-word-head-pos 1)
               (lse-tpu:next-word-tail-pos 2)
             )
           )
           (comment-pat (concat "[ \t\n]*;+ *" (regexp-quote d-name)))
           comment
          )
      (forward-list)
      (save-excursion
        (lse-tpu:next-beginning-of-line 2)
        (if (looking-at comment-pat)
            (progn
              (lse-tpu:delete-tail-of-line 1)
              (setq comment lse-tpu:line-deletion)
            )
          (setq comment (concat "; " d-name))
        )
      )
      (if (not (looking-at comment-pat))
          (insert comment)
      )
    )
  )
; lse-indent:add-end-of-defun-comment
)

;;;  2-Jan-1998
(defun lse-indent:format-defun-gnu ()
  "Format defun to be readable by GNU hackers."
  (interactive "*")
  (lse-indent:add-end-of-defun-comment)
  (save-excursion
    (beginning-of-defun)
    (let* ((start (point))
           (end   (save-excursion (forward-list) (point-marker)))
           (indent-line-function 'lisp-indent-line)
           head
           tail
          )
      (lse-safe
        (while t
          (save-excursion
            (forward-list)
            (down-list -1)
            (setq tail (point-marker))
            (if (and
                  ;; whitespace before ')'
                  (< (skip-chars-backward " \t\n") 0)
                  ;; not in comment
                  (= (point) (save-excursion (forward-comment -1) (point)))
                )
                (progn
                  (delete-region (point) tail)
                  (goto-char tail)
                  (if (and auto-fill-function
                           (> (current-column) fill-column)
                      )
                      (progn
                        (insert " ")
                        (funcall auto-fill-function)
                      )
                  )
                )
            )
          )
          (lse-tpu:forward-char 1)
          (if (re-search-forward "[ \t]*(" end t)
              (backward-up-list 1)
            (error "Finished")
          )
        )
      )
    )
  )
  (indent-sexp)
; lse-indent:format-defun-gnu
)

;;;  2-Jan-1998
(defun lse-indent:format-defun-ct ()
  "Format defun to be readable by CT."
  (interactive "*")
  (save-excursion
    (beginning-of-defun)
    (let* ((start (point))
           (end   (save-excursion (forward-list) (point-marker)))
           (indent-line-function 'lse-indent-line)
           h-eol
          )
      (lse-safe
        (while t
          (setq h-eol (save-excursion (end-of-line) (point)))
          (save-excursion
            (forward-list)
            (down-list -1)
            (if (< h-eol (point)) (newline-and-indent))
          )
          (lse-tpu:forward-char 1)
          (if (re-search-forward "[ \t]*(" end t)
              (backward-up-list 1)
            (error "Finished")
          )
        )
      )
    )
  )
  (lse-indent:add-end-of-defun-comment)
; lse-indent:format-defun-ct
)

;;;  2-Jan-1998
(defun lse-indent:format-defuns (formatter)
  (save-excursion
    (lse-tpu:move-to-beginning)
    (while (re-search-forward "^(defun " nil t)
      (funcall formatter)
    )
  )
; lse-indent:format-defuns
)

;;;  2-Jan-1998
(defun lse-indent:format-defuns-gnu ()
  "Apply `lse-indent:format-defun-gnu' to all defuns in buffer."
  (interactive "*")
  (lse-indent:format-defuns 'lse-indent:format-defun-gnu)
; lse-indent:format-defuns-gnu
)

;;;  2-Jan-1998
(defun lse-indent:format-defuns-ct ()
  "Apply `lse-indent:format-defun-ct' to all defuns in buffer."
  (interactive "*")
  (lse-indent:format-defuns 'lse-indent:format-defun-ct)
  (save-excursion
    (lse-tpu:move-to-beginning)
    (lse-tpu:replace-all ")\n\\([ \\t]*\n\\)+)" ")\n)")
  )
; lse-indent:format-defuns-ct
)
