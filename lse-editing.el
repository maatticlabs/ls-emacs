;-*- coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work

;;;;unix_ms_filename_correspondency lse-editing:el lse_edit:el
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
;;;;    lse-editing
;;;;
;;;; Purpose
;;;;    Basic editing functions
;;;;
;;;; Revision Dates
;;;;    26-May-1994 (CT) Creation (of comment)
;;;;    26-May-1994 (CT) lse-open-line added
;;;;    26-Jun-1994 (CT) lse-align-to-word and lse-open-line: act
;;;;                     appropriately if inside a fill-in
;;;;    26-Jun-1994 (CT) lse-select-current-word added
;;;;    27-Jun-1994 (CT) lse-count-matches added
;;;;    23-Sep-1994 (CT) lse-align-to-pattern and lse-find-pattern-alignment
;;;;                     added
;;;;    17-Dec-1994 (CT) lse-indent-line-by-word and
;;;;                     lse-deindent-line-by-word added
;;;;    19-Mar-1995 (CT) lse-open-line and lse-align-to-word simplified
;;;;                     (handling of flat fill-in's removed)
;;;;    25-Mar-1995 (CT) Made lse-indent-line-by-word and
;;;;                     lse-deindent-line-by-word work properly for comment
;;;;                     lines
;;;;    25-Mar-1995 (CT) lse-current-indentation added
;;;;    29-Mar-1995 (CT) Linguistic error corrected (`column' instead of `row')
;;;;    26-Apr-1996 (CT) lse-insert-backquote-quote and friends added
;;;;     7-Oct-1996 (CT) lse-self-insert-pair removed
;;;;     5-Mar-1997 (CT) lse-show-length added
;;;;     7-Apr-1997 (CT) lse-select-current-line added
;;;;    16-Dec-1997 (CT) Set `fill-prefix' in `lse-fill-range'
;;;;    30-Dec-1997 (CT) `lse-clean-empty-*' and `lse-join-*-maybe' added
;;;;     1-Jan-1998 (CT) `lse-join-sexp-boundary-maybe' corrected
;;;;     2-Jan-1998 (CT) `lse-join-sexp-boundary-maybe' corrected
;;;;                         (consider comments; no space between ']]')
;;;;    10-Jun-1998 (CT) `lse-insert-bquotes' and `lse-remove-bquotes' added
;;;;     4-Sep-1998 (CT) `lse-insert-bars' and `lse-remove-bars' added
;;;;     1-Jan-2000 (CT) Functions `lse-*-register' added
;;;;     3-Jan-2000 (CT) `lse-insert-semicolon' changed to consider
;;;;                     begin-of-line and following `;'
;;;;     1-Feb-2001 (CT) `lse-find-word-alignment' changed to consider `"""'
;;;;     1-Mar-2001 (CT) `lse-insert+blank-maybe' changed to advance cursor
;;;;                     position if there already is a blank following the
;;;;                     cursor position
;;;;    31-Aug-2002 (CT) `negative-digit-argument` added
;;;;     1-Sep-2002 (CT) `negative-digit-argument` corrected
;;;;     1-Sep-2002 (CT) `lse-insert-num-*` added
;;;;    11-Sep-2002 (CT) `lse-previous-indentation` changed to consider
;;;;                     `lse@hanging-indent`
;;;;     4-Oct-2002 (CT) `lse-previous-indentation` changed to consider
;;;;                     text-property `hang-indent`
;;;;     9-Oct-2002 (CT) `lse-previous-indentation` changed back to start
;;;;                     with `offset` -1
;;;;    20-Mar-2003 (CT) `lse-remove-next-blank-lines` and
;;;;                     `lse-remove-prev-blank-lines` added (factored from
;;;;                     lse-flat-fill-in.el)
;;;;     4-Oct-2007 (CT) Use `lse-tpu:next-end-of-line` instead of
;;;;                     `lse-tpu:end-of-line`
;;;;     6-Oct-2007 (CT) Use `lse-tpu:search+goto+set-match` instead of
;;;;                     `lse-tpu:search-internal`
;;;;     7-Oct-2007 (CT) `lse@select-brace-range` changed to call
;;;;                     `lse-tpu:save-pos-before-search`
;;;;    29-Jul-2009 (CT) Modernize use of backquotes
;;;;    19-Jan-2011 (CT) `lse-close-line-down` and `lse-close-line-up` added
;;;;    19-Jan-2011 (CT) `lse@align-search-limit` reduced from `100` to `15`
;;;;    19-Jan-2011 (CT) `lse-find-pattern-alignment` fixed to really obey
;;;;                     `lse@align-search-limit`
;;;;    19-Jan-2011 (CT) `target-col` added to `lse-align-to-pattern`
;;;;    20-Jan-2011 (CT) `lse-line-startswith` added
;;;;    28-Jan-2011 (CT) `lse-line-endswith` added
;;;;    13-May-2011 (CT) `lse-indent-rigidly` changed to indent line above if
;;;;                     at beginning of an empty line
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-editing)

(if lse-emacs19-p
    t
  (defmacro save-match-data (&rest forms)
    `(let ((old-match-data (match-data)))
       (unwind-protect
           (progn ,@forms)
         ;; restore match-data of calling environment
         (store-match-data old-match-data)
       )
     )
  )
)

;;; 19-Jan-2011
(defun lse-line-empty-p ()
  (lse-line-startswith "[ \t]*$")
; lse-line-empty-p
)

;;; 28-Jan-2011
(defun lse-line-endswith (pat &optional len num)
  (save-excursion
    (if (not (integerp len)) (setq len (length pat)))
    (if (not (integerp num)) (setq num 0))
    (lse-tpu:previous-end-of-line num)
    (skip-chars-backward " \t")
    (while (and (bolp) (not (bobp)))
      (lse-tpu:previous-end-of-line 1)
      (skip-chars-backward " \t")
    )
    (backward-char len)
    (save-match-data
      (looking-at pat)
    )
  )
; lse-line-endswith
)

;;; 20-Jan-2011
(defun lse-line-startswith (pat)
  (save-excursion
    (beginning-of-line)
    (save-match-data
      (looking-at pat)
    )
  )
; lse-line-startswith
)

(defun lse-insert+blank-maybe (text)
  (lse-tpu:insert text)
  (if (looking-at "[^ \t]")
      (lse-tpu:insert " ")
    (lse-tpu:forward-char 1);  1-Mar-2001
  )
  (if (> (current-column) fill-column)   ;  4-Apr-1994
      (and auto-fill-function            ; 18-May-1994
           (funcall auto-fill-function)
      )
  )
; lse-insert+blank-maybe
)

(defun lse-insert-comma ()
  (interactive)
  (lse-insert+blank-maybe ",")
; lse-insert-comma
)

(defun lse-insert-semicolon ()
  (interactive)
  (cond ((looking-at ";")    (lse-tpu:insert ";"));  3-Jan-2000
        ((and (bolp) (eolp)) (lse-tpu:insert ";"));  3-Jan-2000
        (t                   (lse-insert+blank-maybe ";"))
  )
; lse-insert-semicolon
)

(defun lse-insert-braces ()
  "Insert balanced braces `{}' (around selection or cursor)"
  (interactive)
  (lse-tpu:enclose-selection "{" "}")
; lse-insert-braces
)

(defun lse-remove-braces ()
  "Remove balanced braces (around selection or cursor)"
  (interactive)
  (lse-tpu:de-enclose-selection "{" "}")
; lse-remove-braces
)

(defun lse-insert-brackets ()
  "Insert balanced brackets `[]' (around selection or cursor)"
  (interactive)
  (lse-tpu:enclose-selection "[" "]")
; lse-insert-brackets
)

(defun lse-remove-brackets ()
  "Remove balanced brackets (around selection or cursor)"
  (interactive)
  (lse-tpu:de-enclose-selection "\\[" "\\]")
; lse-remove-brackets
)

(defun lse-insert-parentheses ()
  "Insert balanced parentheses `()' (around selection or cursor)"
  (interactive)
  (lse-tpu:enclose-selection "(" ")")
; lse-insert-parentheses
)

(defun lse-remove-parentheses ()
  "Remove balanced parentheses (around selection or cursor)"
  (interactive)
  (lse-tpu:de-enclose-selection "(" ")")
; lse-remove-parentheses
)

(defun lse-insert-dquotes ()
  "Insert balanced double quotes (around selection or cursor)"
  (interactive)
  (lse-tpu:enclose-selection "\"" "\"")
; lse-insert-dquotes
)

(defun lse-remove-dquotes ()
  "Remove balanced double quotes (around selection or cursor)"
  (interactive)
  (lse-tpu:de-enclose-selection "\"" "\"")
; lse-remove-dquotes
)

(defun lse-insert-squotes ()
  "Insert balanced single quotes (around selection or cursor)"
  (interactive)
  (lse-tpu:enclose-selection "'" "'")
; lse-insert-squotes
)

(defun lse-remove-squotes ()
  "Remove balanced single quotes (around selection or cursor)"
  (interactive)
  (lse-tpu:de-enclose-selection "'" "'")
; lse-remove-squotes
)

;;; 10-Jun-1998
(defun lse-insert-bquotes ()
  "Insert balanced backquotes (around selection or cursor)"
  (interactive)
  (lse-tpu:enclose-selection "`" "`")
; lse-insert-bquotes
)

(defun lse-remove-bquotes ()
  "Remove balanced backquotes (around selection or cursor)"
  (interactive)
  (lse-tpu:de-enclose-selection "`" "`")
; lse-remove-bquotes
)

;;;; 26-Apr-1996
(defun lse-insert-backquote-quote ()
  "Insert a balanced backquote/quote pair (around selection or cursor)"
  (interactive)
  (lse-tpu:enclose-selection "`" "'")
; lse-insert-backquote-quote
)

;;;; 26-Apr-1996
(defun lse-remove-backquote-quote ()
  "Remove a balanced backquote/quote pair (around selection or cursor)"
  (interactive)
  (lse-tpu:de-enclose-selection "`" "'")
; lse-remove-backquote-quote
)

;;;; 26-Apr-1996
(defun lse-insert-double-backquote-quote ()
  "Insert a balanced double backquote/quote pair (around selection or cursor)"
  (interactive)
  (lse-tpu:enclose-selection "``" "''")
; lse-insert-double-backquote-quote
)

;;;; 26-Apr-1996
(defun lse-remove-double-backquote-quote ()
  "Remove a balanced double backquote/quote pair (around selection or cursor)"
  (interactive)
  (lse-tpu:de-enclose-selection "``" "''")
; lse-remove-double-backquote-quote
)

;;; 13-Dec-1997
(defun lse-remove-guillemots ()
  "Remove a guillemot pair"
  (interactive "*")
  (lse-tpu:de-enclose-selection "«" "»")
; lse-remove-guillemots
)

;;;  4-Sep-1998
(defun lse-insert-bars ()
  "Insert balanced bars (around selection or cursor)"
  (interactive "*")
  (lse-tpu:enclose-selection "|" "|")
; lse-insert-bars
)

;;;  4-Sep-1998
(defun lse-remove-bars ()
  "Remove balanced bars (around selection or cursor)"
  (interactive "*")
  (lse-tpu:de-enclose-selection "|" "|")
; lse-remove-bars
)

;;; 20-Jan-2000
(defun lse-insert-angles ()
  "Insert angles `<>' (around selection or cursor)"
  (interactive "*")
  (lse-tpu:enclose-selection "<" ">")
; lse-insert-angles
)

;;; 20-Jan-2000
(defun lse-remove-angles ()
  "Remove angles `<>' (around selection or cursor)"
  (interactive "*")
  (lse-tpu:de-enclose-selection "<" ">")
; lse-remove-angles
)

(defun lse-line-number (&optional p)
  (save-excursion
    (save-restriction
      (widen)
      (if (integerp p) (goto-char p))
      (beginning-of-line)
      (1+ (count-lines 1 (point)))
    )
  )
; lse-line-number
)

(defun lse-lines-in-buffer (&optional buf)
  (save-excursion
    (if buf (set-buffer (get-buffer buf)))
    (lse-line-number (point-max))
  )
; lse-lines-in-buffer
)

(defun lse-current-line-length ()
  (save-excursion
    (end-of-line)
    (1+ (current-column))
  )
; lse-current-line-length
)

(defun lse-show-position ()
  "Show position in buffer"
  (interactive)
  (message "Line %d of %d, column %d of %d, character %d of %d in buffer %s"
           (lse-line-number)     (lse-lines-in-buffer)
           (1+ (current-column)) (lse-current-line-length)
           (point)               (buffer-size) (buffer-name)
  )
; lse-show-position
)

;;;  5-Mar-1997
(defun lse-show-length ()
  "Show length of current selection or word"
  (interactive)
  (message "Length of current %s is %d"
           (if (lse-tpu:position-of-select-mark) "selection" "word")
           (if (lse-tpu:position-of-select-mark)
               (- (lse-tpu:selection-tail-pos) (lse-tpu:selection-head-pos))
             (let ((cwr (lse-tpu:current-word-range))
                  )
               (if cwr
                   (- (lse-range:tail cwr) (lse-range:head cwr) )
                 0
               )
             )
           )
  )
; lse-show-length
)

(defun lse-count-matches ()
  "Count number of occurences of regexp in buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (call-interactively 'count-matches)
  )
; lse-count-matches
)

(defun lse-untabify-buffer ()
  "Remove tabs from buffer (replaced by the appropriate number of blanks)"
  (interactive "*")
  (if (lse-tpu:position-of-select-mark)
      (untabify (lse-tpu:selection-head-pos) (lse-tpu:selection-tail-pos))
    (untabify (point-min) (point-max))
  )
; lse-untabify-buffer
)

(defun lse-untabify-line ()
  "Remove tabs from line (replaced by the appropriate number of blanks)"
  (interactive "*")
  (untabify (save-excursion (beginning-of-line) (point))
            (save-excursion (end-of-line)       (point))
  )
; lse-untabify-line
)

(defun lse-find-word-alignment (&optional dir)
  ;; "Find alignment in previous line"
  (lse-tpu:next-line-internal dir)
  (if (not (eolp))
      (cond ((looking-at "\"\"\"") (lse-tpu:forward-char 3));  1-Feb-2001
            (t                     (lse-tpu:goto-next-bs-word-head 1))
      )
  )
; lse-find-word-alignment
)

(defun lse-align-to-word (&optional dir in-fill-in)
  "Align position of next word with the position of the corresponding word of
line in direction dir"
  (interactive "*p")
  (or dir (setq dir 1))
  (let ((target-pos 0)
        (cc         (current-column))
       )
    (save-excursion
      (lse-find-word-alignment dir)
      (setq target-pos (current-column))
    )
    (indent-to target-pos)
    (if (and overwrite-mode (not in-fill-in))
        (delete-char (- target-pos cc))
    )
  )
; lse-align-to-word
)

(defun lse-align-to-next-word (&optional num)
  "Align position of next word with the position of the corresponding word of
next line"
  (interactive "*p")
  (lse-align-to-word num)
; lse-align-to-next-word
)

(defun lse-align-to-next-word-and-up (&optional num)
  (interactive "*p")
  (save-excursion
    (lse-align-to-next-word num)
  )
  (lse-tpu:next-line-internal (- 1))
; lse-align-to-next-word-and-up
)

(defun lse-align-to-previous-word (&optional num)
  "Align position of next word with the position of the corresponding word of
previous line"
  (interactive "*p")
  (lse-align-to-word -1)
; lse-align-to-previous-word
)

(defun lse-align-to-previous-word-and-down (&optional num)
  (interactive "*p")
  (let ((cp (point)))
    (lse-align-to-previous-word num)
    (goto-char cp); for some reason save-excursion does not work properly here
    (lse-tpu:next-line-internal 1)
  )
; lse-align-to-previous-word-and-down
)

(defconst lse@align-search-limit 15)

(defun lse-align-and-down ()
  (interactive "*")
  (lse-align (following-char) -1)
; lse-align-and-down
)

(defun lse-align-and-up ()
  (interactive "*")
  (lse-align (following-char) +1)
; lse-align-and-up
)

(defun lse-align (pat &optional dir)
  (if (eolp) (error "Cannot align at end-of-line"))
  (or (not (equal pat ?\ ))
      (error "Cannot align blanks")
  )
  (or (stringp pat) (setq pat (regexp-quote (char-to-string pat))))
  (or dir (setq dir +1))
  (or (looking-at pat)
      (error (concat "Current position not at `" pat "'"))
  )
  (let ((cp         (point))
        (source-pos (current-column))
        (target-pos nil)
        (distance   1)
       )
    (while (and (< distance lse@align-search-limit)
                (not target-pos)
           )
      (save-excursion
        (lse-scroll-vertically (* distance dir))
        ;;      (lse-untabify-line)
        (if (re-search-forward pat (save-excursion (end-of-line) (point)) t)
            (setq target-pos (- (current-column) (length pat)))
          (if (re-search-backward
                 pat (save-excursion (beginning-of-line) (point)) t
              )
              (setq target-pos (1- (current-column)))
          )
        )
        (setq distance (1+  distance))
      )
    )
    (cond ((not target-pos)
               (error (concat "No occurence of " pat " found nearby"))
          )
          ((> target-pos source-pos)
               (indent-to target-pos)
          )
          ((< target-pos source-pos)
               (delete-horizontal-space)
               (indent-to target-pos)
               (if (equal (preceding-char) ?\ )
                   (insert " ")
               )
          )
          (t nil)
    )
    (if target-pos
        (progn
          (goto-char cp)
          (lse-tpu:next-line-internal (- dir))
          (or (progn
                (re-search-forward pat
                                   (save-excursion (end-of-line) (point)) t
                )
                (lse-tpu:backward-char  (length pat))
              )
              (re-search-backward  pat
                   (save-excursion (beginning-of-line) (point)) t
              )
          )
        )
    )
  )
; lse-align
)

(defun lse-shift-select-mark (by)
  "Shift select-range by `by' characters."
  (interactive "NNumber of characters yo shift: ")
  (if (lse-tpu:position-of-select-mark)
      (lse-tpu:set-mark (+ (mark) by))
    (error "Select not active")
  )
)

(defun lse-blink-select-mark ()
  "Blink position of select-mark."
  (interactive)
  (save-excursion
    (if (lse-tpu:position-of-select-mark)
        (progn
          (goto-char (lse-tpu:position-of-select-mark))
          (sit-for 2)
          (lse-ring-bell)
        )
      (error "No selection active")
    )
  )
)

(defun lse@select-brace-range (starter)
  (let (head
        tail
       )
    (lse-tpu:save-pos-before-search)
    (if (lse-tpu:search+goto+set-match starter nil)
        (progn
          (setq head (lse-tpu:match-end))
          (lse-tpu:set-mark head)
          (lse-tpu:update-mode-line)
          (forward-list 1)
          (lse-tpu:forward-char -1)
          (setq tail (1- (point)))
          (lse-range:new head tail)
        )
    )
  )
; lse@select-brace-range
)

(defun lse-select-brace-range ()
  (interactive)
  (lse@select-brace-range "{")
)

(defun lse-select-paren-range ()
  (interactive)
  (lse@select-brace-range "(")
)

(defun lse-select-bracket-range ()
  (interactive)
  (lse@select-brace-range "\\[")
)

(defun lse-select-angle-range ()
  (interactive)
  (lse@select-brace-range "<")
)

(defun lse-select-guillemot-range ()
  (interactive)
  (lse@select-brace-range "«")
)

(defun lse-select-current-word (num)
  (interactive "p")
  (lse-tpu:unselect t)
  (goto-char        (lse-tpu:curr-word-head-pos))
  (lse-tpu:select   t)
  (goto-char        (lse-tpu:next-word-tail-pos num))
; lse-select-current-word
)

(defun lse-select-current-bs-word (num)
  (interactive "p")
  (let ((lse-tpu:word-chars lse-tpu:blank-sep-word-chars))
    (lse-select-current-word num)
  )
; lse-select-current-bs-word
)
;;;  7-Apr-1997
(defun lse-select-current-line (num)
  (interactive "p")
  (lse-tpu:unselect t)
  (goto-char        (lse-tpu:line-head-pos))
  (lse-tpu:select   t)
  (goto-char        (lse-tpu:line-tail-pos))
; lse-select-current-line
)

(defun lse-fill-range ()
  (interactive "*")
  (let ((fill-prefix; 15-Dec-1997
          (save-excursion
            (move-to-left-margin)
            (while (and (not (bobp)) (looking-at "^\\s-*$"))
              ;; while in empty line go up
              (lse-tpu:next-beginning-of-line 1)
            )
            (buffer-substring (point) (+ (current-indentation) (point)))
          )
        )
       ); 15-Dec-1997
    (if (lse-tpu:position-of-select-mark)
        (fill-region (lse-tpu:selection-head-pos) (lse-tpu:selection-tail-pos))
      (fill-paragraph nil)
    )
  )
  (setq deactivate-mark nil); 17-Mar-1995
; lse-fill-range
)

(defun lse-indent-rigidly (&optional amount)
  (interactive "*p")
  (if (lse-tpu:position-of-select-mark)
      (indent-rigidly
         (lse-tpu:selection-head-pos) (lse-tpu:selection-tail-pos)
         (or amount 2)
      )
    (save-excursion
      (move-to-left-margin)
      (while (and (bolp) (not (bobp)) (looking-at "^\\s-*$"))
        ;; while in empty line go up
        (lse-tpu:next-beginning-of-line 1)
      )
      (indent-rigidly
         (save-excursion (beginning-of-line 1)     (point))
         (save-excursion (end-of-line       1) (1+ (point)))
         (or amount 2)
      )
    )
  )
  (setq deactivate-mark nil); 17-Mar-1995
; lse-indent-rigidly
)

(defun lse-previous-indentation ()
  (let ((offset -1)
        (hang-indent (get-text-property (point) 'hang-indent))
       )
    (if hang-indent
        (setq offset hang-indent)
      (if lse@hanging-indent
          (setq offset lse@hanging-indent)
        (save-excursion
          (if (equal (char-syntax (following-char)) ?\) )
              ;; for closing parenthetical characters return offset of associated
              ;; opening character
              (lse-safe
                (lse-tpu:forward-char 1)
                (backward-list)
                (setq offset (current-column))
              )
          )
          (while (and (= offset -1) (not (bobp)))
            (lse-tpu:next-beginning-of-line 1)
            (setq offset (current-indentation))
            (or (> offset 0) (looking-at "[^ \t\n]") (setq offset -1))
          )
        )
      )
    )
    offset
  )
; lse-previous-indentation
)

(defun lse-indent-line ()
  (interactive "*")
  (save-excursion
    (if (not (bolp))
        (lse-tpu:next-beginning-of-line 1)
    )
    (lse-indent:remove-leading-indentation);2-Jan-98; (delete-horizontal-space)
    (insert-before-markers (make-string (lse-previous-indentation) ?\ ))
  )
; lse-indent-line
)

(setq indent-line-function 'lse-indent-line)

(defun lse-open-line ()
  (interactive "*")
  (let ((cp (point-marker)))
    (lse-split-line)
    (goto-char (marker-position cp))
    (setq cp nil)
  )
; lse-open-line
)

;;; 19-Jan-2011
(defun lse-close-line-down ()
  (interactive "*")
  (if (lse-line-empty-p)
      (let (bh bt)
        (save-excursion
          (while (lse-line-empty-p) (lse-tpu:previous-line 1))
          (lse-tpu:forward-line 1)
          (setq bh (point-marker))
        )
        (save-excursion
          (while (lse-line-empty-p) (lse-tpu:forward-line 1))
          (setq bt (point-marker))
        )
        (delete-region bh bt)
        (skip-chars-forward " \t")
      )
  )
; lse-close-line-down
)

;;; 19-Jan-2011
(defun lse-close-line-up ()
  (interactive "*")
  (lse-close-line-down)
  (lse-tpu:next-end-of-line 0)
; lse-close-line-up
)

(defun lse-tab-increment ()
  (if (integerp lse-language:tab-increment)
      lse-language:tab-increment
    2
  )
; lse-tab-increment
)

(defun lse-next-indentation ()
  (let ((offset 0)
       )
    (save-excursion
      (lse-tpu:forward-line 1)
      (while (= offset 0)
        (skip-chars-forward " \t")
        (if (eobp)
            (setq offset (lse-tab-increment))
          (if (and (bolp) (not (eolp)))      ; lse-tpu:next-line doesn't work
              (lse-tpu:next-end-of-line 1)   ;     if eol = eob
          )
          (if (eobp)
              (setq offset (lse-tab-increment))
            (if (eolp) (lse-tpu:forward-char 1))
            (setq offset (current-column))
          )
        )
      )
    )
    offset
  )
; lse-next-indentation
)

(defun lse-tabulator ()
  (interactive "*")
  (if (bolp)
      (let ((n (lse-next-indentation)))
        (insert-char ?\  n)
        (if overwrite-mode (delete-char n))
      )
    (lse-align-to-previous-word)
  )
; lse-tabulator
)

;;; 23-Sep-1994
(defun lse-find-pattern-alignment (pat &optional dir)
  (save-excursion
    (or (not (equal pat ?\ ))
        (error "Cannot align blanks")
    )
    (or (stringp pat) (setq pat (regexp-quote (char-to-string pat))))
    (or dir (setq dir +1))
    (let ((cp         (point))
          (found      nil)
          (distance   1)
         )
      (while (and (< distance lse@align-search-limit)
                  (not found)
             )
        (lse-scroll-vertically dir)
        ;;      (lse-untabify-line)
        (if (setq found
              (re-search-forward pat (save-excursion (end-of-line) (point)) t)
            )
            (goto-char (match-beginning 0))
          (if (setq found
                (re-search-backward
                   pat (save-excursion (beginning-of-line) (point)) t
                )
              )
              (goto-char (match-beginning 0))
          )
        )
        (setq distance (1+  distance))
      )
      (and found (current-column))
    )
  )
; lse-find-pattern-alignment
)

;;; 23-Sep-1994
(defun lse-align-to-pattern (pat &optional dir eat-blanks target-col)
  ;; Align position of current word with the position of a pattern of line in direction dir (default up)"
  (or dir (setq dir -1))
  (save-match-data
    (let* ((source-pos (current-column))
           (target-pos (lse-find-pattern-alignment pat dir))
           (cp         (point-marker))
           (in-fill-in (lse_inside_fill-in))
          )
      (if (not (integerp target-pos))
          (if (integerp target-col)
              (setq target-pos target-col)
            (goto-char cp)
            (error (concat "No occurence of " pat " found nearby"))
          )
      )
      (if (= target-pos source-pos)
          (let ((n (lse-next-indentation)))
            (insert-char ?\  n)
            (if (and overwrite-mode (not in-fill-in))
                (delete-char n)
            )
          )
        (if in-fill-in
            (goto-char
              (lse-range:head-pos (lse-fill-in:range lse_current_fill-in))
            )
        )
        (indent-to target-pos)
        (if (and eat-blanks (looking-at " +"))
            (delete-char (- (match-beginning 0) (match-end 0)))
        )
        (if (and overwrite-mode (not in-fill-in))
            (delete-char (- target-pos source-pos))
        )
        (if in-fill-in (goto-char (marker-position cp)))
      )
      (setq cp nil)
    )
  )
; lse-align-to-pattern
)

;;; 25-Mar-1995
(defun lse-current-indentation ()
  (let (result)
    (save-excursion
      (beginning-of-line)
      (lse-skip-whitespace+empty-comments-forward (lse-tpu:line-tail-pos))
      (setq result (current-column))
    )
    result
  )
; lse-current-indentation
)

;;; 17-Dec-1994
(defun lse-indent-line-by-word (&optional dir)
  "Align first word of line to word of line in direction `dir'"
  (interactive "*")
  (or dir (setq dir -1))
  (let ((pos (save-excursion (unless (eobp) (forward-char 1)) (point-marker))))
    (lse-tpu:next-beginning-of-line 1)
    ;; 25-Mar-1995 ;; (lse-tpu:goto-word-head 1) replaced by ...
    (lse-skip-whitespace+empty-comments-forward (lse-tpu:line-tail-pos))
    (lse-align-to-word              dir)
    (goto-char                      (1- pos))
  )
; lse-indent-line-by-word
)

(defun lse-deindent-line-by-word (&optional dir)
  "Deindent first word of line to word of line in direction `dir'"
  (interactive "*")
  (or dir (setq dir -1))
  (let* ((ci       (lse-current-indentation))
         (ti       ci)
         (distance 1)
         i
        )
    (if (> ci 0)
        (save-excursion
          (while (and (< distance lse@align-search-limit)
                      (not (< ti ci))
                 )
            (lse-scroll-vertically dir)
            (beginning-of-line)
            ;; 25-Mar-1995 ;; (lse-tpu:goto-next-word-head 1 (lse-tpu:line-tail-pos))
            (lse-skip-whitespace+empty-comments-forward (lse-tpu:line-tail-pos))
            (setq i (current-column))
            (while (and (> i 1) (< i ci))
              (setq ti i)
              (lse-tpu:goto-next-bs-word-head 1 (lse-tpu:line-tail-pos))
              (setq i (current-column))
            )
            (setq distance (1+  distance))
          )
        )
    )
    (if (< ti ci)
        (let (pos)
          (save-excursion
            (lse-tpu:forward-char 1)
            (setq pos (point-marker))
          )
          (beginning-of-line)
          ;; 25-Mar-1995 ;; (lse-tpu:goto-next-word-head 1 (lse-tpu:line-tail-pos))
          (lse-skip-whitespace+empty-comments-forward (lse-tpu:line-tail-pos))
          (lse-indent:remove-leading-indentation)
          (indent-to ti); 25-Mar-1995 ; (indent-to (1- ti))
          (goto-char (1- pos))
        )
    )
  )
; lse-deindent-line-by-word
)

;;; 30-Dec-1997
(defun lse-clean-empty-range (head-char tail-char)
  (let* (in-empty-braces
         (head
           (save-excursion
             (skip-chars-backward " \t\n")
             (setq in-empty-braces (eq (preceding-char) head-char))
             (point)
           )
         )
         (tail
           (save-excursion
             (skip-chars-forward  " \t\n")
             (setq in-empty-braces
                   (and in-empty-braces (eq (following-char) tail-char))
             )
             (point)
           )
         )
        )
    (if in-empty-braces (delete-region head tail))
  )
; lse-clean-empty-range
)

;;; 30-Dec-1997
(defun lse-clean-empty-braces ()
  "Cleans all whitespace between empty braces."
  (interactive "*")
  (lse-clean-empty-range ?\{ ?\})
; lse-clean-empty-braces
)

;;; 30-Dec-1997
(defun lse-clean-empty-brackets ()
  "Cleans all whitespace between empty brackets."
  (interactive "*")
  (lse-clean-empty-range ?\[ ?\])
; lse-clean-empty-brackets
)

;;; 30-Dec-1997
(defun lse-clean-empty-parens ()
  "Cleans all whitespace between empty parentheses."
  (interactive "*")
  (lse-clean-empty-range ?\( ?\))
; lse-clean-empty-parens
)

;;; 31-Dec-1997
(defun lse-join-sexp-boundary-maybe ()
  "Remove whitespace after begin and before end of sexp if on same line then."
  (interactive "*")
  (let* (h-head h-tail h-col dont-join lim
         (h-eol
           (save-excursion
             (lse-safe
               (backward-up-list 1) (setq lim (point))
               (end-of-line)
               (skip-chars-backward " \t")
               (setq dont-join (lse-comment:insidep lim));  2-Jan-1998
               (setq h-col (current-column)) (setq h-head (point-marker))
               (skip-chars-forward  " \t\n") (setq h-tail (point-marker))
               (end-of-line)                 (point-marker)
             )
           )
         )
         t-head t-tail t-col
         (t-eol
           (save-excursion
             (skip-chars-forward  " \t\n") (end-of-line) (point-marker)
           )
         )
         (may-join
           (save-excursion
             (lse-safe
               (backward-up-list -1)
               (lse-tpu:forward-char -1)     (setq t-tail (point-marker))
               (setq t-col
                     (- (save-excursion (end-of-line) (point)) (point)))
               (skip-chars-backward " \t\n") (setq t-head (point-marker))
               (and h-eol
                    (looking-at "[ \t]*$")
                    (<= (point) t-eol)
                    (<= (point) h-eol)
               )
             )
           )
         )
         h-txt t-txt need-hspace need-tspace
         (t-head-1 t-head)
         (h-head-1 h-head)
         (joined-cols (+ h-col t-col (- t-head h-tail)))
        )
    (if (and may-join (not dont-join) (<= joined-cols fill-column))
        (progn
          (save-excursion
            (goto-char t-head)
            (if (equal (char-syntax (preceding-char)) ?.)
                (progn
                  (insert " ")
                  (setq t-head (point-marker))
                  (setq need-tspace t)
                )
            )
          )
          (setq t-txt
                (cons (lse-range:new t-head-1 t-tail)
                      (buffer-substring-no-properties t-head t-tail)
                )
          )
          (delete-region t-head t-tail)
          (if (< h-tail t-head)
              (progn
                (save-excursion
                  (goto-char h-head)
                  (if (or need-tspace
                        (not (or  (string-match "[[{( \t]"
                                           (char-to-string (preceding-char))
                                  ); no space after opening paren or space
                                  (and (= (char-syntax (preceding-char)) ?\))
                                     (save-excursion (goto-char h-tail)
                                       (= (char-syntax (following-char)) ?\))
                                     )
                                  ); no space between two closing parens; 2-Jan-98
                             )
                        )
                      )
                      (progn
                        (insert " ")
                        (setq h-head (point-marker))
                        (setq need-hspace t)
                      )
                  )
                )
                (setq h-txt
                      (cons (lse-range:new h-head-1 h-tail)
                            (buffer-substring-no-properties h-head h-tail)
                      )
                )
                (delete-region h-head h-tail)
              )
          )
          (list h-txt t-txt need-hspace need-tspace)
        )
    )
  )
; lse-join-sexp-boundary-maybe
)

;;; 20-Mar-2003
(defun lse-remove-next-blank-lines ()
  (interactive "*")
  (let ((cp (point-marker)))
    (or (bolp) (lse-tpu:next-beginning-of-line 0))
    (if (eobp)
        nil ;  nothing to remove
      (delete-blank-lines)
    )
    (goto-char (marker-position cp))
    (setq cp nil)
  )
; lse-remove-next-blank-lines
)

;;; 20-Mar-2003
(defun lse-remove-prev-blank-lines ()
  (interactive "*")
  (let ((cp (point-marker)))
    (or (bolp) (lse-tpu:next-beginning-of-line 2))
    (if (bobp)
        nil ;  nothing to remove
      (delete-blank-lines)
    )
    (goto-char (marker-position cp))
    (setq cp nil)
  )
; lse-remove-prev-blank-lines
)

;;;; functions for putting number into Emacs registers
;;;  1-Jan-2000
(defun lse-number-to-register (register number)
  "Store a number into register"
  (interactive "cPress key naming register to put number into\nnNumber to store: ")
  (number-to-register (or number 0) register)
; lse-number-to-register
)

;;;  1-Jan-2000
(defun lse-insert-register (register)
  "Insert contents of register REGISTER into current buffer."
  (interactive "*cPress key naming register to insert")
  (insert-register register t)
; lse-insert-register
)

;;;  1-Jan-2000
(defun lse-increment-register (register inc)
  "Increment number in register REGISTER by increment INC (default = 1)."
  (interactive "*cPress key naming register to increment\np")
  (if (eq inc 0) (setq inc 1))
  (increment-register inc register)
; lse-increment-register
)

;;;  1-Sep-2002
(defun negative-digit-argument (arg)
  "negative-argument followed by digit-argument"
  (interactive "P")
  (negative-argument prefix-arg)
  (digit-argument prefix-arg)
; negative-digit-argument
)

(let ((i 0))
  (while (<= i 9)
    (eval (car
      (read-from-string
        (format
          "
(defun lse-insert-num-%d ()
  (interactive \"*\")
  (insert \"%d\")
)
          "
          i i
        )
      )
    ))
    (setq i (1+ i))
  )
)

;;;  1-Sep-2002
(defun lse-insert-num-minus ()
  (interactive "*")
  (insert "-")
; lse-insert-num-minus
)

;;;  1-Sep-2002
(defun lse-insert-num-comma ()
  (interactive "*")
  (insert ",")
; lse-insert-num-comma
)

;;;  1-Sep-2002
(defun lse-insert-num-point ()
  (interactive "*")
  (insert ".")
; lse-insert-num-point
)
