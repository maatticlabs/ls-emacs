;-*- unibyte: t; coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work

;;;;unix_ms_filename_correspondency lse-tpu:el lse_tpu:el
;;;; Copyright (C) 1994-2007 Mag. Christian Tanzer. All rights reserved.
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
;;;;    lse-tpu
;;;;
;;;; Purpose
;;;;    Provide functions for (CT-modified) DEC TPU compatibility
;;;;
;;;; Revision Dates
;;;;    27-May-1994 (CT) Creation (of comment)
;;;;    27-May-1994 (CT) lse-tpu:insert added (honors overstrike mode)
;;;;    18-Jun-1994 (CT) tpu-edt integrated here
;;;;    26-Jun-1994 (CT) lse-tpu:curr-word-head-pos added
;;;;     2-Jul-1994 (CT) lse-tpu:selection-head-pos and
;;;;                     lse-tpu:selection-tail-pos return nil if no selection
;;;;    20-Feb-1995 (CT) lse-tpu:copy-current-line  added
;;;;    26-Feb-1995 (CT) lse-tpu:copy-current-defun added
;;;;    12-Mar-1995 (CT) lse-tpu:load-xkeys removed (was remnant from tpu-edt)
;;;;    19-Mar-1995 (CT) lse-tpu:match-overlay added
;;;;    19-Mar-1995 (CT) lse-tpu:remove-from-bol and lse-tpu:remove-from-eol
;;;;                     added
;;;;    20-Mar-1995 (CT) lse-tpu:match-overlay corrected
;;;;    15-Oct-1995 (CT) Error in lse-tpu:current-word-range corrected (tail
;;;;                     was off by one)
;;;;     1-Apr-1996 (CT) lse-face:search-match-bg added
;;;;                     lse-tpu:match-overlay-bg added
;;;;     3-Oct-1996 (CT) lse-tpu:mark-active-hook added
;;;;     7-Oct-1996 (CT) `lse-insert' replaced by `insert'
;;;;    11-Oct-1996 (CT) mode-line-format changed a bit
;;;;                     - added line-number-mode and column-number-mode
;;;;                     - removed whitespace
;;;;                     - Enclose mode-name inside «» when a LS-Emacs
;;;;                       language is used
;;;;                     - mode-line-buffer-identification set to 15b (was 17b)
;;;;                     - mode-line-modified set to %*%+ (was %*%*)
;;;;    14-Dec-1997 (CT) Adaptions to Emacs 20.2
;;;;    19-Dec-1997 (CT) `lse-tpu:delete-*-char' changed to work with Emacs 20
;;;;                         Arithmetic on (point) does not work for 8-bit
;;;;                         chars anymore. Use
;;;;                             `(save-excursion (move ...) (point))'
;;;;                         instead
;;;;    29-Dec-1997 (CT) `lse-tpu:shift-mark-hook' added
;;;;    10-Jan-1998 (CT) Define [?\A-m] instead of [?\C-m]
;;;;    13-Jan-1998 (CT) Corrected `lse-tpu:set-mark-if-shift'
;;;;    24-May-1999 (CT) `unibyte' comment added to first line of file
;;;;    24-May-1999 (CT) `lse-emacs20.3-p' used for definition of
;;;;                     `lse-tpu:word-whitespace-chars'
;;;;    19-Dec-1999 (CT) Minor changes in mode-line-format
;;;;    28-Dec-1999 (CT) Commented out the change of eol-mnemonic-unix since
;;;;                     it breaks Emacs 20.3
;;;;    28-Dec-1999 (CT) Call to `interprogram-cut-function' added to
;;;;                     `lse-tpu:copy@range'
;;;;    28-Feb-2000 (CT) Don't call `interprogram-cut-function'
;;;;     6-Jan-2002 (CT) `lse-tpu:delete-prev-bs-word` added
;;;;    25-Aug-2002 (CT) lse-tpu:delete-next-word-tail,
;;;;                     lse-tpu:delete-next-bs-word-tail,
;;;;                     lse-tpu:delete-prev-bs-word-tail,
;;;;                     lse-tpu:delete-to-prev-tail-of-line added
;;;;    29-Aug-2002 (CT) `lse-tpu:key-shifted-p` factored
;;;;    31-Aug-2002 (CT) `lse-tpu:search-again-forward` and
;;;;                     `lse-tpu:search-again-reverse` added
;;;;    31-Aug-2002 (CT) `lse-tpu:last-pos-before-search` added and used
;;;;    31-Aug-2002 (CT) `lse-tpu:word-search-forward` and friends added
;;;;     4-Sep-2002 (CT) `lse-tpu:key-shifted-p` corrected
;;;;     8-Sep-2002 (CT) `lse-tpu:page-forward` and `lse-tpu:page-backward`
;;;;                     added
;;;;     8-Sep-2002 (CT) `lse-tpu:search-again-forward` and
;;;;                     `lse-tpu:search-again-reverse` corrected (use
;;;;                     `lse-tpu:advance-direction` and
;;;;                     `lse-tpu:backup-direction`, respectively)
;;;;    14-Nov-2002 (CT) Optional argument `pat` added to
;;;;                     `lse-tpu:search-forward` and `lse-tpu:search-reverse`
;;;;    14-Nov-2002 (CT) `lse-tpu:search-again-forward` and
;;;;                     `lse-tpu:search-again-reverse` fixed to leave
;;;;                     direction alone
;;;;     2-Oct-2007 (CT) Use `move-to-column` instead of `move-to-column-force`
;;;;     2-Oct-2007 (CT) Non-nil default for `lse-tpu:word-chars` defined
;;;;                     (otherwise <C-right> doesn't work in minibuffers)
;;;;     4-Oct-2007 (CT) Direction variables and direction-dependent
;;;;                     functions removed (need no stinking modes anymore!)
;;;;     4-Oct-2007 (CT) Pre-Emacs-19 code removed
;;;;     5-Oct-2007 (CT) Replace `search` by `search-forward` and
;;;;                     `search-reverse`
;;;;     6-Oct-2007 (CT) `lse-tpu:search-again` factored and changed to deal
;;;;                     with prefix argument
;;;;     6-Oct-2007 (CT) `lse-tpu:search+goto+set-match` and
;;;;                     `lse-tpu:search+goto` factored from
;;;;                     `lse-tpu:search-internal`
;;;;     6-Oct-2007 (CT) Argument `stay-at-bob` added to
;;;;                     `lse-tpu:adjust-search` and `lse-tpu:search+goto`
;;;;     7-Oct-2007 (CT) Major surgery on replace-functions
;;;;     7-Oct-2007 (CT) Unused `lse-tpu:last-replaced-text removed`
;;;;     7-Oct-2007 (CT) `lse-tpu:last-replace-info` and
;;;;                     `lse-tpu:replace:add-info` added
;;;;     7-Oct-2007 (CT) `lse-tpu:search+goto` changed not to call
;;;;                     `lse-tpu:save-pos-before-search`
;;;;     7-Oct-2007 (CT) `lse-tpu:search-internal`, `lse-tpu:replace`, and
;;;;                     `lse-tpu:replace-all` changed to call
;;;;                     `lse-tpu:save-pos-before-search`
;;;;     7-Oct-2007 (CT) `lse-tpu:do-replace` factored
;;;;    ««revision-date»»···
;;;;--
;;; we use picture-mode functions
(require 'picture)

(load "vt-control")
;;;
;;;  Revision and Version Information
;;;
(defconst lse-tpu:version "3.9" "lse-tpu version number.")

(defvar lse-tpu:edt-mode nil
  "If non-nil, lse-tpu mode is active.")

(defconst lse-tpu:emacs19-p lse-emacs19-p
  "Non-NIL if we are running Lucid or GNU Emacs version 19.")

;;;+
;;; Variables
;;;-
(defconst lse-tpu:have-ispell t
  "*If non-nil (default), TPU-edt uses ispell for spell checking.")

(defvar lse-tpu:percent-scroll 75
  "*Percentage of the screen to scroll for next/previous screen commands.")

(defvar lse-tpu:pan-columns 16
  "*Number of columns the lse-tpu:pan functions scroll left or right."
)

(defvar lse-tpu:match-beginning-mark (make-marker))
(defvar lse-tpu:match-end-mark       (make-marker))
(defvar lse-tpu:searching-forward t
  "If non-nil, lse-tpu is searching in the forward direction."
)
(defvar lse-tpu:search-last-string ""
  "Last text searched for by the lse-tpu search commands."
)

;;; 31-Aug-2002
(defvar lse-tpu:last-pos-before-search nil
  "Position before last search command."
)

;;;  7-Oct-2007
(defvar lse-tpu:last-replace-info nil
  "List of matches/replacements done by last replace command."
)

;;;  7-Oct-2007
(defvar lse-tpu:last-replace-index 0
  "Index into`lse-tpu:last-replace-info`."
)

(defvar lse-tpu:regexp-p nil
  "If non-nil, lse-tpu uses regexp search and replace routines.")
;;; 31-Aug-2002
(defvar lse-tpu:word-search-p nil
  "If non-nil, lse-tpu uses word search and replace routines.")
(defvar lse-tpu:rectangular-p nil
  "If non-nil, lse-tpu removes and inserts rectangles.")
(defvar lse-tpu:rectangle-string nil
  "Mode line string to identify rectangular mode.")
(defvar lse-tpu:add-at-bol-hist nil
  "History variable for lse-tpu:add-at-bol function.")
(defvar lse-tpu:add-at-eol-hist nil
  "History variable for lse-tpu:add-at-eol function.")
(defvar lse-tpu:regexp-prompt-hist  nil
  "History variable for search and replace functions.")

(defvar lse-tpu:newline-and-indent-p nil
  "If non-nil, Return produces a newline and indents.")
(defvar lse-tpu:newline-and-indent-string nil
  "Mode line string to identify AutoIndent mode.")

(defvar lse-tpu:mark-flag " ")

(defvar lse-tpu:saved-delete-func nil
  "Saved value of the delete key.")

(make-variable-buffer-local 'lse-tpu:saved-delete-func)
(make-variable-buffer-local 'lse-tpu:mark-flag)
(make-variable-buffer-local 'lse-tpu:newline-and-indent-p)
(make-variable-buffer-local 'lse-tpu:newline-and-indent-string)
(make-variable-buffer-local 'lse-tpu:rectangle-string)
(make-variable-buffer-local 'lse-tpu:rectangular-p)
(make-variable-buffer-local 'lse-tpu:searching-forward)
(make-variable-buffer-local 'lse-tpu:match-beginning-mark)
(make-variable-buffer-local 'lse-tpu:match-end-mark)
(make-variable-buffer-local 'lse-tpu:last-pos-before-search)
(make-variable-buffer-local 'lse-tpu:last-replace-info);   7-Oct-2007
(make-variable-buffer-local 'lse-tpu:last-replace-index);  7-Oct-2007

(add-hook 'activate-mark-hook   'lse-tpu:update-mode-line)
(add-hook 'deactivate-mark-hook 'lse-tpu:update-mode-line)

;;;+
;;;  Match Markers -
;;;
;;;     Set in:  Search
;;;
;;;     Used in: Replace, Substitute, Store-Text, Cut/Remove,
;;;              Append, and Change-Case
;;;-
(defvar                      lse-tpu:match-overlay nil); 22-Mar-1995
(make-variable-buffer-local 'lse-tpu:match-overlay)    ; 22-Mar-1995
(defvar                      lse-tpu:match-overlay-bg nil);  1-Apr-1996
(make-variable-buffer-local 'lse-tpu:match-overlay-bg)    ;  1-Apr-1996

;;; 22-Mar-1995
(defun lse-tpu:set-match-highlight ()
  (and (marker-position lse-tpu:match-beginning-mark); 19-Mar-1995
       (marker-position lse-tpu:match-end-mark)
       (< (lse-tpu:match-beginning) (lse-tpu:match-end))
       (setq lse-tpu:match-overlay
             (make-overlay (lse-tpu:match-beginning) (lse-tpu:match-end))
       )
       (overlay-put lse-tpu:match-overlay 'face 'lse-face:search-match)
       (setq lse-tpu:match-overlay-bg
             (make-overlay (lse-tpu:line-head-pos) (lse-tpu:line-tail-pos))
       )
       (overlay-put lse-tpu:match-overlay-bg 'face 'lse-face:search-match-bg)
  )
; lse-tpu:set-match-highlight
)

;;; 22-Mar-1995
(defun lse-tpu:unset-match-highlight ()
  (if (and ; (marker-position lse-tpu:match-beginning-mark); 19-Mar-1995
           ; (marker-position lse-tpu:match-end-mark)
           ; (< lse-tpu:match-beginning-mark lse-tpu:match-end-mark)
           lse-tpu:match-overlay
      )
      (progn
        (delete-overlay lse-tpu:match-overlay)
        (delete-overlay lse-tpu:match-overlay-bg)
      )
  )
; lse-tpu:unset-match-highlight
)

(defun lse-tpu:set-match nil
  "Set markers at match beginning and end."
  ;; Add one to beginning mark so it stays with the first character of
  ;;   the string even if characters are added just before the string.
  (lse-tpu:unset-match-highlight); 22-Mar-1995 factored out
  (setq lse-tpu:match-beginning-mark (copy-marker (1+ (match-beginning 0))))
  (setq lse-tpu:match-end-mark       (copy-marker     (match-end       0)))
  (lse-tpu:set-match-highlight)
; lse-tpu:set-match
)

(defun lse-tpu:unset-match nil
  "Unset match beginning and end markers."
  (lse-tpu:unset-match-highlight); 22-Mar-1995 factored out
  (set-marker lse-tpu:match-beginning-mark nil)
  (set-marker lse-tpu:match-end-mark       nil)
; lse-tpu:unset-match
)

(defun lse-tpu:match-beginning nil
  "Returns the location of the last match beginning."
  (1- (marker-position lse-tpu:match-beginning-mark))
; lse-tpu:match-beginning
)

(defun lse-tpu:match-end nil
  "Returns the location of the last match end."
  (marker-position lse-tpu:match-end-mark)
; lse-tpu:match-end
)

(defun lse-tpu:check-match nil
  "Returns t if point is between lse-tpu:match markers.
Otherwise sets the lse-tpu:match markers to nil and returns nil."
  ;; make sure 1- marker is in this buffer
  ;;           2- point is at or after beginning marker
  ;;           3- point is before ending marker, or in the case of
  ;;              zero length regions (like bol, or eol) that the
  ;;              beginning, end, and point are equal.
  (cond ((and
          (equal (marker-buffer lse-tpu:match-beginning-mark)
                 (current-buffer)
          )
          (>= (point) (1- (marker-position lse-tpu:match-beginning-mark)))
          (or
           (< (point) (marker-position lse-tpu:match-end-mark))
           (and (= (1- (marker-position lse-tpu:match-beginning-mark))
                       (marker-position lse-tpu:match-end-mark)
                )
                (= (marker-position lse-tpu:match-end-mark)
                   (point)
                )
           )
          )
         )
         t
        )
        (t
         (lse-tpu:unset-match)
         nil
        )
  )
; lse-tpu:check-match
)

(defun lse-tpu:show-match-markers nil
  "Show the values of the match markers."
  (interactive)
  (if (markerp lse-tpu:match-beginning-mark)
      (let ((beg (marker-position lse-tpu:match-beginning-mark))
           )
        (message "(%s, %s) in %s -- current %s in %s"
                 (if beg (1- beg) nil)
                 (marker-position lse-tpu:match-end-mark)
                 (marker-buffer   lse-tpu:match-end-mark)
                 (point) (current-buffer)
        )
      )
  )
; lse-tpu:show-match-markers
)

;;;
;;;  Utilities
;;;
(defun lse-tpu:mark nil
  "lse-tpu version of the mark function.
Return the appropriate value of the mark for the current
version of emacs."
  (and mark-active (mark (not transient-mark-mode)))
; lse-tpu:mark
)

(defun lse-tpu:set-mark (pos)
  "lse-tpu verion of the set-mark function.
Sets the mark at POS and activates the region acording to the
current version of emacs."
  (set-mark pos)
; lse-tpu:set-mark
)

(defun lse-tpu:string-prompt (prompt history-symbol)
  "Read a string with PROMPT."
  (read-from-minibuffer prompt nil nil nil history-symbol)
; lse-tpu:string-prompt
)

(defvar lse-tpu:last-answer nil "Most recent response to lse-tpu:y-or-n-p.")

(defun lse-tpu:y-or-n-p (prompt &optional not-yes)
  "Prompt for a y or n answer with positive default.
Optional second argument NOT-YES changes default to negative.
Like emacs y-or-n-p, also accepts space as y and DEL as n."
  (message (format "%s[%s]" prompt (if not-yes "n" "y")))
  (let ((doit t))
    (while doit
      (setq doit nil)
      (let ((ans (read-char)))
        (cond ((or (= ans ?y) (= ans ?Y) (= ans ?\ ))
               (setq lse-tpu:last-answer t)
              )
              ((or (= ans ?n) (= ans ?N) (= ans ?\C-?))
               (setq lse-tpu:last-answer nil)
              )
              ((= ans ?\r) (setq lse-tpu:last-answer (not not-yes)))
              (t
               (setq doit t) (beep)
               (message (format "Please answer y or n.  %s[%s]"
                                prompt (if not-yes "n" "y"))
               )
              )
        )
      )
    )
  )
  lse-tpu:last-answer
; lse-tpu:y-or-n-p
)

(defun lse-tpu:invert-case-region (beg end)
  (while (> end beg)
    (funcall (if (= (downcase (char-after beg)) (char-after beg))
                 'upcase-region
               'downcase-region
             )
             beg (1+ beg)
    )
    (setq beg (1+ beg))
  )
; lse-tpu:invert-case-region
)

(defun lse-tpu:change-case (num)
  "Change the case of the character under the cursor or region.
Accepts a prefix argument of the number of characters to invert."
  (interactive "p")
  (cond ((lse-tpu:mark)
         (lse-tpu:invert-case-region (lse-tpu:selection-head-pos)
                                     (lse-tpu:selection-tail-pos)
         )
         (lse-tpu:unselect t)
        ); lse-tpu:mark
        ((lse-tpu:check-match)
         (lse-tpu:invert-case-region (lse-tpu:match-beginning)
                                     (lse-tpu:match-end)
         )
         (lse-tpu:unset-match)
        ); lse-tpu:check-match
        (t  ; neither selection nor search range are active
         (lse-tpu:invert-case-region (point) (+ (point) num))
         (goto-char (+ (point) num))
        ); t
  )
; lse-tpu:change-case
)

(defun lse-tpu:version nil
  "Print the lse-tpu version number."
  (interactive)
  (message
   "lse-tpu version %s by Christian Tanzer"
   lse-tpu:version
  )
; lse-tpu:version
)

(defun lse-tpu:toggle-newline-and-indent nil
  "Toggle between 'newline and indent' and 'simple newline'."
  (interactive)
  (cond (lse-tpu:newline-and-indent-p
         (setq lse-tpu:newline-and-indent-string "")
         (setq lse-tpu:newline-and-indent-p      nil)
         ;;  4-Jan-1998 ;; use `global-set-key' instead of `local-set-key'
         (global-set-key [?\A-m]  'newline-and-indent) ; 29-Dec-1997
         (global-set-key [return] 'newline)            ; use [return] _and_ \C-m
        )
        (t
         (setq lse-tpu:newline-and-indent-string " AI")
         (setq lse-tpu:newline-and-indent-p      t)
         ;;  4-Jan-1998 ;; use `global-set-key' instead of `local-set-key'
         (global-set-key [?\A-m]  'newline)            ; 29-Dec-1997
         (global-set-key [return] 'newline-and-indent) ; use [return] _and_ \C-m
        )
  )
  (lse-tpu:update-mode-line)
  (and (interactive-p)
       (message "The <return> key inserts a newline%s"
                (if lse-tpu:newline-and-indent-p " and indents." ".")
       )
  )
  lse-tpu:newline-and-indent-p
; lse-tpu:toggle-newline-and-indent
)

(defun lse-tpu:spell-check nil
  "Checks the spelling of the region, or of the entire buffer if no
 region is selected."
  (interactive)
  (cond (lse-tpu:have-ispell
         (if (lse-tpu:mark)
             (ispell-region (lse-tpu:selection-head-pos)
                            (lse-tpu:selection-tail-pos)
             )
           (ispell-buffer)
         )
        )
        (t
         (if (lse-tpu:mark)
             (spell-region  (lse-tpu:selection-head-pos)
                            (lse-tpu:selection-tail-pos)
             )
           (spell-buffer)
         )
        )
  )
  (if (lse-tpu:mark) (lse-tpu:unselect t))
; lse-tpu:spell-check
)

(defun lse-tpu:toggle-overwrite-mode nil
  "Switches in and out of overwrite mode"
  (interactive)
  (cond (overwrite-mode
         (local-set-key [backspace] lse-tpu:saved-delete-func)
         (overwrite-mode 0)
        )
        (t
         (setq lse-tpu:saved-delete-func (local-key-binding "\177"))
         (local-set-key [backspace] 'picture-backward-clear-column)
         (overwrite-mode 1)
        )
  )
  overwrite-mode
; lse-tpu:toggle-overwrite-mode
)

(defun lse-tpu:special-insert (num)
  "Insert a character or control code according to its ASCII decimal value."
  (interactive "*P")
  (if overwrite-mode (delete-char 1))
  (if num
      (insert num)
    (lse-message
        "Specify ASCII code of special character to insert via prefix"
    )
  )
; lse-tpu:special-insert
)

(defun lse-tpu:quoted-insert (num)
  "Read next input character and insert it. This is useful for inserting control characters."
  (interactive "*p")
  (let ((char (read-char))
       )
    (if overwrite-mode (delete-char num))
    (insert-char char num)
  )
; lse-tpu:quoted-insert
)

(defun lse-tpu:exit nil
  "Exit after saving current buffer and asking about other unsafed buffers."
  (interactive)
  (if (not (eq (recursion-depth) 0))
      (exit-recursive-edit)
    (progn (save-buffer) (save-buffers-kill-emacs))
  )
; lse-tpu:exit
)

(defun lse-tpu:quit nil
  "Quit without saving unused buffers, ask to make sure changes should be abandoned."
  (interactive)
  (let ((list (buffer-list))
        (working t)
       )
    (while (and list working)
      (let ((buffer (car list)))
        (if (and (buffer-file-name buffer) (buffer-modified-p buffer))
            (if (lse-tpu:y-or-n-p
                   "Modifications will not be saved, continue quitting? "
                )
                (kill-emacs t)
              (setq working nil)
            )
        )
        (setq list (cdr list))
      )
    )
    (if working (kill-emacs t))
  )
; lse-tpu:quit
)

(fset 'exit 'lse-tpu:exit)
(fset 'EXIT 'lse-tpu:exit)

(fset 'quit 'lse-tpu:quit)
(fset 'QUIT 'lse-tpu:quit)

(fset 'spell 'lse-tpu:spell-check)
(fset 'SPELL 'lse-tpu:spell-check)

;; Around emacs version 18.57, function line-move was renamed to
;; next-line-internal.  If we're running under an older emacs,
;; make next-line-internal equivalent to line-move.

(if (not (fboundp 'next-line-internal)) (fset 'next-line-internal 'line-move))

(defun lse-tpu:insert-formfeed nil
  "Inserts a formfeed character."
  (interactive)
  (insert "\C-L")
; lse-tpu:insert-formfeed
)


;;;+
;;; Basic functions
;;;-
(defun lse-ring-bell ()
  "Ring bell on terminal"
  (interactive)
  (ding t)
; lse-ring-bell
)

(defun lse-message (&rest text)
  (lse-ring-bell)
  (if text (apply 'message text))
; lse-message
)

(defvar lse-tpu:original-mode-line mode-line-format)
(defvar lse-tpu:original-mm-alist  minor-mode-alist)

(defun lse-tpu:set-mode-line (for-tpu)
  "Set the mode for lse-tpu, or reset it to default Emacs."
  (cond ((not for-tpu)
         (setq mode-line-format lse-tpu:original-mode-line)
         (setq minor-mode-alist lse-tpu:original-mm-alist))
        (t
         (setq-default mode-line-format
                       (list (purecopy "")
                             'mode-line-modified
                             ;; Emacs 20.n mode-line-format contains also:
                             'mode-line-mule-info              ; 18-Dec-1997
                             ;; 'mode-line-frame-identification   ; 18-Dec-1997
                             'mode-line-buffer-identification
                             (purecopy " ")
                             'global-mode-string
                             (purecopy " ")
                             'lse-tpu:mark-flag
                             (purecopy "%[(")
                             '(lse-language:name "«")
                             'mode-name
                             '(lse-language:name "»")
                             'mode-line-process 'minor-mode-alist
                             (purecopy "%n")
                             (purecopy ")%]--")
                             (purecopy '(line-number-mode "L%l--"))
                             (purecopy '(column-number-mode "C%c--"))
                             (purecopy '(-3 . "%p"))
                             (purecopy "-%-")
                       )
         )
         ;;; setting eol-mnemonic-unix trips 20.3
         ; (setq eol-mnemonic-unix "")
         ;; rectangle mode for cut&paste
         (or (assq 'lse-tpu:rectangular-p minor-mode-alist)
             (setq minor-mode-alist
                   (cons '(lse-tpu:rectangular-p lse-tpu:rectangle-string)
                         minor-mode-alist
                   )
             )
         )
         ;; newline auto-indents
         (or (assq 'lse-tpu:newline-and-indent-p minor-mode-alist)
             (setq minor-mode-alist
                   (cons '(lse-tpu:newline-and-indent-p
                           lse-tpu:newline-and-indent-string
                          )
                         minor-mode-alist
                   )
             )
         )
         ;; auto-filling (change emacs default)
         (lse-remove-from-list
             minor-mode-alist
             (assq 'auto-fill-function minor-mode-alist)
         )
         (setq minor-mode-alist
               (cons '(auto-fill-function " >>>") minor-mode-alist)
         )
         ;; key learning (change emacs default)
         (lse-remove-from-list
             minor-mode-alist
             (assq 'defining-kbd-macro minor-mode-alist)
         )
         (setq minor-mode-alist
               (cons '(defining-kbd-macro lse@key-currently-learned@info)
                     minor-mode-alist
               )
         )
         ;;
         (or (assq 'delete-selection-mode minor-mode-alist)
             (setq minor-mode-alist
               (cons
                   '(lse-tpu:delete-sel-mode-flag lse-tpu:delete-sel-mode-flag)
                    minor-mode-alist
               )
             )
         )

         ;; redefine some standard mode-line variables
         (setq-default mode-line-modified              (purecopy '("%1*%1+")))
         (setq-default mode-line-buffer-identification (purecopy '(" %15b")))
         (display-time)
        )
  )
; lse-tpu:set-mode-line
)

(defvar lse-tpu:delete-sel-mode-flag "")

(defun lse-tpu:update-mode-line ()
  "Make sure mode-line in the current buffer reflects all changes."
  ;; 18-Dec-1997 removed SELECT-indication
  ;; (setq lse-tpu:mark-flag (if (lse-tpu:mark) " SELECT " ""))
  (setq lse-tpu:delete-sel-mode-flag
        (if (and (lse-tpu:mark) delete-selection-mode) " Del-Sel")
  )
  (cond ((force-mode-line-update))
        (t (set-buffer-modified-p (buffer-modified-p)) (sit-for 0))
  )
; lse-tpu:update-mode-line
)

(defun lse-tpu:insert (&rest args)
  (let ((p (point)))
    (apply 'insert args)
    (if overwrite-mode
        (delete-region (point) (+ (point) (- (point) p)))
    )
  )
; lse-tpu:insert
)
;;;++
;;; Direction of buffer movement
;;;--
(defconst lse-tpu:direction-forward  +1)
(defconst lse-tpu:direction-backward -1)

;;;+
;;; line head and tail functions
;;;-
(defun lse-tpu:line-head-pos (&optional count)
  (save-excursion (beginning-of-line count) (point))
)

(defun lse-tpu:line-tail-pos (&optional count)
  (save-excursion (end-of-line count) (point))
)

(defun lse-tpu:line-tail-pos-sans-bs (&optional count)
  (save-excursion
    (end-of-line count)
    (skip-chars-backward " \t")
    (point)
  )
; lse-tpu:line-tail-pos-sans-bs
)

(defun lse-tpu:trim-line-end ()
  (interactive "*")
  (let ((from (lse-tpu:line-tail-pos-sans-bs))
        (to   (lse-tpu:line-tail-pos))
       )
    (if (< from to)
        (delete-region from to)
    )
  )
; lse-tpu:trim-line-end
)

(defun lse-tpu:pan-right (num)
  "Pan right lse-tpu:pan-columns (16 by default)."
  (interactive "p")
  (scroll-left (* lse-tpu:pan-columns num))
; lse-tpu:pan-right
)

(defun lse-tpu:pan-left (num)
  "Pan left lse-tpu:pan-columns (16 by default)."
  (interactive "p")
  (scroll-right (* lse-tpu:pan-columns num))
; lse-tpu:pan-left
)


;;;++
;;; selection handling
;;;--
;;;  3-Oct-1996
(defun lse-tpu:mark-active-hook ()
  "Inhibit deactivation of selection by buffer changes"
  (setq deactivate-mark nil)
; lse-tpu:mark-active-hook
)

;;; 29-Aug-2002
(defun lse-tpu:key-shifted-p (key)
  (and (vectorp key);  4-Sep-2002
       (memq 'shift (event-modifiers (aref key (1- (length key)))))
  )
; lse-tpu:key-shifted-p
)

;;; 13-Jan-1998
(defun lse-tpu:set-mark-if-shift (key)
  (if (lse-tpu:key-shifted-p key); 29-Aug-2002 factored
      (lse-tpu:select)
  )
; lse-tpu:set-mark-if-shift
)

;;; 29-Dec-1997
(defun lse-tpu:shift-mark-hook ()
  "Set mark if shift key was pressed and the command has property 'shift-mark"
  (let ((flag (and (symbolp this-command) (get this-command 'shift-mark))))
    (if (and flag (not (lse-tpu:mark)))
        (lse-tpu:set-mark-if-shift (this-command-keys))
    )
  )
; lse-tpu:shift-mark-hook
)

(defun lse-tpu:select (&optional quiet)
  "Sets the mark to define one end of a region."
  ;; 22-Nov-1993 changed by CT : do not unselect quietly
  (interactive "P")
  (cond ((lse-tpu:mark)
         (lse-message "select already active")
        )
        (t
         (lse-tpu:set-mark (point))
         (lse-tpu:update-mode-line)
         (or quiet (message "Move the text cursor to select text."))
        )
  )
  (add-hook 'post-command-hook 'lse-tpu:mark-active-hook)
; lse-tpu:select
)

(defun lse-tpu:unselect (&optional quiet)
  "Removes the mark to unselect the current region."
  (interactive "P")
  (setq mark-ring nil)
  (lse-tpu:set-mark nil)
  (lse-tpu:unset-match); 22-Mar-1995
  (lse-tpu:update-mode-line)
  (remove-hook 'post-command-hook 'lse-tpu:mark-active-hook)
  (or quiet (message "Selection canceled."))
; lse-tpu:unselect
)

(defun lse-tpu:exchange-point-and-mark ()
  "Put the mark where point is now, and point where the mark is now."
  (interactive)
  (exchange-point-and-mark)
; lse-tpu:exchange-point-and-mark
)

(defun lse-tpu:position-of-select-mark ()
  (lse-tpu:mark)
)

(defun lse-tpu:selection-head-pos ()
  (let ((smark (lse-tpu:position-of-select-mark))
       )
    (if smark
        (min smark (point))
    )
  )
; lse-tpu:selection-head-pos
)

(defun lse-tpu:selection-tail-pos ()
  (let ((smark (lse-tpu:position-of-select-mark))
       )
    (if smark
        (max smark (point))
    )
  )
; lse-tpu:selection-tail-pos
)

(defun lse-tpu:selected-range ()
  (and (lse-tpu:position-of-select-mark)
       (cons (lse-tpu:selection-head-pos) (lse-tpu:selection-tail-pos))
  )
; lse-tpu:selected-range
)

(defun lse-tpu:selection ()
  (if (not (lse-tpu:position-of-select-mark))
      nil
    (buffer-substring (lse-tpu:selection-head-pos)(lse-tpu:selection-tail-pos))
  )
; lse-tpu:selection
)

(defun lse-tpu:enclose-range (begin end leader trailer)
  ;; Enclose range between begin and end by leader and trailer.
  (save-excursion
    (goto-char begin) (insert leader)
    (goto-char end)   (insert trailer)
  )
; lse-tpu:enclose-range
)

(defun lse-tpu:enclose-selection (leader trailer)
  ;; Enclose selected range (or current point if no selection) with leader
  ;; and trailer.
  (let ((br (lse-tpu:selection-head-pos))
        (er (lse-tpu:selection-tail-pos))
       )
    (if (equal br er)
        (progn
          (insert leader)
          (save-excursion (insert trailer))
        )
      (lse-tpu:enclose-range br (1+ er) leader trailer)
      (setq deactivate-mark nil); 17-Mar-1995
    )
  )
; lse-tpu:enclose-selection
)

(defun lse-tpu:nearby@start (pos pat)
  ;; Checks if `pat' starts near position `pos'. Return start-position or nil.
  (save-excursion
    (let ((len    (chars-in-string pat))
          (i      0)
          (result nil)
         )
      (or (progn
            (goto-char pos)
            (while (and (not result) (<= i len))
              (and (looking-at pat)
                   (setq result (point))
              )
              (lse-tpu:backward-char 1)
              (setq i (1+ i))
            )
            result
          )
          (progn
            (goto-char (1+ pos))
            (and (looking-at pat)
                 (setq result (point))
            )
            result
          )
      )
    )
  )
; lse-tpu:nearby@start
)

(defun lse-tpu:de-enclose-range (begin end leader trailer)
  ;; Remove leader and trailer from boundaries of range between begin and end.
  ;; Returns amount by which end marked is affected.
  (save-excursion
    (let ((b (lse-tpu:nearby@start begin leader ))
          (e (lse-tpu:nearby@start end   trailer))
         )
      (if (and b e)
          (progn
            (goto-char b)
                (delete-region (point) (+ (chars-in-string leader)  (point)))
            (goto-char (- e (length leader)))
                (delete-region (point) (+ (chars-in-string trailer) (point)))
          )
        (error (concat "Selected range not enclosed by '"
                       leader "' and `" trailer "'"
               )
        )
      )
    )
  )
; lse-tpu:de-enclose-range
)

(defun lse-tpu:de-enclose-selection (leader trailer)
  ;; Remove leader and trailer from boundaries of selected range
  ;; (or current point if no selection).
  (let ((br (lse-tpu:selection-head-pos))
        (er (lse-tpu:selection-tail-pos  ))
       )
    (lse-tpu:de-enclose-range br er leader trailer)
    (setq deactivate-mark nil); 17-Mar-1995
  )
; lse-tpu:de-enclose-selection
)

;;;++
;;; word handling
;;;--
(defconst lse-tpu:word-whitespace-chars
  (concat
     "\000-\011"                              ; ^@ - ^I
     "\016-\040"                              ; ^N - SPACE
     (if (or lse-emacs19-p lse-emacs20.3-p)   ; lse-emacs20.3-p ; 24-May-1999
       "\177-\240"                            ; 8-bit control-chararcters
     )
  )
)
(defconst lse-tpu:blank-sep-word-chars
  (concat
     "!-~"
     (if lse-emacs19-p "\241-\376")
     ;; should really be \377 but emacs's 19.22 skip-chars-forward hangs then
     ;; (bug, oh bug)
  )
)

(defvar                      lse-tpu:ident-chars       "A-Za-z0-9")
(defvar                      lse-tpu:ident-group-chars "-_*@+")
(make-variable-buffer-local 'lse-tpu:ident-chars)
(make-variable-buffer-local 'lse-tpu:ident-group-chars)

(defvar                      lse-tpu:word-chars
  (concat lse-tpu:ident-group-chars lse-tpu:ident-chars);  2-Oct-2007
)
(make-variable-buffer-local 'lse-tpu:word-chars);  2-Oct-2006

(defun lse-tpu:set-word-char-for-idents ()
  (setq lse-tpu:word-chars
        (concat
           lse-tpu:ident-group-chars
           lse-tpu:ident-chars
        )
  )
; lse-tpu:set-word-char-for-idents
)
(or lse-tpu:word-chars (lse-tpu:set-word-char-for-idents))

(defun lse-tpu:punctuation-chars ()
  ;; everything defined neither as word-contents nor as whitespace
  (concat
     "^"
     lse-tpu:word-chars
     lse-tpu:word-whitespace-chars
  )
; lse-tpu:punctuation-chars
)

(defmacro lse-tpu:char-in-string (c s)
  (` (memq (, c)
           (mapcar (function identity) (, s))
     )
  )
; lse-tpu:char-in-string
)

(defmacro lse-tpu:remove-char-from-string  (c s)
  (` (setq s (mapconcat (function char-to-string)
                        (delq (, c)
                              (mapcar (function identity) (, s))
                        )
                        ""
             )
     )
  )
; lse-tpu:remove-char-from-string
)

(defun lse-tpu:current-word-range ()
  (let (head
        tail
        (wpat (concat "[" lse-tpu:word-chars "]"))
       )
    (save-excursion
      (if (or (looking-at (concat wpat))
              (re-search-backward wpat)
              (prog1
                  (re-search-forward wpat)
                (lse-tpu:forward-char -1)
              )
          )
          (progn
            (save-excursion
              (skip-chars-backward lse-tpu:word-chars)
              (setq head (point))
            )
            (save-excursion
              (skip-chars-forward lse-tpu:word-chars)
              (setq tail (1- (point))); 15-Oct-1995 (1- ...)
            )
          )
      )
    )
    (and head tail (lse-range:new head tail))
  )
; lse-tpu:current-word-range
)

(defun lse-tpu:change@case (command)
  ;; Change case of current word as indicated by command
  ;; (must conform to upcase-region).
  (if (lse-tpu:position-of-select-mark)
      (funcall command
               (lse-tpu:selection-head-pos) (lse-tpu:selection-tail-pos)
      )
    (let ((cwr (lse-tpu:current-word-range))
         )
      (if cwr
          (funcall command (lse-range:head cwr) (lse-range:tail cwr))
      )
    )
  )
  (setq deactivate-mark nil); 17-Mar-1995
; lse-tpu:change@case
)

(defun lse-tpu:capitalize-region@weakly (head tail)
  (let ((lse-tpu:word-chars lse-tpu:ident-chars))
    (save-excursion
      (goto-char head)
      (while (< (point) tail)
        (capitalize-region (point) (1+ (point)))
        (lse-tpu:goto-next-word-head 1)
      )
    )
  )
  (setq deactivate-mark nil); 17-Mar-1995
; lse-tpu:capitalize-region@weakly
)

(defun lse-tpu:capitalize-strongly ()
  "Capitalize selection or search-match or current-word strongly"
  (interactive "*")
  (lse-tpu:change@case 'capitalize-region)
; lse-tpu:capitalize-strongly
)

(defun lse-tpu:capitalize-weakly ()
  "Capitalize selection or search-match or current-word weakly (not touching characters inside words)"
  (interactive "*")
  (lse-tpu:change@case 'lse-tpu:capitalize-region@weakly)
; lse-tpu:capitalize-weakly
)

(defun lse-tpu:change-case-upper ()
  "Change case of selection or search-match or current-word to uppercase"
  (interactive "*")
  (lse-tpu:change@case 'upcase-region)
; lse-tpu:change-case-upper
)

(defun lse-tpu:change-case-lower ()
  "Change case of selection or search-match or current-word to lowercase"
  (interactive "*")
  (lse-tpu:change@case 'downcase-region)
; lse-tpu:change-case-lower
)

(defun lse-tpu:invert-case ()
  "Invert case of selection or search-match or current-word"
  (interactive "*")
  (lse-tpu:change@case 'lse-tpu:invert-case-region)
; lse-tpu:invert-case
)


(defun lse-tpu:curr-word-head-pos ()
  (let ((result (point)))
    (save-match-data
      (save-excursion
        (if (looking-at (concat "[" lse-tpu:word-chars "]"))
            (skip-chars-backward lse-tpu:word-chars)
          (skip-chars-backward lse-tpu:word-whitespace-chars)
          (lse-tpu:forward-char -1)
          (if (looking-at (concat "[" (lse-tpu:punctuation-chars) "]"))
                t
              (lse-tpu:forward-char 1)
              (skip-chars-backward lse-tpu:word-chars)
            )
          (setq result (point))
        )
      )
    result
    )
  )
; lse-tpu:curr-word-head-pos
)

(defun lse-tpu:next-word-head-pos (num &optional limit)
  (let ((result (point)))
    (save-match-data
      (save-excursion
        (or limit (setq limit (point-max)))
        (while (and (> num 0) (< (point) limit))
          (if (looking-at (concat "[" (lse-tpu:punctuation-chars) "]"))
              (lse-tpu:forward-char 1)
            (skip-chars-forward lse-tpu:word-chars)
          )
          (skip-chars-forward lse-tpu:word-whitespace-chars)
          (setq result (point))
          (setq num (1- num))
        )
      )
    )
    result
  )
; lse-tpu:next-word-head-pos
)

(defun lse-tpu:next-word-tail-pos (num &optional limit)
  (let ((result (point))
        at-punctuation
       )
    (save-match-data
      (save-excursion
        (or limit (setq limit (point-max)))
        (while (and (> num 0) (< (point) limit))
          (skip-chars-forward lse-tpu:word-whitespace-chars)
          (setq at-punctuation
                (looking-at (concat "[" (lse-tpu:punctuation-chars) "]"))
          )
          (if at-punctuation
              (lse-tpu:forward-char 1)
            (skip-chars-forward lse-tpu:word-chars)
          )
          (setq result (point))
          (setq num (1- num))
        )
      )
    result
    )
  )
; lse-tpu:next-word-tail-pos
)

(defun lse-tpu:prev-word-head-pos (num &optional limit)
  (let ((result (point)))
    (save-match-data
      (save-excursion
        (or limit (setq limit (point-min)))
        (while (and (> num 0) (> (point) limit))
          (skip-chars-backward lse-tpu:word-whitespace-chars)
          (lse-tpu:forward-char -1)
          (if (looking-at (concat "[" (lse-tpu:punctuation-chars) "]"))
              t
            (lse-tpu:forward-char 1)
            (skip-chars-backward lse-tpu:word-chars)
          )
          (setq result (point))
          (setq num (1- num))
        )
      )
    result
    )
  )
; lse-tpu:prev-word-head-pos
)

(defun lse-tpu:prev-word-tail-pos (num &optional limit)
  (let ((result (point)))
    (save-match-data
      (save-excursion
        (or limit (setq limit (point-min)))
        (while (and (> num 0) (> (point) limit))
          (lse-tpu:forward-char -1)
          (if (looking-at (concat "[" (lse-tpu:punctuation-chars) "]"))
              t
            (lse-tpu:forward-char 1)
            (skip-chars-backward lse-tpu:word-chars)
          )
          (skip-chars-backward lse-tpu:word-whitespace-chars)
          (setq result (point))
          (setq num (1- num))
        )
      )
    )
    result
  )
; lse-tpu:prev-word-tail-pos
)

(defun lse-tpu:goto-next-word-head (num &optional limit)
  "Goto beginning of next word."
  (interactive "p")
  (goto-char (lse-tpu:next-word-head-pos num limit))
)

(defun lse-tpu:goto-next-word-tail (num &optional limit)
  "Goto end of next word."
  (interactive "p")
  (goto-char (lse-tpu:next-word-tail-pos num limit))
)

(defun lse-tpu:goto-prev-word-head (num &optional limit)
  "Goto beginning of previous word."
  (interactive "p")
  (goto-char (lse-tpu:prev-word-head-pos num limit))
)

(defun lse-tpu:goto-prev-word-tail (num &optional limit)
  "Goto end of next word."
  (interactive "p")
  (goto-char (lse-tpu:prev-word-tail-pos num limit))
)

(defun lse-tpu:goto-next-bs-word-head (num &optional limit)
  "Goto to beginning of next word (using only blanks as separators.)"
  (interactive "p")
  (let ((lse-tpu:word-chars lse-tpu:blank-sep-word-chars))
    (lse-tpu:goto-next-word-head num limit)
  )
)

(defun lse-tpu:goto-prev-bs-word-head (num &optional limit)
  "Goto to beginning of previous word (using only blanks as separators.)"
  (interactive "p")
  (let ((lse-tpu:word-chars lse-tpu:blank-sep-word-chars))
    (lse-tpu:goto-prev-word-head num limit)
  )
)

(defun lse-tpu:goto-next-bs-word-tail (num &optional limit)
  "Goto to beginning of next word (using only blanks as separators.)"
  (interactive "p")
  (let ((lse-tpu:word-chars lse-tpu:blank-sep-word-chars))
    (lse-tpu:goto-next-word-tail num limit)
  )
)

(defun lse-tpu:goto-prev-bs-word-tail (num &optional limit)
  "Goto to beginning of previous word (using only blanks as separators.)"
  (interactive "p")
  (let ((lse-tpu:word-chars lse-tpu:blank-sep-word-chars))
    (lse-tpu:goto-prev-word-tail num limit)
  )
)

;;;++
;;; Deletion/Undeletion of chars, words, and lines
;;;--
(defun lse-tpu:undelete (num deletion dir)
  (let ((head (point))
       )
    (while (> num 0)
      (lse-tpu:insert deletion)
      (setq num (1- num))
    )
    (if (equal dir lse-tpu:direction-forward)
        (goto-char head)
    )
  )
  (setq deactivate-mark nil); 17-Mar-1995
; lse-tpu:undelete
)

(defun lse-tpu:delete (head tail)
  (delete-region head tail)
  (if overwrite-mode
      (let ((cp (point)))
        (insert-char (string-to-char " ") (- tail head))
        (goto-char cp)
      )
  )
  (setq deactivate-mark nil); 17-Mar-1995
; lse-tpu:delete
)

;;; 29-Aug-2002
(defun lse-tpu:delete-entity (head tail dir append buffer buffer-dir)
  (let ((deletion (buffer-substring head tail))
       )
    (set buffer-dir dir)
    (set buffer
      (if append
          (if (equal dir lse-tpu:direction-forward)
              (concat buffer deletion)
            (concat deletion buffer)
          )
        deletion
      )
    )
    (lse-tpu:delete head tail)
  )
; lse-tpu:delete-entity
)

;;; 29-Aug-2002
(defun lse-tpu:delete-cwl (head tail dir append buffer buffer-dir)
  (if (lse-tpu:key-shifted-p (this-command-keys))
      (lse-tpu:delete-entity head tail dir append
         'lse-tpu:pasted-region 'lse-tpu:pasted-region-dir
      )
    (lse-tpu:delete-entity head tail dir append buffer buffer-dir)
  )
; lse-tpu:delete-cwl
)

(defvar lse-tpu:char-deletion     "")
(defvar lse-tpu:char-deletion-dir lse-tpu:direction-forward)

(defun lse-tpu:delete-char (head tail dir &optional append)
  (lse-tpu:delete-cwl head tail dir append
    'lse-tpu:char-deletion 'lse-tpu:char-deletion-dir
  )
; lse-tpu:delete-char
)

(defun lse-tpu:delete-next-char (num &optional append)
  "Delete next character and store for the undelete-char command"
  (interactive "*p")
  (let ((head (point))
        ;; 19-Dec-1997; (tail (+ (point) num))
        (tail (save-excursion (lse-tpu:forward-char num) (point)))
       )
    (if (not (eobp))
        (lse-tpu:delete-char head tail lse-tpu:direction-forward append)
    )
  )
; lse-tpu:delete-next-char
)

(defun lse-tpu:delete-prev-char (num &optional append)
  "Delete previous character and store for the \\[lse-tpu:undelete-char] command"
  (interactive "*p")
  (if (bobp)
      t ; relax
    (if (equal (preceding-char) ?\t)
        (let ((cc (current-column))
             )
          (untabify (lse-tpu:line-head-pos) (lse-tpu:line-tail-pos))
          (move-to-column cc)
        )
    )
    (let ((tail (point))
          ;; 19-Dec-1997; (head (- (point) num))
          (head (save-excursion (lse-tpu:backward-char num) (point)))
         )
      (lse-tpu:delete-char head tail lse-tpu:direction-backward append)
    )
  )
; lse-tpu:delete-prev-char
)

(defun lse-tpu:delete-next-char-append (num)
  (interactive "*p")
  (lse-tpu:delete-next-char num t)
)

(defun lse-tpu:delete-prev-char-append (num)
  (interactive "*p")
  (lse-tpu:delete-prev-char num t)
)

(defun lse-tpu:undelete-char (num)
  (interactive "*p")
  (lse-tpu:undelete num lse-tpu:char-deletion lse-tpu:char-deletion-dir)
)

(defvar lse-tpu:word-deletion     "")
(defvar lse-tpu:word-deletion-dir lse-tpu:direction-forward)

(defun lse-tpu:delete-word (head tail dir &optional append)
  (if (/= head tail)
      (lse-tpu:delete-cwl head tail dir append
                         'lse-tpu:word-deletion 'lse-tpu:word-deletion-dir
      )
  )
; lse-tpu:delete-word
)

(defun lse-tpu:delete-next-word (num &optional append)
  (interactive "*p")
  (let ((head (point))
        (tail (lse-tpu:next-word-head-pos num))
       )
    (lse-tpu:delete-word head tail lse-tpu:direction-forward append)
  )
; lse-tpu:delete-next-word
)

;;; 25-Aug-2002
(defun lse-tpu:delete-next-word-tail (num &optional append)
  (interactive "*p")
  (let ((head (point))
        (tail
          (save-excursion
            (lse-tpu:goto-next-word-tail num)
            (point)
          )
        )
       )
    (lse-tpu:delete-word head tail lse-tpu:direction-forward append)
  )
; lse-tpu:delete-next-word-tail
)

;;; 25-Aug-2002
(defun lse-tpu:delete-next-bs-word-tail (num &optional append)
  (interactive "*p")
  (let ((head (point))
        (tail
          (save-excursion
            (lse-tpu:goto-next-bs-word-tail num)
            (point)
          )
        )
       )
    (lse-tpu:delete-word head tail lse-tpu:direction-forward append)
  )
; lse-tpu:delete-next-bs-word-tail
)

(defun lse-tpu:delete-prev-word (num &optional append)
  (interactive "*p")
  (let ((tail (point))
        (head (lse-tpu:prev-word-head-pos num))
       )
    (lse-tpu:delete-word head tail lse-tpu:direction-backward append)
  )
; lse-tpu:delete-prev-word
)

;;;  6-Jan-2002
(defun lse-tpu:delete-prev-bs-word (num &optional append)
  (interactive "*p")
  (let ((tail (point))
        (head
          (save-excursion
            (lse-tpu:goto-prev-bs-word-head num)
            (point)
          )
        )
       )
    (lse-tpu:delete-word head tail lse-tpu:direction-backward append)
  )
; lse-tpu:delete-prev-bs-word
)

;;; 25-Aug-2002
(defun lse-tpu:delete-prev-bs-word-tail (num &optional append)
  (interactive "*p")
  (let ((tail (point))
        (head
          (save-excursion
            (lse-tpu:goto-prev-bs-word-tail num)
            (point)
          )
        )
       )
    (lse-tpu:delete-word head tail lse-tpu:direction-backward append)
  )
; lse-tpu:delete-prev-bs-word-tail
)

(defun lse-tpu:delete-next-word-append (num)
  (interactive "*p")
  (lse-tpu:delete-next-word num t)
)

(defun lse-tpu:delete-prev-word-append (num)
  (interactive "*p")
  (lse-tpu:delete-prev-word num t)
)

(defun lse-tpu:undelete-word (num)
  (interactive "*p")
  (lse-tpu:undelete num lse-tpu:word-deletion lse-tpu:word-deletion-dir)
)

(defvar lse-tpu:line-deletion     "")
(defvar lse-tpu:line-deletion-dir lse-tpu:direction-forward)

(defun lse-tpu:delete-line (head tail dir &optional append)
  (if (/= head tail)
      (lse-tpu:delete-cwl head tail dir append
                         'lse-tpu:line-deletion 'lse-tpu:line-deletion-dir
      )
  )
; lse-tpu:delete-line
)

(defun lse-tpu:delete-next-line (num &optional append)
  (interactive "*p")
  (let ((head (point))
        (tail (lse-tpu:line-tail-pos num))
       )
    (lse-tpu:delete-line
         head (min (point-max) (1+ tail)) lse-tpu:direction-forward append
    )
  )
; lse-tpu:delete-next-line
)

(defun lse-tpu:delete-head-of-line (num &optional append)
  (interactive "*p")
  (let ((tail (point))
        (head (lse-tpu:line-head-pos (- 2 (if (bolp) (1+ num) num))))
       )
    (lse-tpu:delete-line head tail lse-tpu:direction-backward append)
  )
; lse-tpu:delete-head-of-line
)

;;; 25-Aug-2002
(defun lse-tpu:delete-to-prev-tail-of-line (num &optional append)
  (interactive "*p")
  (let ((tail (point))
        (head (1- (lse-tpu:line-head-pos (- 2 (if (bolp) (1+ num) num)))))
       )
    (lse-tpu:delete-line head tail lse-tpu:direction-backward append)
  )
; lse-tpu:delete-to-prev-tail-of-line
)

(defun lse-tpu:delete-tail-of-line (num &optional append)
  (interactive "*p")
  (let ((head (point))
        (tail (lse-tpu:line-tail-pos (if (eolp) (1+ num) num)))
       )
    (lse-tpu:delete-line head tail lse-tpu:direction-forward append)
  )
; lse-tpu:delete-tail-of-line
)

(defun lse-tpu:delete-next-line-append (num)
  (interactive "*p")
  (lse-tpu:delete-next-line num t)
)

(defun lse-tpu:delete-head-of-line-append (num)
  (interactive "*p")
  (lse-tpu:delete-head-of-line num t)
)

(defun lse-tpu:delete-tail-of-line-append (num)
  (interactive "*p")
  (lse-tpu:delete-tail-of-line num t)
)

(defun lse-tpu:undelete-line (num)
  (interactive "*p")
  (lse-tpu:undelete num lse-tpu:line-deletion lse-tpu:line-deletion-dir)
)

;;;++
;;; Selection cut/paste
;;;--
(defun lse-tpu:toggle-rectangle nil
  "Toggle rectangular mode for remove and insert."
  (interactive)
  (setq lse-tpu:rectangular-p    (not lse-tpu:rectangular-p))
  (setq lse-tpu:rectangle-string (if  lse-tpu:rectangular-p " []" ""))
  (lse-tpu:update-mode-line)
  (and (interactive-p)
       (message "Rectangular cut and paste %sabled."
                (if lse-tpu:rectangular-p "en" "dis")
       )
  )
  lse-tpu:rectangular-p
; lse-tpu:toggle-rectangle
)

(defun lse-tpu:arrange-rectangle nil
  "Adjust point and mark to mark upper left and lower right
corners of a rectangle."
  (let ((mc (current-column))
        (pc (progn (lse-tpu:exchange-point-and-mark) (current-column)))
       )
    (cond ((> (point) (lse-tpu:mark))               ; point on lower line
           (cond ((> pc mc)                         ; point @  lower-right
                  (lse-tpu:exchange-point-and-mark) ; point -> upper-left
                 )
                 (t                                 ; point @  lower-left
                  (move-to-column mc t)             ; point -> lower-right
                  (lse-tpu:exchange-point-and-mark) ; point -> upper-right
                  (move-to-column pc t)             ; point -> upper-left
                 )
           )
          )
          (t                                        ; point on upper line
           (cond ((> pc mc)                         ; point @  upper-right
                  (move-to-column mc t)             ; point -> upper-left
                  (lse-tpu:exchange-point-and-mark) ; point -> lower-left
                  (move-to-column pc t)             ; point -> lower-right
                  (lse-tpu:exchange-point-and-mark) ; point -> upper-left
                 )
           )
          )
    )
  )
; lse-tpu:arrange-rectangle
)

(defvar lse-tpu:pasted-region     "")
(defvar lse-tpu:pasted-region-dir lse-tpu:direction-forward)

(defun lse-tpu:copy@range (head tail unselect-command)
  (setq lse-tpu:pasted-region-dir
        (if (> head tail)
            lse-tpu:direction-backward
          lse-tpu:direction-forward
        )
  )
  (setq lse-tpu:pasted-region
        (if (equal lse-tpu:pasted-region-dir lse-tpu:direction-forward)
            (concat lse-tpu:pasted-region (buffer-substring head tail))
          (concat (buffer-substring head tail) lse-tpu:pasted-region)
        )
  )
  (eval unselect-command)
  ;; 28-Feb-2000;; (if interprogram-cut-function (funcall interprogram-cut-function lse-tpu:pasted-region nil))
; lse-tpu:copy@range
)

(defun lse-tpu:copy-selection (&optional last-action)
  (let ((head (lse-tpu:mark))
        (tail (point))
       )
    (lse-tpu:copy@range head tail '(lse-tpu:unselect t))
    (if last-action (funcall last-action head tail))
  )
; lse-tpu:copy-selection
)

(defun lse-tpu:copy-match (&optional last-action)
  (let (head
        tail
        (mhead (lse-tpu:match-beginning))
        (mtail (lse-tpu:match-end))
       )
    (cond ((> (point) (1- mtail))
           (setq head mtail)
           (setq tail mhead)
          )
          (t
           (setq head mhead)
           (setq tail mtail)
          )
    )
    (lse-tpu:copy@range head tail '(lse-tpu:unset-match))
    (if last-action (funcall last-action mhead mtail))
  )
; lse-tpu:copy-match
)

(defun lse-tpu:cut/copy-rectangle (cut/copy-command)
  (if (> (lse-tpu:mark) (point))
      (setq lse-tpu:pasted-region-dir lse-tpu:direction-backward)
    (setq lse-tpu:pasted-region-dir lse-tpu:direction-forward)
  )
  (lse-tpu:arrange-rectangle)
  (eval cut/copy-command)
  (lse-tpu:unselect t)
; lse-tpu:cut/copy-rectangle
)

(defun lse-tpu:copy-append-region (&optional last-action)
  "Append the selected region to the paste buffer without deleting it.
The text is saved for the paste command."
  (interactive)
  (cond ((lse-tpu:mark)
           (if lse-tpu:rectangular-p
               (error "Cannot append in rectangular mode.")
             (lse-tpu:copy-selection (or last-action 'ignore))
           )
        )
        ((lse-tpu:check-match)
           (lse-tpu:copy-match (or last-action 'ignore))
        )
        (t
           (error "No selection active.")
        )
  )
; lse-tpu:copy-append-region
)

(defun lse-tpu:cut-append-region ()
  "Delete the selected region and append it to the paste buffer.
The text is saved for the paste command."
  (interactive "*")
  (lse-tpu:copy-append-region 'lse-tpu:delete)
)

(defun lse-tpu:copy-region (&optional last-action last-rectangle-action)
  "Copy the selected region to the paste buffer without deleting it.
The text is saved for the paste command."
  (interactive)
  (cond ((lse-tpu:mark)
           (if lse-tpu:rectangular-p
               (lse-tpu:cut/copy-rectangle
                    (or last-rectangle-action
                      '(setq picture-killed-rectangle
                             (extract-rectangle (lse-tpu:selection-head-pos)
                                                (lse-tpu:selection-tail-pos)
                             )
                       )
                    )
               )
             (setq lse-tpu:pasted-region nil)
             (lse-tpu:copy-selection (or last-action 'ignore))
           )
        )
        ((lse-tpu:check-match)
           (setq lse-tpu:pasted-region nil)
           (lse-tpu:copy-match (or last-action 'ignore))
        )
        (t
           (error "No selection active.")
        )
  )
; lse-tpu:copy-region
)

(defun lse-tpu:copy-current-line (&optional append)
  "Copy current line into paste buffer.
Prefix argument means: append to paste buffer"
  (interactive "P")
  (if (not append)
      (setq lse-tpu:pasted-region nil)
  )
  (lse-tpu:copy@range (lse-tpu:line-head-pos) (lse-tpu:line-head-pos 2) nil)
; lse-tpu:copy-current-line
)

(defun lse-tpu:copy-current-defun (&optional append)
  "Copy current function-sexp into paste buffer.
Prefix argument means: append to paste buffer"
  (interactive "P")
  (let (head tail
       )
    (save-excursion
      (beginning-of-defun) (setq  head (point))
      (end-of-defun)       (setq  tail (point))
    )
    (if (not append)
        (setq lse-tpu:pasted-region nil)
    )
    (lse-tpu:copy@range head tail nil)
  )
; lse-tpu:copy-current-defun
)

(defun lse-tpu:cut-region ()
  "Delete the selected region and put it into the paste buffer.
The text is saved for the paste command."
  (interactive "*")
  (lse-tpu:copy-region
       'lse-tpu:delete
       '(picture-clear-rectangle
             (lse-tpu:selection-head-pos) (lse-tpu:selection-tail-pos)
             (not overwrite-mode)
        )
  )
; lse-tpu:cut-region
)

(defun lse-tpu:paste-region (num)
  "Insert the last region or rectangle of killed text.
With argument reinserts the text that many times."
  (interactive "p")
  (cond (lse-tpu:rectangular-p
         (let (tail)
           (while (> num 0)
             (save-excursion
               (picture-yank-rectangle (not overwrite-mode))
               (message "")
               (setq tail (point))
             )
             (setq num (1- num))
           )
           (if (equal lse-tpu:pasted-region-dir lse-tpu:direction-backward)
               (goto-char tail)
           )
         )
         (setq deactivate-mark nil); 17-Mar-1995
        )
        (t
         (lse-tpu:undelete num
                           lse-tpu:pasted-region
                           (- lse-tpu:pasted-region-dir)
         )
        )
  )
; lse-tpu:paste-region
)

(defun lse-tpu:duplicate-previous-line (num)
  (interactive "*p")
  (let (head tail delta)
    (if (and (bolp)
             ; 30-Jun-1996 ; (not (looking-at "[ \t]*$"))
        )
        (open-line 1)
    )
    (setq delta (max 0 (- (lse-tpu:line-tail-pos-sans-bs 1) (point))))
    (save-excursion
      (lse-tpu:previous-line num)
      (if (eolp)
          t; relax
        (setq head (point))
        (setq tail (- (lse-tpu:line-tail-pos 1) delta))
      )
    )
    (if (and head tail) (insert (buffer-substring head tail)))
    (setq deactivate-mark nil); 17-Mar-1995
  )
; lse-tpu:duplicate-previous-line
)

(defun lse-tpu:duplicate-word-in-previous-line (num)
  (interactive "*p")
  (let (head tail
       )
    (save-excursion
      (lse-tpu:previous-line num)
      (if (eolp)
          t
        (setq head (point))
        (setq tail (lse-tpu:next-word-tail-pos 1))
      )
    )
    (if (and head tail) (insert (buffer-substring head tail)))
    (setq deactivate-mark nil); 17-Mar-1995
  )
; lse-tpu:duplicate-word-in-previous-line
)

(defun lse-tpu:duplicate-previous-word (num)
  (interactive "*p")
  (insert (buffer-substring (lse-tpu:prev-word-head-pos num) (point)))
  (setq deactivate-mark nil); 17-Mar-1995
; lse-tpu:duplicate-previous-word
)

(defun lse-tpu:duplicate-previous-bs-word (num)
  (interactive "*p")
  (let ((lse-tpu:word-chars lse-tpu:blank-sep-word-chars))
    (lse-tpu:duplicate-previous-word num)
  )
; lse-tpu:duplicate-previous-bs-word
)

(defun lse-tpu:duplicate-previous-char (num)
  (interactive "*p")
  (insert-char (preceding-char) num)
  (setq deactivate-mark nil); 17-Mar-1995
; lse-tpu:duplicate-previous-char
)

;;;
;;;  Search
;;;
(defun lse-tpu:toggle-regexp nil
  "Switches in and out of regular expression search and replace mode."
  (interactive)
  (setq lse-tpu:regexp-p (not lse-tpu:regexp-p))
  (lse-tpu:set-search lse-tpu:searching-forward)
  (and (interactive-p)
       (message "Regular expression search and substitute %sabled."
                (if lse-tpu:regexp-p "en" "dis")
       )
  )
  lse-tpu:regexp-p
; lse-tpu:toggle-regexp
)

(defun lse-tpu:regexp-prompt (prompt)
  "Read a string, adding 'RE' to the prompt if lse-tpu:regexp-p is set."
  (let ((re-prompt
         (concat (if lse-tpu:word-search-p
                     "Word "
                   (if lse-tpu:regexp-p "RE ")
                 )
                 prompt
         )
        )
       )
    (read-from-minibuffer re-prompt nil nil nil 'lse-tpu:regexp-prompt-hist)
  )
; lse-tpu:regexp-prompt
)

(defun lse-tpu:search (dir word pat)
  (setq lse-tpu:searching-forward dir)
  (setq lse-tpu:word-search-p word)
  (lse-tpu:set-search dir)
  (lse-tpu:search-internal (or pat ""))
; lse-tpu:search
)

(defun lse-tpu:search-forward (&optional pat)
  "Search for a string or regular expression in forward direction."
  (interactive)
  (lse-tpu:search t nil pat)
; lse-tpu:search-forward
)

(defun lse-tpu:search-reverse (&optional pat)
  "Search for a string or regular expression reverse direction."
  (interactive)
  (lse-tpu:search nil nil pat)
; lse-tpu:search-reverse
)

(defun lse-tpu:word-search-forward (&optional pat)
  "Search for a word in forward direction."
  (interactive)
  (lse-tpu:search t t pat)
; lse-tpu:word-search-forward
)

(defun lse-tpu:word-search-reverse (&optional pat)
  "Search for a word in reverse direction."
  (interactive)
  (lse-tpu:search nil t pat)
; lse-tpu:word-search-reverse
)

;;;  6-Oct-2007
(defun lse-tpu:search-again (n fct)
  (let ((pat (if (numberp n)
                 (nth n lse-tpu:regexp-prompt-hist)
               lse-tpu:search-last-string
             )
        )
       )
    (funcall fct pat)
  )
; lse-tpu:search-again
)

;;; 31-Aug-2002
(defun lse-tpu:search-again-forward (n)
  "Search for the same string or regular expression as last time in forward
direction."
  (interactive "P")
  (lse-tpu:search-again n 'lse-tpu:search-forward)
; lse-tpu:search-again-forward
)

;;; 31-Aug-2002
(defun lse-tpu:search-again-reverse (n)
  "Search for the same string or regular expression as last time in reverse
direction."
  (interactive "P")
  (lse-tpu:search-again n 'lse-tpu:search-reverse)
; lse-tpu:search-again-reverse
)

;;;  6-Oct-2007
(defun lse-tpu:word-search-again-forward (n)
  "Search for the same word as last time in forward direction."
  (interactive "P")
  (lse-tpu:search-again n 'lse-tpu:word-search-forward)
; lse-tpu:word-search-again-forward
)

;;;  6-Oct-2007
(defun lse-tpu:word-search-again-reverse (n)
  "Search for the same word as last time in reverse direction."
  (interactive "P")
  (lse-tpu:search-again n 'lse-tpu:word-search-reverse)
; lse-tpu:word-search-again-reverse
)

;;  lse-tpu:set-search defines the search functions used by the lse-tpu internal
;;  search function.  It should be called whenever the direction changes, or
;;  the regular expression mode is turned on or off.  It can also be called
;;  to ensure that the next search will be in the current direction.  It is
;;  called from:

;;       lse-tpu:toggle-regexp        lse-tpu:replace
;;       lse-tpu:search-forward (t)   lse-tpu:search-reverse (t)

(defun lse-tpu:set-search (dir)
  "Set the search functions."
  (cond (dir
         (cond (lse-tpu:word-search-p ; 31-Aug-2002
                (fset 'lse-tpu:emacs-search     'word-search-forward)
                (fset 'lse-tpu:emacs-rev-search 'word-search-backward)
               )
               (lse-tpu:regexp-p
                (fset 'lse-tpu:emacs-search     're-search-forward)
                (fset 'lse-tpu:emacs-rev-search 're-search-backward)
               )
               (t
                (fset 'lse-tpu:emacs-search     'search-forward)
                (fset 'lse-tpu:emacs-rev-search 'search-backward)
               )
         )
        )
        (t
         (cond (lse-tpu:word-search-p ; 31-Aug-2002
                (fset 'lse-tpu:emacs-search     'word-search-backward)
                (fset 'lse-tpu:emacs-rev-search 'word-search-forward)
               )
               (lse-tpu:regexp-p
                (fset 'lse-tpu:emacs-search     're-search-backward)
                (fset 'lse-tpu:emacs-rev-search 're-search-forward)
               )
               (t
                (fset 'lse-tpu:emacs-search     'search-backward)
                (fset 'lse-tpu:emacs-rev-search 'search-forward)
               )
         )
        )
  )
; lse-tpu:set-search
)

;;;  6-Oct-2007
(defun lse-tpu:search+goto (pat &optional limit stay-at-bob)
  (lse-tpu:unset-match)
  (lse-tpu:adjust-search nil stay-at-bob)
  (if (lse-tpu:emacs-search pat limit t)
      (progn
        (goto-char (match-beginning 0))
        t
      )
  )
; lse-tpu:search+goto
)

;;;  6-Oct-2007
(defun lse-tpu:search+goto+set-match (pat &optional limit stay-at-bob)
  (if (lse-tpu:search+goto pat limit stay-at-bob)
      (progn
        (lse-tpu:set-match)
        t
      )
  )
; lse-tpu:search+goto+set-match
)

(defun lse-tpu:search-internal
    (pat &optional quiet limit dont-look-other-dir stay-at-bob)
  ;; Search for a string or regular expression.
  ;; 12-Dec-1993 CT : do not prompt if found in reverse direction
  (setq lse-tpu:search-last-string
        (if (not (string= "" pat))
            pat
          (lse-tpu:regexp-prompt
            (format "Search %s : " (if lse-tpu:searching-forward "vvv" "^^^"))
          )
        )
  )
  (lse-tpu:save-pos-before-search)
  (cond ((lse-tpu:search+goto+set-match
           lse-tpu:search-last-string limit stay-at-bob
         )
           t
        )
        ((not dont-look-other-dir)
           (lse-tpu:adjust-search t)
           (let ((found nil))
             (save-excursion
               (let ((lse-tpu:searching-forward (not lse-tpu:searching-forward)))
                 (lse-tpu:adjust-search stay-at-bob)
                 (setq found
                   (lse-tpu:emacs-rev-search lse-tpu:search-last-string nil t)
                 )
               )
             )
             (if (not quiet)
                 (if found
                     (lse-message "Found in %s direction. "
                              (if lse-tpu:searching-forward "reverse" "forward")
                     )
                   (lse-message "%sSearch failed: \"%s\""
                     (if lse-tpu:regexp-p "RE " "") lse-tpu:search-last-string
                   )
                   (lse-tpu:unset-match)
                   nil
                 )
             )
           )
           nil
        )
        (t (lse-tpu:unset-match)
           (or quiet
               (lse-message "%sSearch failed: \"%s\""
                  (if lse-tpu:regexp-p "RE " "") lse-tpu:search-last-string
               )
           )
           nil
        )
  )
; lse-tpu:search-internal
)

(defun lse-tpu:adjust-search (&optional arg stay-at-bob)
  "For forward searches, move forward a character before searching,
and backward a character after a failed search.  Arg means end of search."
  (if lse-tpu:searching-forward
      (cond (arg  (if (not (bobp)) (lse-tpu:forward-char -1)))
            ((not (and (bobp) stay-at-bob))
             (if  (not (eobp)) (lse-tpu:forward-char  1))
            )
      )
  )
; lse-tpu:adjust-search
)

;;; 31-Aug-2002
(defun lse-tpu:save-pos-before-search ()
  (or (looking-at lse-tpu:search-last-string)
      (setq lse-tpu:last-pos-before-search (point-marker))
  )
; lse-tpu:save-pos-before-search
)

;;; 31-Aug-2002
(defun lse-tpu:goto-pos-before-search ()
  "Move point to position before last search."
  (interactive)
  (and lse-tpu:last-pos-before-search
       (goto-char lse-tpu:last-pos-before-search)
  )
; lse-tpu:goto-pos-before-search
)

;;;  7-Oct-2007
(defun lse-tpu:replace:reset-info ()
  (setq lse-tpu:last-replace-info  nil)
  (setq lse-tpu:last-replace-index 0)
; lse-tpu:replace:reset-info
)

;;;  7-Oct-2007
(defun lse-tpu:replace:add-info (&optional not-replaced)
  (lse-add-to-list lse-tpu:last-replace-info
    (vector
      (copy-marker (match-beginning 0))
      (copy-marker (match-end       0))
      (if not-replaced nil (match-string-no-properties 0))
    )
  )
; lse-tpu:replace:add-info
)

;;;  7-Oct-2007
(defun lse-tpu:replace@goto (n op)
  (let* ((index (if (numberp n) n lse-tpu:last-replace-index))
         (info  (nth index lse-tpu:last-replace-info))
        )
    (if info
        (let* ((head (aref info 0))
               (tail (aref info 1))
               (repl (aref info 2))
               (lse-tpu:match-beginning-mark (1+ head))
               (lse-tpu:match-end-mark tail)
              )
          (setq lse-tpu:last-replace-index (funcall op index))
          (if (< lse-tpu:last-replace-index 0)
              (setq lse-tpu:last-replace-index 0)
          )
          (lse-tpu:unset-match-highlight)
          (lse-tpu:set-match-highlight)
          (goto-char head)
          (if repl
              (message "Value replaced: %s" repl)
            (message "No replacement done")
          )
        )
      (message "No more replacement instance.")
    )
  )
; lse-tpu:replace:goto-next
)

;;;  7-Oct-2007
(defun lse-tpu:replace:goto-next (&optional n)
  "Goto next instance of last replacement set."
  (interactive "P")
  (lse-tpu:replace@goto n '1+)
; lse-tpu:replace:goto-next
)

;;;  7-Oct-2007
(defun lse-tpu:replace:goto-prev (&optional n)
  "Goto previous instance of last replacement set."
  (interactive "P")
  (lse-tpu:replace@goto n '1-)
; lse-tpu:replace:goto-prev
)

(defun lse-tpu:replace-one (to)
  (let ((beg (point)))
    (lse-tpu:unset-match-highlight); 22-Mar-1995
    (lse-tpu:replace:add-info);  7-Oct-2007
    (replace-match to (not case-replace) (not lse-tpu:regexp-p))
    (if lse-tpu:searching-forward
        (lse-tpu:forward-char -1)
      (goto-char beg)
    )
  )
; lse-tpu:replace-one
)

(defun lse-tpu:replace-rest (from to &optional tail-limit)
  (let ((go-on t)
        (repl-number 0)
       )
    (save-excursion
      (while go-on
        (lse-tpu:replace-one to)
        (setq repl-number (1+ repl-number))
        (setq go-on (lse-tpu:search+goto+set-match from tail-limit))
      )
    )
    repl-number
  )
; lse-tpu:replace-rest
)

(defun lse-tpu:replace@all (from to head-limit tail-limit)
  (save-excursion
    (goto-char head-limit)
    (if (not (looking-at (if lse-tpu:regexp-p from (regexp-quote from))))
        (lse-tpu:search+goto+set-match from tail-limit)
    )
    (lse-tpu:replace-rest from to tail-limit)
  )
; lse-tpu:replace@all
)

;;;  7-Oct-2007
(defun lse-tpu:do-replace (from to head-limit tail-limit replace-fct)
  (let (repl-number)
    (lse-tpu:replace:reset-info)
    (lse-tpu:save-pos-before-search)
    (unless (and head-limit tail-limit)
        (setq head-limit (lse-tpu:selection-head-pos))
        (setq tail-limit (lse-tpu:selection-tail-pos))
        (if (and tail-limit (<= tail-limit (point)))
            (lse-tpu:exchange-point-and-mark)
        )
    )
    (let* ((lse-tpu:searching-forward t)
           (start (or head-limit (point-min)))
           (end   (or tail-limit (point-max)))
           (head  (min start end))
           (tail  (max start end))
          )
      (setq repl-number (funcall replace-fct from to head tail))
      (unless (boundp 'lse-tpu:quiet-replace)
        (message "Replaced %s occurrence%s."
          repl-number (if (not (= 1 repl-number)) "s" "")
        )
      )
    )
    (setq deactivate-mark nil)
    repl-number
  )
; lse-tpu:do-replace
)

(defun lse-tpu:replace-all (from to &optional head-limit tail-limit)
  "Replace all occurences of `from` by `to`."
  (interactive (list (lse-tpu:regexp-prompt "replace all: ")
                     (lse-tpu:regexp-prompt "by: ")
               )
  )
  (lse-tpu:do-replace from to head-limit tail-limit 'lse-tpu:replace@all)
; lse-tpu:replace-all
)

(defun lse-tpu:replace (from to &optional head-limit tail-limit)
  "Interactively replace occurrences of `from` by `to`."
  (interactive (list (lse-tpu:regexp-prompt "replace: ")
                     (lse-tpu:regexp-prompt "by: ")
               )
  )
  (lse-tpu:do-replace from to head-limit tail-limit 'lse-tpu:replace-internal)
; lse-tpu:replace
)

(defvar lse-tpu:quit-search nil); 14-Dec-1997

(defun lse-tpu:replace-internal (from to &optional head-limit tail-limit)
  (if (string= "" from) (error "No string to replace."))
  (let* ((repl-number 0)
         (cp (point))
         lse-tpu:quit-search
       )
    (if (< cp head-limit) (goto-char head-limit))
    (setq repl-number (lse-tpu:replace-loop from to tail-limit))
    (or lse-tpu:quit-search
        (<     cp head-limit)
        (equal cp head-limit)
        (let ((found nil)
              (match-pos nil)
             )
          (save-excursion
            (goto-char head-limit)
            (lse-tpu:adjust-search nil t)
            (setq found (lse-tpu:search+goto+set-match from cp))
            (setq match-pos (match-beginning 0))
          )
          (if found
              (progn
                (goto-char match-pos)
                (setq repl-number
                   (+ repl-number (lse-tpu:replace-loop from to cp))
                )
              )
          )
        )
    )
    repl-number
  )
; lse-tpu:replace-internal
)

;;; must not be called by any routine other than lse-tpu:replace
(defun lse-tpu:replace-loop (from to &optional tail-limit)
  (let ((go-on t)
        (repl-number 0)
       )
    (lse-tpu:set-search lse-tpu:searching-forward)
    (if (not (looking-at (if lse-tpu:regexp-p from (regexp-quote from))))
        (setq go-on (lse-tpu:search+goto+set-match from tail-limit))
    )
    (while go-on
      (setq go-on nil)
      (let ((ans (read-char "Replace? Type Yes, No, All, Last, or Quit: ")))
        (cond ((or (= ans ?y) (= ans ?Y) (= ans ?\r) (= ans ?\ ))
               (lse-tpu:replace-one to)
               (setq repl-number (1+ repl-number))
               (setq go-on (lse-tpu:search+goto+set-match from tail-limit))
              )

              ((or (= ans ?n) (= ans ?N) (= ans ?\C-?))
               (lse-tpu:replace:add-info t);  7-Oct-2007
               (setq go-on (lse-tpu:search+goto+set-match from tail-limit))
              )

              ((or (= ans ?a) (= ans ?A))
               (setq repl-number
                 (+ repl-number (lse-tpu:replace-rest from to tail-limit))
               )
              )

              ((or (= ans ?l) (= ans ?L))
               (lse-tpu:replace-one to)
               (setq repl-number (1+ repl-number))
               (setq lse-tpu:quit-search t)
              )

              ((or (= ans ?q) (= ans ?Q))
               (setq lse-tpu:quit-search t)
              )
        )
      )
    )
    repl-number
  )
; lse-tpu:replace-loop
)

(defun lse-tpu:add-at-bol (text)
  "Add text to the beginning of each line in a region,
or to the current line if no region is selected."
  (interactive
     (list (lse-tpu:string-prompt "String to add: " 'lse-tpu:add-at-bol-hist))
  )
  (if (string= "" text) (error "No string specified."))
  (cond ((lse-tpu:mark)
         (save-excursion
           (if (> (point) (lse-tpu:mark)) (lse-tpu:exchange-point-and-mark))
           (while (and (< (point) (lse-tpu:mark))
                       (re-search-forward "^" (lse-tpu:mark) t)
                  )
             (if (< (point) (lse-tpu:mark)) (replace-match text))
           )
         )
        )
        (t
         (save-excursion
           (if (not (bolp))
               (lse-tpu:next-beginning-of-line 1)
           )
           (insert text)
         )
        )
  )
  (setq deactivate-mark nil); 17-Mar-1995
; lse-tpu:add-at-bol
)

(defun lse-tpu:add-at-eol (text)
  "Add text to the end of each line in a region,
or to the current line if no region is selected."
  (interactive
     (list (lse-tpu:string-prompt "String to add: " 'lse-tpu:add-at-eol-hist))
  )
  (if (string= "" text) (error "No string specified."))
  (cond ((lse-tpu:mark)
         (save-excursion
           (if (> (point) (lse-tpu:mark)) (lse-tpu:exchange-point-and-mark))
           (while (< (point) (lse-tpu:mark))
             (end-of-line)
             (if (<= (point) (lse-tpu:mark)) (insert text))
             (lse-tpu:forward-line 1)
           )
         )
        )
        (t
         (save-excursion
           (if (not (eolp))
               (lse-tpu:next-end-of-line 1)
           )
           (insert text)
         )
        )
  )
  (setq deactivate-mark nil); 17-Mar-1995
; lse-tpu:add-at-eol
)

(defun lse-tpu:remove-from-bol (text)
  "Remove text from the beginning of each line in a region,
or from the current line if no region is selected."
  (interactive
     (list (lse-tpu:string-prompt "String to remove: " 'lse-tpu:add-at-bol-hist))
  )
  (if (string= "" text) (error "No string specified."))
  (cond ((lse-tpu:mark)
         (save-excursion
           (if (> (point) (lse-tpu:mark)) (lse-tpu:exchange-point-and-mark))
           (while (and (< (point) (lse-tpu:mark))
                       (re-search-forward (concat "^" text) (lse-tpu:mark) t)
                  )
             (if (< (point) (lse-tpu:mark))
                 (delete-region (match-beginning 0) (match-end 0))
             )
           )
         )
        )
        (t
         (save-excursion
           (if (not (bolp))
               (lse-tpu:next-beginning-of-line 1)
           )
           (if (looking-at (concat "^" text))
               (delete-region (match-beginning 0) (match-end 0))
           )
         )
        )
  )
  (setq deactivate-mark nil); 17-Mar-1995
; lse-tpu:remove-from-bol
)

(defun lse-tpu:remove-from-eol (text)
  "Remove text from the end of each line in a region,
or from the current line if no region is selected."
  (interactive
     (list (lse-tpu:string-prompt "String to remove: " 'lse-tpu:add-at-eol-hist))
  )
  (if (string= "" text) (error "No string specified."))
  (cond ((lse-tpu:mark)
         (save-excursion
           (if (> (point) (lse-tpu:mark)) (lse-tpu:exchange-point-and-mark))
           (while (and (< (point) (lse-tpu:mark))
                       (re-search-forward (concat text "$") (lse-tpu:mark) t)
                  )
             (if (<= (point) (lse-tpu:mark))
                 (delete-region (match-beginning 0) (match-end 0))
             )
           )
         )
        )
        (t
         (save-excursion
           (save-restriction
             (narrow-to-region
                  (lse-tpu:line-head-pos) (1+ (lse-tpu:line-tail-pos))
             )
             (if (re-search-forward (concat text "$") nil t)
                 (delete-region (match-beginning 0) (match-end 0))
             )
           )
         )
        )
  )
  (setq deactivate-mark nil); 17-Mar-1995
; lse-tpu:remove-from-eol
)

(defun lse-tpu:trim-line-ends nil
  "Removes trailing whitespace from every line in the buffer."
  (interactive)
  (picture-clean)
  (setq deactivate-mark nil); 17-Mar-1995
; lse-tpu:trim-line-ends
)

;;;
;;;  Movement by character
;;;
(defun lse-tpu:forward-char (num)
  "Move right ARG characters (left if ARG is negative)."
  (interactive "p")
  (condition-case nil
      (forward-char num)
    (error (lse-message ""))
  )
; lse-tpu:forward-char
)

(defun lse-tpu:backward-char (num)
  "Move left ARG characters (right if ARG is negative)."
  (interactive "p")
  (condition-case nil
      (backward-char num)
    (error (lse-message ""))
  )
; lse-tpu:backward-char
)

;;;
;;;  Movement by line
;;;
(defun lse-tpu:next-line-internal (num &optional command)
  (condition-case nil
      (progn
        (next-line-internal num)
        (and command (setq this-command command))
      )
    (error (lse-message ""))
  )
; lse-tpu:next-line-internal
)

(defun lse-tpu:next-line (num)
  "Move to next line.
Prefix argument serves as a repeat count."
  (interactive "p")
  (lse-tpu:next-line-internal num 'next-line)
; lse-tpu:next-line
)

(defun lse-tpu:previous-line (num)
  "Move to previous line.
Prefix argument serves as a repeat count."
  (interactive "p")
  (lse-tpu:next-line-internal (- num) 'previous-line)
; lse-tpu:previous-line
)

(defun lse-tpu:next-beginning-of-line (num)
  "Move to beginning of line; if at beginning, move to beginning of next line.
Accepts a prefix argument for the number of lines to move."
  (interactive "p")
  (lse-tpu:backward-char 1)
  (lse-tpu:forward-line  (- 1 num))
; lse-tpu:next-beginning-of-line
)

(defun lse-tpu:next-end-of-line (num)
  "Move to end of line; if at end, move to end of next line.
Accepts a prefix argument for the number of lines to move."
  (interactive "p")
  (lse-tpu:forward-char 1)
  (end-of-line num)
; lse-tpu:next-end-of-line
)

(defun lse-tpu:previous-end-of-line (num)
  "Move EOL upward.
Accepts a prefix argument for the number of lines to move."
  (interactive "p")
  (end-of-line (- 1 num))
; lse-tpu:previous-end-of-line
)

(defun lse-tpu:current-end-of-line nil
  "Move point to end of current line."
  (interactive)
  (let ((beg (point)))
    (end-of-line)
    (if (= beg (point)) (message "You are already at the end of a line."))
  )
; lse-tpu:current-end-of-line
)

(defun lse-tpu:forward-line (num)
  "Move to beginning of next line.
Prefix argument serves as a repeat count."
  (interactive "p")
  (condition-case nil
      (forward-line num)
    (error (lse-message ""))
  )
; lse-tpu:forward-line
)

(defun lse-tpu:backward-line (num)
  "Move to beginning of previous line.
Prefix argument serves as repeat count."
  (interactive "p")
  (lse-tpu:forward-line (- num))
; lse-tpu:backward-line
)

;;;
;;;  Movement by paragraph
;;;
(defun lse-tpu:next-paragraph (num)
  "Move to beginning of the next paragraph.
Accepts a prefix argument for the number of paragraphs."
  (interactive "p")
  (beginning-of-line)
  (while (and (not (eobp)) (> num 0))
    (if (re-search-forward "^[ \t]*$" nil t)
        (if (re-search-forward "[^ \t\n]" nil t)
            (goto-char (match-beginning 0))
          (goto-char (point-max))
        )
    )
    (setq num (1- num))
  )
  (beginning-of-line)
; lse-tpu:next-paragraph
)


(defun lse-tpu:previous-paragraph (num)
  "Move to beginning of previous paragraph.
Accepts a prefix argument for the number of paragraphs."
  (interactive "p")
  (end-of-line)
  (while (and (not (bobp)) (> num 0))
    (if (not (and (re-search-backward "^[ \t]*$" nil t)
                  (re-search-backward "[^ \t\n]" nil t)
                  (re-search-backward "^[ \t]*$" nil t)
                  (progn (re-search-forward "[^ \t\n]" nil t)
                         (goto-char (match-beginning 0))
                  )
             )
        )
        (goto-char (point-min))
    )
    (setq num (1- num))
  )
  (beginning-of-line)
; lse-tpu:previous-paragraph
)

;;;
;;;  Movement by page
;;;
;;;  8-Sep-2002
(defun lse-tpu:page-forward (num)
  "Mode to the end of the current page.
A repeat count means move that many pages."
  (interactive "p")
  (lse-tpu:save-pos-before-search)
  (forward-page num)
  (if (eobp) (recenter -1))
; lse-tpu:page-forward
)

;;;  8-Sep-2002
(defun lse-tpu:page-backward (num)
  "Mode to the beginning of the current page.
A repeat count means move that many pages."
  (interactive "p")
  (lse-tpu:save-pos-before-search)
  (backward-page num)
  (if (bobp) (recenter 1))
; lse-tpu:page-backward
)

;;;
;;;  Scrolling and movement within the buffer
;;;
(defun lse-tpu:current-line nil
  "Return the vertical position of point in the selected window.
Top line is 0.  Counts each text line only once, even if it wraps."
  (+ (count-lines (window-start) (point)) (if (= (current-column) 0) 1 0) -1)
)

(defun lse-tpu:scroll-window-down (num)
  "Scroll the display down to the next section.
A repeat count means scroll that many sections."
  (interactive "p")
  (let* ((beg (lse-tpu:current-line))
         (height (1- (window-height)))
         (lines (* num (/ (* height lse-tpu:percent-scroll) 100)))
        )
    (lse-tpu:next-line-internal (- lines))
    (if (> lines beg) (recenter 0))
  )
; lse-tpu:scroll-window-down
)

(defun lse-tpu:scroll-window-up (num)
  "Scroll the display up to the next section.
A repeat count means scroll that many sections."
  (interactive "p")
  (let* ((beg (lse-tpu:current-line))
         (height (1- (window-height)))
         (lines (* num (/ (* height lse-tpu:percent-scroll) 100)))
        )
    (lse-tpu:next-line-internal lines)
    (if (>= (+ lines beg) height) (recenter -1))
  )
; lse-tpu:scroll-window-up
)

(defun lse-tpu:move-to-beginning nil
  "Move cursor to the beginning of buffer, but don't set the mark."
  (interactive)
  (goto-char (point-min))
; lse-tpu:move-to-beginning
)

(defun lse-tpu:move-to-end nil
  "Move cursor to the end of buffer, but don't set the mark."
  (interactive)
  (goto-char (point-max))
  (recenter -1)
; lse-tpu:move-to-end
)

;;;
;;;  Emacs version 19 minibuffer history support
;;;
(defun lse-tpu:next-history-element (n)
  "Insert the next element of the minibuffer history into the minibuffer."
  (interactive "p")
  (next-history-element n)
  (goto-char (point-max))
; lse-tpu:next-history-element
)

(defun lse-tpu:previous-history-element (n)
  "Insert the previous element of the minibuffer history into the minibuffer."
  (interactive "p")
  (previous-history-element n)
  (goto-char (point-max))
; lse-tpu:previous-history-element
)

(defun lse-tpu:arrow-history nil
  "Modify minibuffer maps to use arrows for history recall."
  (interactive)
  (let ((loc (where-is-internal 'lse-tpu:previous-line))
        (cur nil)
       )
    (while (setq cur (car loc))
      (define-key read-expression-map             cur 'lse-tpu:previous-history-element)
      (define-key minibuffer-local-map            cur 'lse-tpu:previous-history-element)
      (define-key minibuffer-local-ns-map         cur 'lse-tpu:previous-history-element)
      (define-key minibuffer-local-completion-map cur 'lse-tpu:previous-history-element)
      (define-key minibuffer-local-must-match-map cur 'lse-tpu:previous-history-element)
      (setq loc (cdr loc))
    )
    (setq loc (where-is-internal 'lse-tpu:next-line))
    (while (setq cur (car loc))
      (define-key read-expression-map             cur 'lse-tpu:next-history-element)
      (define-key minibuffer-local-map            cur 'lse-tpu:next-history-element)
      (define-key minibuffer-local-ns-map         cur 'lse-tpu:next-history-element)
      (define-key minibuffer-local-completion-map cur 'lse-tpu:next-history-element)
      (define-key minibuffer-local-must-match-map cur 'lse-tpu:next-history-element)
      (setq loc (cdr loc))
    )
  )
; lse-tpu:arrow-history
)


;;;
;;;  Emacs version 19 X-windows key definition support
;;;
(defun lse-tpu:edt-on nil
  "Turn on TPU/edt emulation."
  (interactive)
  (cond
   ((not lse-tpu:edt-mode)
    (lse-tpu:arrow-history)
    (lse-tpu:set-mode-line t)
    (lse-tpu:set-search t)
    (lse-tpu:update-mode-line)
    ;; set page delimiter, display line truncation, and scrolling like TPU
    (setq-default page-delimiter "\f")
    (setq-default truncate-lines t)
    (setq scroll-step 1)
    (setq lse-tpu:edt-mode t)
   )
  )
; lse-tpu:edt-on
)
;;;
;;;  Turn on lse-tpu and announce it as a feature
;;;
(lse-tpu:edt-on)

;;;
;;; Put 'delete-selection on some lse-tpu commands
;;;
(put 'lse-tpu:delete-prev-char    'delete-selection 'supersede)
(put 'lse-tpu:delete-next-char    'delete-selection 'supersede)
(put 'lse-tpu:undelete-word       'delete-selection 'kill)
(put 'lse-tpu:undelete-line       'delete-selection 'kill)
(put 'lse-tpu:undelete-char       'delete-selection 'kill)
(put 'lse-tpu:paste-region        'delete-selection 'kill)

(provide 'lse-tpu)
