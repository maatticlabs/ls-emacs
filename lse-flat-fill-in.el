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
;;;;    lse-flat-fill-in
;;;;
;;;; Purpose
;;;;    Functions for dealing with flat (i.e., unexpanded) fill-ins
;;;;
;;;; Revision Dates
;;;;    24-May-1994 (CT) Creation (of comment)
;;;;    24-May-1994 (CT) Clarification of structure (major surgery!)
;;;;    26-May-1994 (CT) Simple tokens: handling of lists (consp)
;;;;    26-May-1994 (CT) Interactive functions moved to lse-interactive
;;;;    12-Jun-1994 (CT) Correction of errors
;;;;    12-Jun-1994 (CT) Auto-replication added
;;;;    26-Jun-1994 (CT) replacement-vanguard added
;;;;    27-Jun-1994 (CT) Variable lse-flat-fill-in:open-replacement added
;;;;     8-Sep-1994 (CT) case-fold parameter passed to lse-complete
;;;;    22-Sep-1994 (CT) lse-insert-replica-or-flat added
;;;;    24-Sep-1994 (CT) Error corrected
;;;;    24-Sep-1994 (CT) lse-insert-replica-or-flat-opt added
;;;;    26-Sep-1994 (CT) Error in token expansion corrected (did not work
;;;;                     for menu and function fill-in's)
;;;;    30-Sep-1994 (CT) Force manual completion of menu's
;;;;                     (otherwise a menu containing an empty entry is never
;;;;                     seen by the user)
;;;;    30-Sep-1994 (CT) Removed optional parameter `quiet' from
;;;;                     lse_expand_token:try
;;;;    30-Sep-1994 (CT) Recover collapsed range of fill-in
;;;;                     Such a collapse can happen by actions performed by
;;;;                     completion-leading or one of its friends (e.g., if
;;;;                     `just-one-space' is executed and the replacement
;;;;                     consisted of blanks only)
;;;;    22-Jan-1995 (CT) lse_replicate_fill_in and
;;;;                     lse_replicate_fill_in_by_older moved in here (were
;;;;                     contained inline in lse-replicate-fill-in and
;;;;                     lse-replicate-fill-in-by-older in lse-interactive.el)
;;;;    22-Jan-1995 (CT) lse_replicate-fill-in and
;;;;                     lse-replicate-fill-in-by-older stay in replacement
;;;;                     mode
;;;;    16-Feb-1995 (CT) lse-expand-future-fill-in added
;;;;    18-Feb-1995 (CT) lse-fill-in:kill-action added
;;;;    23-Feb-1995 (CT) Maintain order of @-menu-entries
;;;;    24-Feb-1995 (CT) lse-insert-replica factored from
;;;;                     lse-insert-replica-or-flat
;;;;    24-Feb-1995 (CT) Changed error message for non-replaceable fill-ins
;;;;    25-Feb-1995 (CT) Error in lse-insert-replica corrected
;;;;    26-Feb-1995 (CT) lse-auto-replicate-future-fill-in added
;;;;                     lse-flat-fill-in:auto-replicating added
;;;;    27-Feb-1995 (CT) lse_expand-menu remembers last selection
;;;;     2-Mar-1995 (CT) optional argument trailer for lse-insert-replica*
;;;;    18-Mar-1995 (CT) lse-flat-fill-in:open-replacement-highlight and
;;;;                     lse-flat-fill-in:close-replacement-highlight added
;;;;    18-Mar-1995 (CT) lse-flat-fill-in:open-replacement-bobp added
;;;;    19-Mar-1995 (CT) Text properties for flat fill-in's added
;;;;                     (lse-flat-fill-in:keymap and some functions)
;;;;    20-Mar-1995 (CT) Let lse-insert-replica deal with dead fill-in's in
;;;;                     expansion history
;;;;    25-Mar-1995 (CT) Highlighting done by overlays instead of text
;;;;                     properties (first try at 22-Mar, but it took some
;;;;                     time to work around Emacs's idiosyncrasies)
;;;;    29-Mar-1995 (CT) Don't care about empty replacements (previously, the
;;;;                     flat fill-in was restored by
;;;;                     lse-fill-in:shut-replacement)
;;;;    23-Apr-1995 (CT) Check if replacement allowed
;;;;                     (lse-flat-fill-in:auto-open-replacement)
;;;;    19-Aug-1995 (CT) Optional argument `lazy' added to
;;;;                     `lse-flat-fill-in:close-replacement'
;;;;    24-Nov-1995 (CT) Message after end of replacement disabled
;;;;    10-Mar-1996 (CT) `lse_replicate_fill_in_menu*' added
;;;;    27-Sep-1996 (CT) redraw-display replaced by redraw-frame
;;;;     2-Oct-1996 (CT) Hack around change of semantics of
;;;;                     insert-in-front-hooks bug introduced by Emacs 19.30
;;;;     2-Oct-1996 (CT) Use add-to-list in lse_replicate_fill_in_menu_entries
;;;;     3-Oct-1996 (CT) lse_expand_token:try changed to return `t' if there
;;;;                     was more than 1 matching token (to avoid more
;;;;                     completion windows after the user aborted completion)
;;;;     7-Oct-1996 (CT) insert-in-front-hooks hack refactored
;;;;     7-Oct-1996 (CT) Replaced
;;;;                         `(or lse_current_fill-in (lse_inside_fill-in))'
;;;;                     by
;;;;                         `(lse_inside_fill-in)'.
;;;;                     The old code was marginally more efficient but
;;;;                     terribly incorrect.
;;;;     9-Oct-1996 (CT) Use add-to-list unconditionally in
;;;;                     lse_replicate_fill_in_menu_entries (now defined by
;;;;                     lse-list-util if not by Emacs)
;;;;     9-Oct-1996 (CT) Added optional parameter `dont-highlight' to
;;;;                     `lse-fill-in:toggle-expansion'
;;;;    11-Oct-1996 (CT) Use 'category to define text-properties for current
;;;;                     fill-in
;;;;    13-Oct-1996 (CT) Use lse-fill-in:{add|remove}-text-properties
;;;;    13-Oct-1996 (CT) Use different categories for different fill-in types
;;;;    13-Oct-1996 (CT) lse-flat-fill-in:highlight-all added
;;;;    13-Oct-1996 (CT) Text properties for fill-in id's added (id, parent)
;;;;    16-Oct-1996 (CT) lse-flat-fill-in:highlight-all corrected
;;;;    17-Oct-1996 (CT) Handling of 'id and 'parent-id corrected
;;;;    17-Dec-1996 (CT) lse-flat-fill-in:highlight-all corrected (don't
;;;;                     highlight first and last character of fill-in range
;;;;                     to avoid merging of property ranges of adjacent
;;;;                     fill-ins)
;;;;    18-Mar-1997 (CT) Vanguard-expansion in lse-expand-fill-in-token
;;;;    18-Mar-1997 (CT) Changed vanguard-expansion of
;;;;                     lse-flat-fill-in:open-replacement
;;;;                         - `save-excursion'
;;;;                         - don't `lse-goto-next-fill-in' after expansion
;;;;    13-Dec-1997 (CT) `lse-flat-fill-in:replace-and-delegate-key' used for
;;;;                     auto-replacement of fill-ins instead of
;;;;                         `'insert-in-front-hooks
;;;;                          '(lse-flat-fill-in:auto-open-replacement)
;;;;                         '
;;;;    13-Dec-1997 (CT) `lse-flat-fill-in:define-flat-keys' added
;;;;    29-Dec-1997 (CT) `lse-fill-in-marks:insert-*' added
;;;;    31-Dec-1997 (CT) `lse-flat:join-sexp-boundary-maybe' added
;;;;     1-Jan-1998 (CT) `lse-flat:join-sexp-boundary-maybe' corrected
;;;;     5-Jan-1998 (CT) `lse-flat-fill-in:remove-*-blank-line' added
;;;;    18-Jan-1998 (CT) `save-window-excursion' added to
;;;;                     `lse_replicate_fill_in_menu'
;;;;    18-Jan-1998 (CT) `lse-flat-fill-in:remove-*-whitespace' added
;;;;    28-Dec-1999 (CT) Text property `intangible' added for flat fill-in
;;;;    17-Aug-2000 (CT) `auto-expand' added
;;;;     4-Oct-2002 (CT) `hang-indent` added
;;;;     7-Oct-2002 (CT) `properties`  added
;;;;    20-Mar-2003 (CT) `lse-remove-next-blank-lines` and
;;;;                     `lse-remove-prev-blank-lines` factored
;;;;    27-Apr-2003 (CT) `lse_expand_token:try` changed to try new-lines, too
;;;;     4-May-2007 (CT) `lse_expand_token:try` changed to allow spaces after
;;;;                     new-line tokens
;;;;    15-Oct-2007 (CT) `lse_start_replacement_if_in_fill-in` defined as
;;;;                     `defun` instead of as `defmacro`
;;;;    29-Jul-2009 (CT) Modernize use of backquotes
;;;;    10-Nov-2010 (CT) Use `mapc` instead of `mapcar` where appropriate
;;;;    19-Jan-2011 (CT) `lse-auto-expand-replacement-fill-in` added
;;;;    21-Jan-2011 (CT) `lse_expand_token:try-1` factored from
;;;;                     `lse_expand_token:try`, the later changed to also
;;;;                     try a token without leading whitespace
;;;;     4-Jul-2012 (CT) Rewrite `lse-flat-fill-in:replace-and-mouse-yank` to
;;;;                     work in Emacs 24, too
;;;;     4-Jul-2012 (CT) Use `let`, not `setq`, to temporarily change
;;;;                     `lse::expansion@separator` in
;;;;                     `lse-flat-fill-in:expand-menu`
;;;;     9-Nov-2014 (CT) Change `lse-flat-fill-in:replace-and-mouse-yank` to
;;;;                     use `lse-tpu:mouse-paste:get-primary`
;;;;                     and `lse-tpu:mouse-paste:insert`
;;;;    12-Nov-2014 (CT) Remove support for ancient Emacs versions
;;;;    14-Nov-2014 (CT) Change `interactive` of
;;;;                     `lse-flat-fill-in:replace-and-mouse-yank` to "^e"
;;;;    16-Nov-2014 (CT) Change back `interactive` of
;;;;                     `lse-flat-fill-in:replace-and-mouse-yank` to "e"
;;;;     4-Jan-2016 (CT) Use `:propertize` for mode-line
;;;;    23-Nov-2016 (CT) Add and use `lse-flat-fill-in:replacement:recode`
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-flat-fill-in)

(defvar lse::expansion-separator          nil)
(defvar lse-flat-fill-in:no-vanguard      nil); 26-Jun-1994
(defvar lse-flat-fill-in:auto-replicating nil); 26-Feb-1995

(defmacro with_lse_fill_environment_at (pos &rest forms)
  `(let (lse::current-expansion-indent
         lse::original-expansion-indent
         lse::environment-expansion-indent
         lse::expansion-line-leading
         lse::expansion-line-trailer
         lse::expansion-line-leading-indent
         lse::expansion-line-trailer-indent
        )
     (save-excursion
       (and ,pos (goto-char ,pos))
       (setq lse::current-expansion-indent     (current-column))
       (setq lse::original-expansion-indent    (current-column))
       (setq lse::environment-expansion-indent (current-indentation))
       (lse-comment:setup_expansion_leading)
       (lse-comment:setup_expansion_trailer)
     )
     ,@forms
   )
; with_lse_fill_environment_at
)

(defun lse_show_fill-in_description (psym)
  (let ((desc (get psym 'description)))
    (if (stringp desc) (message desc))
  )
; lse_show_fill-in_description
)

(defun lse_show_fill-in_help (psym)
  (let ((help (get psym 'help))
        (desc (get psym 'description))
       )
    (if (stringp help)
        (with-output-to-temp-buffer " $lse help$"
            (princ "Fill-in : ")
            (princ (symbol-name psym))
            (princ "\n")
            (princ "Type    : ")
            (princ (get psym 'type))
            (princ "\n\n\n")
            (princ desc)
            (princ "\n\n")
            (princ help)
            (princ "\n")
        )
      (if (stringp desc) (message desc))
    )
  )
; lse_show_fill-in_help
)

(defun lse-fill-in:option_fill-in (psym option)
  (let ((result (get psym option))
       )
    (if result (lse_fill-in:definition (symbol-name result)))
  )
; lse-fill-in:option_fill-in
)

(defun lse-fill-in:rcompletion-action (psym)
  (lse-fill-in:option_fill-in psym 'rcompletion-action)
)

(defun lse-fill-in:rcompletion-leading (psym)
  (lse-fill-in:option_fill-in psym 'rcompletion-leading)
)

(defun lse-fill-in:rcompletion-trailer (psym)
  (lse-fill-in:option_fill-in psym 'rcompletion-trailer)
)

(defun lse-fill-in:replacement-leading (psym)
  (lse-fill-in:option_fill-in psym 'replacement-leading)
)

(defun lse-fill-in:replacement-trailer (psym)
  (lse-fill-in:option_fill-in psym 'replacement-trailer)
)

;;; 26-Jun-1994
(defun lse-fill-in:replacement-vanguard (psym)
  (lse-fill-in:option_fill-in psym 'replacement-vanguard)
)

;;; 18-Feb-1995
(defun lse-fill-in:kill-action (psym)
  (lse-fill-in:option_fill-in psym 'kill-action)
)


(defun lse-flat-fill-in:clean-current (psym flat-range)
  ;; PRECONDITION (equal (point) (lse-range:head-pos flat-range))
  (let (dupl-range)
    (if (not (lse_current_fill-in_repeats))
        (lse-range:clean flat-range)
      (setq dupl-range
            (lse_convert_current_fill-in_to_duplicate psym flat-range)
      )
    )
    (lse-range:change-tail-pos flat-range (lse-range:head-pos flat-range))
    dupl-range
  )
; lse-flat-fill-in:clean-current
)

(defvar                      lse-flat-fill-in:open-replacement nil)
(make-variable-buffer-local 'lse-flat-fill-in:open-replacement)

;;; 18-Mar-1995
(defvar                      lse-flat-fill-in:open-replacement-bobp nil)
(make-variable-buffer-local 'lse-flat-fill-in:open-replacement-bobp)

;;; 22-Mar-1995
(defvar                      lse-flat-fill-in:replacement-overlay nil)
(make-variable-buffer-local 'lse-flat-fill-in:replacement-overlay)

;;; 18-Mar-1995
(defun lse-flat-fill-in:open-replacement-highlight (head-pos tail-pos)
  (lse-flat-fill-in:unhighlight-current)            ; 25-Mar-1995
  (setq lse-flat-fill-in:replacement-overlay        ; 25-Mar-1995
        (make-overlay head-pos (1+ tail-pos))
  )
  (overlay-put lse-flat-fill-in:replacement-overlay ; 25-Mar-1995
               'face 'lse-face:open-replacement
  )
  (if (and lse-emacsX-p (eq (lse-line-number) 1))   ; 25-Mar-1995
      (redraw-frame (selected-frame))
          ; Otherwise strange things happen on the screen.
          ; Stallman knows what feature or bug is responsible for
          ; this funny behavior
  )
; lse-flat-fill-in:open-replacement-highlight
)

;;; 18-Mar-1995
(defun lse-flat-fill-in:close-replacement-highlight (head-pos tail-pos)
  (and lse-flat-fill-in:replacement-overlay; 25-Mar-1995
       (delete-overlay lse-flat-fill-in:replacement-overlay)
  )
  (if lse-flat-fill-in:open-replacement-bobp
      (save-excursion
        (goto-char (point-min))
        (lse-tpu:delete-next-char 1)
      )
  )
; lse-flat-fill-in:close-replacement-highlight
)

;;; 28-Dec-1999
(defvar lse-flat-fill-in:flat-properties
        '(category nil intangible nil read-only t)
)

(defun lse-flat-fill-in:open-replacement (psym name fill-type)
  (setq lse-flat-fill-in:open-replacement fill-type)
  (lse-fill-in:set-id lse_current_fill-in); 17-Oct-1996
  (let ((rvanguard (lse-fill-in:replacement-vanguard psym)); 26-Jun-1994
       )
    (if (and rvanguard (not lse-flat-fill-in:no-vanguard))
        (save-excursion ; 18-Mar-1997
          (goto-char
               (lse-range:head-pos (lse-fill-in:range lse_current_fill-in))
          )
          (lse-flat-fill-in:interpret-replacement_in_env rvanguard)
          ; 18-Mar-1997 ; (lse-goto-next-fill-in) ; updates lse_current_fill-in's head-pos
        )
    )
  )
  (let* ((flat-range  (lse-fill-in:range                lse_current_fill-in))
         (inner-range (lse-fill-in:inner-range          lse_current_fill-in))
         (head-pos    (lse-range:head-pos               flat-range))
         (tail-pos    (lse-range:tail-pos               flat-range))
         (complement  (lse-range:contents               flat-range))
         (rleading    (lse-fill-in:replacement-leading  psym))
         (rtrailer    (lse-fill-in:replacement-trailer  psym))
         (p-id        (get-text-property head-pos 'lse-fill-in:id))
         p
         dupl-range
        )
    (goto-char head-pos)
    (lse-fill-in:remove-text-properties; 19-Mar-1995
         head-pos tail-pos lse-flat-fill-in:flat-properties
    )
    (lse-fill-in:add-text-properties head-pos (lse-range:tail-pos flat-range)
         (list 'lse-fill-in:id (lse-fill-in:id lse_current_fill-in))
    )
    (if (bobp); 18-Mar-1995
        (progn
          (insert-before-markers "\n")
          (setq lse-flat-fill-in:open-replacement-bobp t)
        )
      (setq lse-flat-fill-in:open-replacement-bobp nil)
    )
    (setq dupl-range (lse-flat-fill-in:clean-current psym flat-range))
    (or lse-flat-fill-in:auto-replicating; 26-Feb-1995
        (lse-flat-fill-in:interpret-replacement_in_env rleading)
    )
    (setq p (point-marker))
    (save-excursion
      (or lse-flat-fill-in:auto-replicating; 26-Feb-1995
          (lse-flat-fill-in:interpret-replacement_in_env rtrailer head-pos)
      )
    )
    (if inner-range
        (progn
          (lse-range:change-head-pos inner-range p)
          (lse-range:change-tail-pos inner-range p)
        )
      (setq inner-range (lse-range:new p p))
      (lse-fill-in:change-inner-range lse_current_fill-in inner-range)
    )
    (lse-flat-fill-in:open-replacement-highlight head-pos p); 18-Mar-1995
    (lse-fill-in:change-state      lse_current_fill-in 'lse::deep)
    (lse-fill-in:change-fill-type  lse_current_fill-in fill-type)
    (lse-fill-in:change-complement lse_current_fill-in complement)
    (lse-fill-in:change-duplicate  lse_current_fill-in dupl-range)
    (lse-fill-in:set-parent-id     lse_current_fill-in p-id); 17-Oct-1996
    (lse-fill-in:add-text-properties head-pos (point); 17-Oct-1996
         (list 'lse-fill-in:id (lse-fill-in:id lse_current_fill-in))
    )
    (lse-fill-in-marks:insert-head head-pos); 29-Dec-1997
    (undo-boundary); 13-Oct-1996
    lse_current_fill-in
  )
; lse-flat-fill-in:open-replacement
)

(defun lse-flat-fill-in:close-replacement
           (psym &optional dont-save-for-unexpand lazy)
  (let* ((range      (lse-fill-in:range      lse_current_fill-in))
         (head-pos   (lse-range:head-pos     range))
         (rcaction   (lse-fill-in:rcompletion-action  psym))
         (rcleading  (lse-fill-in:rcompletion-leading psym))
         (rctrailer  (lse-fill-in:rcompletion-trailer psym))
         (p-id       (lse-fill-in:parent-id  lse_current_fill-in))
         lse_replaced_fill-in ; 30-Sep-1994 avoid infinite recursion in case
                              ; that rcompletion-* calls lse-expand-token
        )
    (lse-flat-fill-in:close-replacement-highlight (lse-range:head range)
                                                  (lse-range:tail range)
    ); 18-Mar-1995
    (if lazy ;  19-Aug-1995
        t    ;  19-Aug-1995
      (save-excursion
        (and rcleading
             (goto-char head-pos)
             (lse-flat-fill-in:interpret-replacement_in_env rcleading)
        )
        (and rctrailer
             (goto-char (lse-range:tail-pos range))
             (lse-flat-fill-in:interpret-replacement_in_env rctrailer head-pos)
        )
      )
      (and rcaction (lse-flat-fill-in:interpret-replacement_in_env rcaction))
    )
    (if (lse-range:is-collapsed range); 30-Sep-1994 avoid errors
        (lse-range:change-tail-pos range (lse-range:head range))
    )
    (if (lse-fill-in:duplicate lse_current_fill-in)
        (lse-range:change-head (lse-fill-in:duplicate lse_current_fill-in)
                               (lse-range:tail range)
        )
    )
    (lse-flat-fill-in:auto-replicate psym (lse-range:contents-np range))
    (or dont-save-for-unexpand (get psym 'no-history)
        (lse_fill-in_history:add_expansion lse_current_fill-in)
    )
    (lse-fill-in:set-id lse_current_fill-in); 17-Oct-1996
    (lse-fill-in:add-text-properties  head-pos (lse-range:tail-pos range)
         (list 'lse-fill-in:id   (lse-fill-in:id lse_current_fill-in)
               'lse-fill-in:parent-id p-id
         )
    )
    (lse-fill-in-marks:insert-tail (lse-range:tail-pos range)); 29-Dec-1997
    (setq lse-flat-fill-in:open-replacement nil)
    range
  )
; lse-flat-fill-in:close-replacement
)

;;; 23-Nov-2016
(defun lse-flat-fill-in:replacement:recode (head-pos tail-pos)
  (let ((bcs buffer-file-coding-system)
       )
    (unless (eq bcs 'utf-8-unix)
      (recode-region head-pos tail-pos 'utf-8 bcs)
    )
  )
; lse-flat-fill-in:replacement:recode
)

;;; 31-Dec-1997
;;; recompletion-action to remove whitespace around sexp
(defun lse-flat:join-sexp-boundary-maybe ()
  (if lse_current_fill-in
      (if (not (save-excursion
                 (skip-chars-forward  " \t\n")
                 (lse_looking_at_fill-in)
               )
          ); 1-Jan-1998 ; don't join if before a flat fill-in
          (let* ((s-bound    (lse-join-sexp-boundary-maybe))
                 (complement (lse-fill-in:complement lse_current_fill-in))
                 (range      (lse-fill-in:range      lse_current_fill-in))
                 (head       (and (listp s-bound) (car   s-bound)))
                 (tail       (and (listp s-bound) (nth 1 s-bound)))
                 (hws-added  (and (listp s-bound) (nth 2 s-bound))); 1-Jan-1998
                 (tws-added  (and (listp s-bound) (nth 2 s-bound))); 1-Jan-1998
                 (h-txt      (and (listp head)    (cdr head)))
                 (t-txt      (and (listp tail)    (cdr tail)))
                )
            (if h-txt
                (progn
                  (setq complement (concat h-txt complement))
                  (if hws-added          ;  1-Jan-1998
                      (lse-range:change-head range (1- (lse-range:head range)))
                  );avoid expand/unexpand to change spacing before flat fill-in
                )
            )
            (if t-txt
                (progn
                  (setq complement (concat complement t-txt))
                  (if tws-added          ;  1-Jan-1998
                      (lse-range:change-tail range (1+ (lse-range:tail range)))
                  );avoid expand/unexpand to change spacing after flat fill-in
                )
            )
            (lse-fill-in:change-complement lse_current_fill-in complement)
          )
      )
  )
; lse-flat:join-sexp-boundary-maybe
)

(defun lse-flat-fill-in:expand-replacement
           (psym name &optional dont-save-for-unexpand expansion)
  (let ((lse-flat-fill-in:expansion-in-progress t)
        (flat-range  (lse-fill-in:range lse_current_fill-in))
        (hang-indent (get psym 'hang-indent)); 4-Oct-2002
        (props       (get psym 'properties));  4-Oct-2002
        range
       )
    (lse-flat-fill-in:open-replacement psym name 'lse::expanded)
    (if expansion
        (lse-fill-in-insert expansion)
      (lse-flat-fill-in:interpret-replacement_in_env psym)
    )
    (setq range
          (lse-flat-fill-in:close-replacement psym dont-save-for-unexpand)
    )
    (lse_fill-in_history:purge_unexpansion psym range)
    (let ((head-pos (lse-range:head-pos range))
          (tail-pos (lse-range:tail-pos range))
         );  4-Oct-2002
      (lse-flat-fill-in:replacement:recode head-pos tail-pos)
      (if hang-indent;  4-Oct-2002
          (let* ((exp-indent
                   (save-excursion
                     (goto-char head-pos)
                     (current-column)
                   )
                 )
                )
            (lse-fill-in:add-text-properties head-pos tail-pos
                 (list 'hang-indent (+ exp-indent hang-indent))
            )
          )
      )
      (if props (lse-fill-in:add-text-properties head-pos tail-pos props))
    )
    (setq lse_current_fill-in nil)
    (or (lse_inside_fill-in)
        (lse-fill-in:goto-first-of-range
            (lse-range:head-pos range)
            (lse-tpu:line-tail-pos (or (get psym 'max-line-move) 1))
        )
    )
    range
  )
; lse-flat-fill-in:expand-replacement
)

(defun lse-flat-fill-in:expand-inner-replacement
           (psym name &optional expansion)
  (let ((rleading  (lse-fill-in:replacement-leading psym))
        (rtrailer  (lse-fill-in:replacement-trailer psym))
        (rcleading (lse-fill-in:rcompletion-leading psym))
        (rctrailer (lse-fill-in:rcompletion-trailer psym))
        (head-pos  (point))
       )
    (lse-flat-fill-in:interpret-replacement_in_env rleading)
    (save-excursion
        (lse-flat-fill-in:interpret-replacement_in_env rtrailer head-pos)
    )
    (if expansion
        (lse-fill-in-insert expansion)
      (lse-flat-fill-in:interpret-replacement_in_env psym)
    )
    (save-excursion
      (goto-char head-pos)
      (lse-flat-fill-in:interpret-replacement_in_env rcleading)
    )
    (lse-flat-fill-in:interpret-replacement_in_env rctrailer head-pos)
    t
  )
; lse-flat-fill-in:expand-inner-replacement
)

(defun lse-flat-fill-in:expand-menu
           (psym name &optional dont-save-for-unexpand)
  (let ((expansion (lse_expand_menu psym name (get psym 'menu))))
    (cond ((not expansion))
          ((symbolp expansion)
           (let ((lse::expansion-separator
                   (or lse::expansion-separator (get psym 'separator))
                 )
                )
             (lse-flat-fill-in:expand
               expansion (symbol-name expansion) dont-save-for-unexpand
             )
           )
          )
          ((stringp expansion)
           (lse-flat-fill-in:expand-replacement
                psym name dont-save-for-unexpand expansion
           )
          )
          (t (error "Invalid menu %s" name))
    )
  )
; lse-flat-fill-in:expand-menu
)

(defun lse-flat-fill-in:expand (psym name &optional dont-save-for-unexpand)
  (let ((fill-in-type (get psym 'type)))
    (cond ((eq fill-in-type 'terminal)
           (lse_show_fill-in_description psym)
          )
          ((eq fill-in-type 'replacement)
           (lse-flat-fill-in:expand-replacement
               psym name dont-save-for-unexpand
           )
          )
          ((eq fill-in-type 'menu)
           (lse-flat-fill-in:expand-menu
                psym name dont-save-for-unexpand
           )
          )
          ((eq fill-in-type 'function)
           (lse-flat-fill-in:interpret-replacement psym 'function)
          )
          ((not fill-in-type)
           (error "Undefined fill-in: `%s'" name)
          )
          (t (error "Unknown fill-in type: `%s'" name))
    )
  )
; lse-flat-fill-in:expand
)

;;;;++
;;;; Second internal level for expansion
;;;;--

;;;+
;;; lse-flat-fill-in:interpret-replacement performs the actual expansion of fill-in
;;; `psym':
;;;    - here the structure of the fill-in is interpreted
;;;    - no explicit change in position before expansion
;;;    - no change of environment
;;; after expansion the cursor position is just at the end of the deep text
;;;-
(defun lse-flat-fill-in:interpret-replacement (psym &optional kind)
  (if psym
      (let ((body  (get psym (or kind 'replacement)))
            next
           )
        (if body
            (progn
              (if (eobp)
                  (save-excursion            ; otherwise strange things happen!
                    (lse-fill-in-insert " ")
                    (lse-newline)
                  )
              )
              (while body
                (setq next (car body))
                (setq body (cdr body))
                (cond ((stringp next) (lse-fill-in-insert next))
                      ((symbolp next)
                       (if (symbol-function next)
                           (funcall next)
                         (if (stringp (symbol-value next))
                             (lse-fill-in-insert (symbol-value next))
                           (error "Invalid value for symbol %s: %s"
                                  (symbol-name next) (symbol-value next)
                           )
                         )
                       )
                      )
                      ((consp next) (eval next))
                      (t (error "Don't understand `%s'" next))
                )
              )
            ); progn
        ); if body
      ); let
  ); if psym
  psym; 12-Jun-1994
; lse-flat-fill-in:interpret-replacement
)

(defun lse-flat-fill-in:interpret-replacement_in_env (psym &optional env-pos)
  (if psym
      (with_lse_fill_environment_at env-pos
        (lse-flat-fill-in:interpret-replacement psym)
      )
  )
  psym; 12-Jun-1994
; lse-flat-fill-in:interpret-replacement_in_env
)

(defun lse-auto-expand-menu-fill-in (psym name)
  (let ((expansion (lse_expand_menu psym name (get psym 'menu)))
       )
    (cond ((symbolp expansion)
           (lse-auto-expand-fill-in (symbol-name expansion))
          )
          ((stringp expansion)
           (lse-flat-fill-in:expand-inner-replacement
                psym name expansion
           )
          )
          (t (error "Invalid menu %s" name))
    )
  )
)

;;;;++
;;;; First expansion level
;;;;--

;;; expand the fill-in associated to a token
(defun lse-expand-fill-in-token
           (psym token token-head token-tail &optional inner-replacement)
  (if inner-replacement
      (progn
        (delete-region token-head token-tail)
        (let ((rvanguard (lse-fill-in:replacement-vanguard psym))); 18-Mar-1997
          (and rvanguard
               (save-excursion
                  (lse-flat-fill-in:interpret-replacement_in_env rvanguard)
               )
          )
        )
        (lse-flat-fill-in:interpret-replacement_in_env psym)
        (let (lse_replaced_fill-in) ; otherwise fatal  recursion ; 12-Jun-1994
          (lse-fill-in:goto-first-of-range                       ; 12-Jun-1994
              token-head
              (lse-tpu:line-tail-pos (or (get psym 'max-line-move) 1))
          )                                                      ; 12-Jun-1994
        )                                                        ; 12-Jun-1994
        psym                                                     ; 12-Jun-1994
      )
    (setq lse_current_fill-in
          (lse-fill-in:new
               psym
               (symbol-name psym)
               'lse::flat
               'lse::expanded
               (lse-range:new token-head token-tail)
               nil
               token
               nil nil nil nil
          )
    )
    ;; 26-Sep-1994 `:expand' instead of `:expand-replacement' (otherwise
    ;; menu- and function-fill-in's fail)
    (lse-flat-fill-in:expand psym (symbol-name psym))
  )
; lse-expand-fill-in-token
)

;;; expansion of embedded fill-in's: @ "name"
(defun lse-auto-expand-fill-in (name)
  ;; expansion includes effects of leading, trainling, ...
  (let* ((psym (lse_fill-in:definition name))
         (fill-in-type (get psym 'type))
        )
    (cond ((eq fill-in-type 'terminal)
           (lse_show_fill-in_description psym)
          )
          ((eq fill-in-type 'replacement)
           (lse-flat-fill-in:expand-inner-replacement psym name)
          )
          ((eq fill-in-type 'menu)
           (lse-auto-expand-menu-fill-in psym name)
          )
          ((eq fill-in-type 'function); 26-Sep-1994
           (lse-flat-fill-in:interpret-replacement psym 'function)
          )
    )
  )
)

;;; inline-expansion of embedded fill-in's: $ "name"
;;; 19-Jan-2011
(defun lse-auto-expand-replacement-fill-in (name)
  ;; expansion includes only the replacement proper, but not leading, ...
  (let* ((psym (lse_fill-in:definition name))
         (fill-in-type (get psym 'type))
        )
    (cond ((eq fill-in-type 'replacement)
           (lse-flat-fill-in:interpret-replacement psym)
          )
          (t
           (lse-define:message
                  "Fill-in $ `%25s': not a replacement; ignored" name
             )
          )
    )
  )
; lse-auto-expand-replacement-fill-in
)

(defun lse_expand_menu:entry_list (the-entries)
  (let (result
        entry
        entry-name
        entry-sym
       )
    (while the-entries
      (setq entry       (car the-entries))
      (setq the-entries (cdr the-entries))
      (cond ((eq entry '@)
             (setq entry       (car the-entries))
             (setq the-entries (cdr the-entries))
             (setq entry-sym
                   (lse_fill-in:definition
                        (if (symbolp entry) (symbol-name entry) entry)
                   )
             )
             (setq result
               (append; 23-Feb-1995 reverse added
                 (reverse (lse_expand_menu:entry_list (get entry-sym 'menu)))
                 result nil
               )
             )
            )
            ((symbolp entry)
             (setq entry-name (symbol-name entry))
             (setq entry-sym  (lse_fill-in:definition entry-name))
             (setq result (cons (cons entry-name entry-sym) result))
            )
            ((stringp entry)
             (setq result (cons (cons entry "") result))
            )
            ((consp   entry)
             (setq result (cons entry result))
            )
            (t
             (error "Invalid menu entry %s" entry)
            )
      )
    )
    (reverse result)
  )
; lse_expand_menu:entry_list
)

(defun lse_expand_menu (psym name the-entries)
  (let (choice
        result
        (entries (lse_expand_menu:entry_list the-entries))
        (starter (or (get psym 'last-menu-entry) "")); 27-Feb-1995
       )
    (setq choice; 27-Feb-1995 starter added
          (lse-complete "" entries (not (get psym 'sort)) nil t starter t)
    ); 30-Sep-1994 force completion (otherwise empty menu entries don't work)
    (if choice
        (if (not (setq result (cdr (assoc choice entries))))
            (error "undefined menu entry : `%s` not in [%s]" choice the-entries)
          (progn
            (cond ((symbolp result)
                   (setq result (lse_fill-in:definition (symbol-name result)))
                   (put psym 'last-menu-entry (symbol-name result));27-Feb-1995
                  )
                  (t
                   (setq result choice)
                   (put psym 'last-menu-entry result); 27-Feb-1995
                  )
            )
          )
        )
    )
    result
  )
; lse_expand_menu
)

;;;;++
;;;; First internal level for token expansion
;;;;--
(defun lse_replace_token_by_expansion
            (token-given token-head token-tail expansion)
  (let (result)
    (if expansion
        (let* ((token-sym   (intern-soft     expansion lse::token-table))
               (token-val   (symbol-value    token-sym))
               (token-fct   (symbol-function token-sym))
               (token-range (lse-range:new   token-head token-tail))
               (inner-replacement lse_replaced_fill-in)
              )
          (if token-fct
              (progn
                (setq result
                      (funcall token-fct
                         (lse_fill-in:definition token-val) expansion
                         token-head token-tail
                         inner-replacement
                      )
                )
                (if inner-replacement
                    (lse_shut_fill-in_replacement)
                )
              )
            (if inner-replacement (lse_shut_fill-in_replacement token-given))
            (delete-region token-head token-tail)
            (setq result t)
            (if (stringp token-val)
                (lse-fill-in-insert token-val)
              (if (consp token-val)
                  (eval token-val)
                (lse-message "Token `%s' has invalid definition: `%s'"
                             token-given token-val
                )
                (setq result nil)
              )
            )
            (if result                           ; 12-Jun-1994 allow unexpansion
                (lse_fill-in_history:add_expansion ; of token-given
                     (lse-fill-in:new
                          token-sym (symbol-name token-sym)
                          'lse::flat 'lse::expanded
                          token-range nil token-given
                          nil nil nil nil
                     )
                )
            )
          )
        )
      (setq result "")
    )
    result
  )
; lse_replace_token_by_expansion
)

(defun lse_expand_token:make_entries (e)
  (let ((tsym (intern (downcase e) lse::token-table)))
    (if (symbol-function tsym)
        (cons e (lse_fill-in:definition (symbol-value tsym)))
      (cons e (format "%s" (symbol-value tsym)))
    )
  )
)

;;; 21-Jan-2011
(defun lse_expand_token:try-1 (token expand-fct bt et)
  (let (matching-tokens expansion result lse::expansion-separator)
    (setq matching-tokens (all-completions token lse::token-table))
    (cond ((eq (length matching-tokens) 1)
           (setq expansion (car matching-tokens))
          )
          ((> (length matching-tokens) 1); partial match
           (setq result t);  3-Oct-1996
           (setq expansion
                 (lse-complete token
                    (mapcar 'lse_expand_token:make_entries matching-tokens)
                    nil nil nil nil t
                 )
           )
          )
    )
    (if expansion
        (if expand-fct
            (setq result (funcall expand-fct token bt et expansion))
          (setq result
            (lse_replace_token_by_expansion token bt et expansion)
          )
        )
    )
    result
  )
; lse_expand_token:try-1
)
(defun lse_expand_token:try (token-delims word-count &optional expand-fct)
  (let* ((lse-tpu:word-chars token-delims)
         (bt (lse-tpu:prev-word-head-pos word-count (lse-tpu:line-head-pos)))
         (et (point))
         (simple-token (downcase (buffer-substring-no-properties bt et)))
         token
        )
    (if (or
          (bobp)
          (not (> et bt))
          (save-excursion (skip-chars-backward " \t") (and (bolp) (setq bt et))
          );  4-May-2007
        )
        (setq token "\n"); 27-Apr-2003
      (setq token (downcase (buffer-substring-no-properties bt et)))
    )
    (if (lse_expand_token:try-1 token expand-fct bt et)
        t
      (if (and (stringp simple-token) (not (string= token simple-token)))
          (lse_expand_token:try-1 simple-token expand-fct bt et)
        nil
      )
    )
  )
; lse_expand_token:try
)

;;;
;;; lse_replaced_fill-in: fill-in-info describing pending replacement
;;;
(defvar                      lse_replaced_fill-in    nil)
(make-variable-buffer-local 'lse_replaced_fill-in)
;;;
;;; lse-fill-in:currently-replaced: string used for mode line while
;;; replacement is pending
;;;
(defvar                      lse-fill-in:currently-replaced)
(make-variable-buffer-local 'lse-fill-in:currently-replaced)

;;;
;;; install fill-in-replacement as a minor mode
;;;
(or (assq 'lse_replaced_fill-in minor-mode-alist)
    (lse-add-to-list
      minor-mode-alist
      '(:propertize
         (lse_replaced_fill-in lse-fill-in:currently-replaced)
         face lse-face:open-replacement
       )
    )
)

;;;;++
;;;; Internals for replacement
;;;;--
(defun lse-fill-in:shut-replacement (&optional quiet by-replacement lazy)
  ;; 19-Aug-1995 optional parameter `lazy' added
  (let ((lse_current_fill-in                   lse_replaced_fill-in)
        (range       (lse-fill-in:range        lse_replaced_fill-in))
        (inner-range (lse-fill-in:inner-range  lse_replaced_fill-in))
        (psym        (lse-fill-in:symbol       lse_replaced_fill-in))
        replaced-by-msg
       )
    (if (and nil; 29-Mar-1995 ; don't care about empty replacements
             (not by-replacement)
             (or (lse-range:is-collapsed inner-range)
                 (lse-range:is-empty     inner-range)
             )
        )
        (if (or (equal (point-marker) (lse-range:head-pos range))
                (lse-range:inside range)
            )
            (progn
              (lse-range:clean range)
              (lse-fill-in-insert
                  (lse-fill-in:complement lse_replaced_fill-in)
              )
              (setq lse_replaced_fill-in nil)
              (lse-goto-prev-fill-in)
            )
        )
      ;; range of replacement is neither empty nor collapsed
      (lse-flat-fill-in:close-replacement psym nil lazy); 19-Aug-1995 lazy
      (if by-replacement
          (setq replaced-by-msg by-replacement)
        (if (< (lse-range:length range) 46)
            (setq replaced-by-msg
                  (buffer-substring (lse-range:head-pos range)
                                    (lse-range:tail-pos range)
                  )
            )
          (setq replaced-by-msg
                (concat (buffer-substring
                                (lse-range:head-pos range)
                             (+ (lse-range:head-pos range) 20)
                        )
                        "..."
                        (buffer-substring
                             (- (lse-range:tail-pos range) 20)
                                (lse-range:tail-pos range)
                        )
                )
          )
        )
      )
      (if (and nil (not quiet)); 24-Nov-1995 `(and nil)' added
          (message "Fill-In `%s' replaced by `%s'"
                   (lse-fill-in:symbol lse_replaced_fill-in) replaced-by-msg
          )
      )
      (setq lse_replaced_fill-in nil)
    )
    (lse-tpu:update-mode-line)
  )
)

(defun lse_shut_fill-in_replacement (&optional by-replacement)
  (if lse_replaced_fill-in
      (lse-fill-in:shut-replacement nil by-replacement)
  )
)

(defun lse_shut_fill-in_replacement/if_outside ()
  (if (and lse_replaced_fill-in
           (not (lse-range:inside (lse-fill-in:range lse_replaced_fill-in)))
      )
      (lse-fill-in:shut-replacement)
  )
)

;;;;++
;;;; internals for replacement
;;;;--
(defun lse_open_fill-in_replacement (name)
  ;; name is the fill-in's name to be replaced
  ;; `name' must be the lse_current_fill-in
  (let ((psym (lse-fill-in:symbol lse_current_fill-in))
       )
    (setq lse_replaced_fill-in
          (lse-flat-fill-in:open-replacement psym name 'lse::replaced)
    )
    (setq lse-fill-in:currently-replaced (concat " «" name "»"))
    (lse-tpu:update-mode-line)
    (setq lse_current_fill-in nil)
  )
)

;;;
;;; start replacement of current fill-in, if any
;;;
(defvar lse-flat-fill-in:expansion-in-progress nil)

(defun lse_start_replacement_if_in_fill-in ()
  (or lse-flat-fill-in:expansion-in-progress
      (if lse::active-in-buffer
          (or lse_replaced_fill-in
              (let ((name (lse_inside_fill-in)))
                 (if name
                     (if (lse-replacement@allowed)
                         (progn
                           (if (lse-replacement@auto-expand)
                               (lse-expand)
                             (lse_open_fill-in_replacement name)
                           )
                           t
                         )
                       (error
                         "Fill-In `%s' cannot be replaced, it must be expanded"
                         name
                       )
                     )
                 )
              )
          )
      )
  )
)

(defun lse-replacement@allowed ()
  ;; this functions assumes that current position is inside a flat fill-in
  (let (result)
    (save-excursion
      (skip-chars-backward (concat "^" lse_fill-in_head_delim_chars))
      (lse-tpu:forward-char -1)
      (setq result (not (looking-at lse_no_replacement_fill-in_marker)))
    )
    result
  )
)

;;; 17-Aug-2000
(defun lse-replacement@auto-expand ()
  (let ((psym (lse-fill-in:symbol lse_current_fill-in))
       )
    (get psym 'auto-expand)
  )
; lse-replacement@auto-expand
)

;;; 26-Feb-1995
(defun lse-auto-replicate-future-fill-in (name auto-replicate)
  ;; the following code is a mixture of lse_replicate_fill_in and
  ;; lse-flat-fill-in:auto-replicate
  (let ((LSE_CURRENT_FILL-IN lse_current_fill-in)
        (replica
          (or
            (assq (lse_fill-in:definition name) lse_fill-in_history/expansion)
            (lse-fill-in:new ; just in case there was no previous expansion
                (lse_fill-in:definition name) name 'lse::deep 'lse::expanded
                (lse-range:new (point) (point))
            )
          )
        )
        (descendants (lse-fill-in:descendants lse_current_fill-in))
        (lse-flat-fill-in:no-vanguard      t)
        (lse-flat-fill-in:auto-replicating t)
        lse_replaced_fill-in                 ; avoid infinite recursion
        current-fill-in
       )
    (save-excursion
      (while (and (> auto-replicate 0)
                  (lse-goto-next-fill-in t name)
             )
        (lse_open_fill-in_replacement name)
        (lse-fill-in-insert (lse-range:contents-np (lse-fill-in:range replica)))
        (lse-add-to-list descendants lse_replaced_fill-in)
        (lse-fill-in:shut-replacement t "")
        (setq auto-replicate (1- auto-replicate))
      )
      (lse-fill-in:change-descendants LSE_CURRENT_FILL-IN descendants)
      ;; restore lse_current_fill-in
      (setq lse_current_fill-in LSE_CURRENT_FILL-IN)
    )
  )
; lse-auto-replicate-future-fill-in
)

;;; 12-Jun-1994
(defun lse-flat-fill-in:auto-replicate (psym replica)
  (let ((auto-replicate (get psym 'auto-replicate))
        (lse-flat-fill-in:no-vanguard t); 26-Jun-1994
       )
    (if (integerp auto-replicate)
        (let ((LSE_CURRENT_FILL-IN lse_current_fill-in)
              (name (symbol-name psym))
              lse_replaced_fill-in          ; avoid infinite recursion
              descendants current-fill-in
             )
          (save-excursion
            (while (and (> auto-replicate 0)
                        (lse-goto-next-fill-in t name)
                   )
              (lse-fill-in:change-complement lse_current_fill-in replica)
              (setq current-fill-in
                    (lse-fill-in:toggle-expansion lse_current_fill-in t)
              )
              ; 26-Feb-1995 ; (lse_fill-in_history:remove_last_expansion)
              (lse-add-to-list descendants current-fill-in)
              (setq auto-replicate (1- auto-replicate))
            )
            (lse-fill-in:change-descendants LSE_CURRENT_FILL-IN descendants)
            ;; restore lse_current_fill-in
            (setq lse_current_fill-in LSE_CURRENT_FILL-IN)
          )
        )
    )
  )
; lse-flat-fill-in:auto-replicate
)

;;; 10-Mar-1996
(defun lse_replicate_fill_in_menu_entries (replicas)
  (let (result
        entry
       )
    (while replicas
      (setq entry    (car replicas))
      (setq replicas (cdr replicas))
      (add-to-list 'result
                   (cons (lse-range:contents-np (lse-fill-in:range entry)) "")
      )
    )
    result
  )
; lse_replicate_fill_in_menu_entries
)

;;; 10-Mar-1996
(defun lse_replicate_fill_in_menu ()
  (let ((name (lse_inside_fill-in))
        (lse-flat-fill-in:no-vanguard      t); 26-Jun-1994
        (lse-flat-fill-in:auto-replicating t); 26-Feb-1995
        replicas
        replica
        cp                              ; 18-Jan-1998
       )
    (if (not name)
        (error "Not inside fill-in")
      (save-window-excursion            ; 18-Jan-1998
        (setq replicas
              (lse_fill-in_history:instances
                  (lse-fill-in:symbol lse_current_fill-in)
                  lse_fill-in_history/expansion
              )
        )
        (if (not replicas)
            (error "No previous expansion found for fill-in `%s'" name)
          (let ((entries (lse_replicate_fill_in_menu_entries replicas))
               )
            (setq replica (lse-complete "" entries nil nil nil nil t))
          )
          (if (not replica)
              t
            (lse_open_fill-in_replacement name)
            (lse-fill-in-insert replica)
            (setq cp (point))
          )
        )
      ); save-window-excursion          ; 18-Jan-1998
      (and cp (goto-char cp))           ; 18-Jan-1998
    )
  )
; lse_replicate_fill_in_menu
)

(defun lse_replicate_fill_in ()
  (let ((name (lse_inside_fill-in))
        (lse-flat-fill-in:no-vanguard      t); 26-Jun-1994
        (lse-flat-fill-in:auto-replicating t); 26-Feb-1995
        replica
       )
    (if (not name)
        (error "Not inside fill-in")
      (setq replica
            (assq (lse_fill-in:definition name) lse_fill-in_history/expansion)
      )
      (if (not replica)
          (error "No previous expansion found for fill-in `%s'" name)
        (lse_open_fill-in_replacement name)
        (lse-fill-in-insert (lse-range:contents-np (lse-fill-in:range replica)))
        (lse-fill-in:change-ancestor
            lse_replaced_fill-in
            (lse-range:head-pos (lse-fill-in:range replica))
        )
      )
    )
  )
; lse_replicate_fill_in
)

(defun lse_replicate_fill_in_by_older ()
  (let* ((current-fill-in lse_replaced_fill-in)
         (range           (lse-fill-in:range current-fill-in))
         (contents        (lse-range:contents-np
                             (lse-fill-in:range current-fill-in)
                          )
         )
         (ancestor        (lse-fill-in:ancestor current-fill-in))
        )
    (if (not (markerp ancestor))
        (error "Last fill-in was not replicated")
      (let ((replica  (lse_fill-in_history:assq
                           (lse-fill-in:symbol current-fill-in)
                           lse_fill-in_history/expansion
                           ancestor
                       )
            )
           )
        (while (and replica
                    (equal (lse-range:contents-np (lse-fill-in:range replica))
                           contents
                    )
               )
          (setq replica (lse_fill-in_history:assq
                           (lse-fill-in:symbol current-fill-in)
                           lse_fill-in_history/expansion
                           (lse-range:head-pos (lse-fill-in:range replica))
                       )
          )
        )
        (if (not replica)
            (error "No previous expansion found for fill-in: %s"
                   (symbol-name (lse-fill-in:symbol current-fill-in))
            )
          (lse-range:clean    range)
          (goto-char          (lse-range:head-pos range))
          (lse-fill-in-insert
              (lse-range:contents-np (lse-fill-in:range replica))
          )
          (lse-fill-in:change-ancestor
              current-fill-in (lse-range:head-pos (lse-fill-in:range replica))
          )
        )
      )
    )
  )
; lse_replicate_fill_in_by_older
)

;;; 24-Feb-1995
;;;  2-Mar-1995 optional argument trailer
(defun lse-insert-replica (name &optional trailer)
  (if (not (stringp name))
      (error "Error in definition of currently expanded fill-in")
    (let ((replica (lse_fill-in_history:assq
                       (lse_fill-in:definition name)
                       lse_fill-in_history/expansion
                       nil                       ; 20-Mar-1995
                       (list 'lse::deep 'lse::dead); 20-Mar-1995
                   )
          )
          (lse-flat-fill-in:no-vanguard      t)
          (lse-flat-fill-in:auto-replicating t)
         )
      (if replica
          (if (eq (lse-fill-in:state replica) 'lse::deep); 20-Mar-1995
              (condition-case nil; 21-Mar-1995 protect against errors
                  (progn
                    (lse-fill-in-insert
                      (lse-range:contents-np (lse-fill-in:range replica))
                    )
                    (if (stringp trailer)
                        (lse-fill-in-insert trailer)
                    )
                  )
                (error           ; 21-Mar-1995 protect against errors
                  (setq replica nil)
                )
              )
          )
      )
      replica
    )
  )
; lse-insert-replica
)

;;; 22-Sep-1994
(defun lse-insert-replica-or-flat (name &optional head_delim tail_delim)
  (if (lse-insert-replica name); 24-Feb-1995 factored out
      t
    (lse-fill-in-insert
       (concat (or head_delim lse_req_fill-in_head_delim)
               name
               (or tail_delim lse_req_fill-in_tail_delim)
       )
    )
  )
; lse-insert-replica-or-flat
)

;;; 24-Sep-1994
(defun lse-insert-replica-or-flat-opt (name)
  (lse-insert-replica-or-flat name lse_opt_fill-in_head_delim
                                   lse_opt_fill-in_tail_delim
  )
; lse-insert-replica-or-flat-opt
)

;;; 16-Feb-1995
(defun lse-expand-future-fill-in (name how-many)
  (save-excursion
    (let (lse_current_fill-in
         )
      (while (and (> how-many 0)
                  (lse-goto-next-fill-in t name)
             )
        (lse-flat-fill-in:expand
           (lse-fill-in:symbol lse_current_fill-in) name
        )
        (setq how-many (1- how-many))
      )
    )
  )
; lse-expand-future-fill-in
)

;;;; 19-Mar-1995
;;;;+
;;;; Text-properties attached to flat fill-ins
;;;;-

;;;  4-Oct-2002
(defun lse-insert-property-value (name &optional default)
  (let ((value (get-text-property (point) name)))
    (if value
        (insert value)
      (if default (insert default))
    )
  )
; lse-insert-property-value
)

;;; 19-Mar-1995
(defun lse-flat-fill-in:open-line ()
  (interactive "*")
  (if (lse_inside_fill-in)
    (let ((cp (point-marker)))
      (or (bolp) (lse-tpu:next-beginning-of-line 1))
      (if (bobp)
          (progn
            (lse-fill-in:remove-text-properties
              (lse-range:head-pos (lse-fill-in:range lse_current_fill-in))
              (lse-range:tail-pos (lse-fill-in:range lse_current_fill-in))
              lse-flat-fill-in:flat-properties
            )
            (insert-before-markers "\n")
          )
        (lse-tpu:backward-char 1)
        (lse-split-line)
      )
      (goto-char (marker-position cp))
      (setq cp nil)
    )
  )
; lse-flat-fill-in:open-line
)

;;; 18-Jan-1998
(defun lse-flat-fill-in:remove-leading-whitespace ()
  "Removes whitespace in front of flat fill-in."
  (interactive "*")
  (if (lse_inside_fill-in)
    (let ((cp (point-marker))
          (hb (lse-tpu:line-head-pos 0))
          (tb (lse-range:head-pos (lse-fill-in:range lse_current_fill-in)))
         )
      (goto-char tb)
      (if (< (skip-chars-backward " \t\n" hb) -1)
          (delete-region (1+ (point)) tb)
      )
      (goto-char (marker-position cp))
      (setq cp nil)
    )
  )
; lse-flat-fill-in:remove-leading-whitespace
)

;;; 18-Jan-1998
(defun lse-flat-fill-in:remove-trailing-whitespace ()
  "Removes whitespace following flat fill-in."
  (interactive "*")
  (if (lse_inside_fill-in)
    (let ((cp (point-marker))
          (hb (lse-range:tail-pos (lse-fill-in:range lse_current_fill-in)))
          (tb (lse-tpu:line-tail-pos 2))
         )
      (goto-char hb)
      (if (> (skip-chars-forward " \t\n" tb) 1)
          (delete-region hb (1- (point)))
      )
      (goto-char (marker-position cp))
      (setq cp nil)
    )
  )
; lse-flat-fill-in:remove-trailing-whitespace
)

;;;  5-Jan-1998
(defun lse-flat-fill-in:remove-next-blank-line ()
  (interactive "*")
  (if (lse_inside_fill-in)
    (lse-remove-next-blank-lines); 20-Mar-2003 ; factored
  )
; lse-flat-fill-in:remove-next-blank-line
)

;;;  5-Jan-1998
(defun lse-flat-fill-in:remove-prev-blank-line ()
  (interactive "*")
  (if (lse_inside_fill-in)
    (lse-remove-prev-blank-lines); 20-Mar-2003 ; factored
  )
; lse-flat-fill-in:remove-prev-blank-line
)

(defun lse-flat-fill-in:align-to-word (&optional dir)
  (interactive "*p")
  (if (lse_inside_fill-in)
    (let ((cp (point-marker)))
      (lse-fill-in:remove-text-properties
        (lse-range:head-pos (lse-fill-in:range lse_current_fill-in))
        (lse-range:tail-pos (lse-fill-in:range lse_current_fill-in))
        lse-flat-fill-in:flat-properties
      )
      (goto-char (lse-range:head-pos (lse-fill-in:range lse_current_fill-in)))
      (lse-align-to-word dir t)
      (goto-char (marker-position cp))
      (setq cp nil)
    )
  )
; lse-flat-fill-in:align-to-word
)

(defun lse-flat-fill-in:align-to-next-word (&optional num)
  "Align position of next word with the position of the corresponding word of
next line"
  (interactive "*p")
  (lse-flat-fill-in:align-to-word num)
; lse-fill-in:align-to-next-word
)

(defun lse-flat-fill-in:align-to-previous-word (&optional num)
  (interactive "*p")
  (lse-flat-fill-in:align-to-word (- num))
; lse-fill-in:align-to-previous-word
)

;;; 13-Dec-1997
(defun lse-flat-fill-in:replace-and-delegate-key ()
  "Open replacement of current fill-in if any and delegate to normal key-binding.
This function is used as key-binding in `lse-flat-fill-in:keymap'
for all the keys which should automatically start replacement of the current
fill-in."
  (interactive "*")
  (if (lse_inside_fill-in)
      (lse-replace-fill-in)
    ;; if not inside fill-in remove text properties to avoid infinite regress
    (lse-fill-in:remove-text-properties (point) (1+ (point)) lse-flat-fill-in:flat-properties)
  )
  (setq this-command  (this-command-keys))
  (call-interactively (key-binding this-command))
  (if (lse_inside_fill-in)
      (lse-flat-fill-in:highlight-current)
  )
; lse-flat-fill-in:replace-and-delegate-key
)

;;; 15-Dec-1997
(defun lse-flat-fill-in:replace-and-mouse-yank (click)
  "Open replacement of current fill-in if any and yank mouse selection.
This function is used as key-binding in `lse-flat-fill-in:keymap' for
[mouse-2]."
  (interactive "e")
  (setq this-command (this-command-keys))
  (let ((primary (lse-tpu:mouse-paste:get-primary click))
       )
    (if (lse_inside_fill-in)
        (while (lse_inside_fill-in)
          (lse-replace-fill-in)
        )
      ;; if not inside fill-in remove text properties to avoid infinite regress
      (lse-fill-in:remove-text-properties
        (point) (1+ (point)) lse-flat-fill-in:flat-properties
      )
    )
    (lse-tpu:mouse-paste:insert primary)
  )
  (if (lse_inside_fill-in)
      (lse-flat-fill-in:highlight-current)
  )
; lse-flat-fill-in:replace-and-mouse-yank
)

;;; 13-Dec-1997
(defun lse-flat-fill-in:define-key (k &optional cmd)
  (define-key lse-flat-fill-in:keymap k
              (or cmd 'lse-flat-fill-in:replace-and-delegate-key)
  )
; lse-flat-fill-in:define-key
)

;;; 13-Dec-1997
(defun lse-flat-fill-in:define-keys (&rest arg)
  (mapc
    (function
      (lambda (cmd)
        (lse-iterate-keys-bound-to-function cmd 'lse-flat-fill-in:define-key)
      )
    )
    arg
  )
; lse-flat-fill-in:define-keys
)

(if (boundp 'lse-flat-fill-in:keymap)
    t
  (setq lse-flat-fill-in:keymap (make-keymap))
)

;;; 13-Oct-1996
(setplist 'lse-flat-fill-in:terminal
    (list 'local-map             lse-flat-fill-in:keymap
          'face                  'lse-face:current-fill-in-terminal
    )
)

(setplist 'lse-flat-fill-in:replacement
    (list 'local-map             lse-flat-fill-in:keymap
          'face                  'lse-face:current-fill-in-replacement
    )
)

(setplist 'lse-flat-fill-in:menu
    (list 'local-map             lse-flat-fill-in:keymap
          'face                  'lse-face:current-fill-in-menu
    )
)

(setplist 'lse-flat-fill-in:function
    (list 'local-map             lse-flat-fill-in:keymap
          'face                  'lse-face:current-fill-in-function
    )
)

;;; 13-Oct-1996
(defun lse-flat-fill-in:category (fill-in-type)
  (cond ((eq fill-in-type 'replacement)
         'lse-flat-fill-in:replacement
        )
        ((eq fill-in-type 'menu)
         'lse-flat-fill-in:menu
        )
        ((eq fill-in-type 'function)
         'lse-flat-fill-in:function
        )
        (t
         'lse-flat-fill-in:terminal
        )
  )
; lse-flat-fill-in:category
)

;;;; 25-Mar-1995 moved in here from lse-fill-in.el
;;;;
;;;;
;;;;
;;; 22-Mar-1995
(defvar                      lse-fill-in:current-overlay nil)
(make-variable-buffer-local 'lse-fill-in:current-overlay)

;;; 19-Mar-1995
(defun lse-flat-fill-in:highlight-current (&optional r-modifier)
  (if lse_current_fill-in
      (let* ((head-pos     (lse-range:head-pos
                                (lse-fill-in:range  lse_current_fill-in)
                           )
             )
             (tail-pos     (lse-range:tail-pos
                                (lse-fill-in:range  lse_current_fill-in)
                           )
             )
             (h-pos        (+ head-pos (or r-modifier 0))); 17-Dec-1996
             (t-pos        (- tail-pos (or r-modifier 0))); 17-Dec-1996
             (fill-in-type (get (lse-fill-in:symbol lse_current_fill-in) 'type))
             (inhibit-read-only t); 13-Dec-1997
            ); 22-Mar-1995
        (lse-fill-in:add-text-properties h-pos t-pos
             (list 'category (lse-flat-fill-in:category fill-in-type))
        )
        (lse-fill-in:add-text-properties
             (lse-range:head-pos (lse-fill-in:inner-range lse_current_fill-in))
             (lse-range:tail-pos (lse-fill-in:inner-range lse_current_fill-in))
             (list 'intangible t); 28-Dec-1999
        )
        (and  lse-fill-in:current-overlay; 25-Mar-1995
              (delete-overlay lse-fill-in:current-overlay)
        )
        (setq lse-fill-in:current-overlay; 22-Mar-1995
              (make-overlay head-pos tail-pos)
        )
        (overlay-put lse-fill-in:current-overlay; 22-Mar-1995
                     'face 'lse-face:current-fill-in
        )
      )
  )
; lse-flat-fill-in:highlight-current
)

;;; 19-Mar-1995
(defun lse-flat-fill-in:unhighlight-current ()
  (let ((inhibit-read-only t); 13-Dec-1997
       )
    (and lse-fill-in:current-overlay; 22-Mar-1995
         (delete-overlay lse-fill-in:current-overlay)
    )
  )
; lse-flat-fill-in:unhighlight-current
)

;;; 13-Oct-1996
(defun lse-flat-fill-in:highlight-all (head tail)
  ;; highlights all flat fill-in's in range `HEAD' and `TAIL'
  (save-excursion
    (save-restriction
      (narrow-to-region head tail)
      (goto-char head)
      (let (lse_current_fill-in)
        (while (prog2
                 (lse_search_fill-in:forward)
                 (and (match-end 1)          ; 16-Oct-1996
                      (<= (match-end 1) tail); 16-Oct-1996
                      (setq lse_current_fill-in (lse-check-fill-in))
                 )
                 (lse-flat-fill-in:highlight-current 1); 17-Dec-1996 ` 1'
               )
        )
      )
    )
  )
; lse-flat-fill-in:highlight-all
)

;;; __END__ lse-flat-fill-in.el
