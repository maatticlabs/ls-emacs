;-*- coding: iso-8859-15; -*-

;;;;unix_ms_filename_correspondency lse-interactive:el lse_intv:el
;;;; Copyright (C) 1994-2012 Mag. Christian Tanzer. All rights reserved.
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
;;;;    lse-interactive
;;;;
;;;; Purpose
;;;;    Provides all interactive commands for emacs lse system
;;;;
;;;; Revision Dates
;;;;    26-May-1994 (CT) Creation
;;;;    12-Jun-1994 (CT) Auto-replication added
;;;;    15-Jun-1994 (CT) Disable overwrite-mode during expansion
;;;;    26-Jun-1994 (CT) Set lse-flat-fill-in:no-vanguard while replacing
;;;;    27-Jun-1994 (CT) lse-flush-replacement added
;;;;     1-Aug-1994 (CT) lse_command:last added to lse-command:do
;;;;     8-Sep-1994 (CT) case-fold parameter passed to lse-complete
;;;;    13-Sep-1994 (CT) lse-language:fill-in-refs & lse-language:fill-in-defs
;;;;                     in lse-language:compile
;;;;    17-Sep-1994 (CT) Use lse-script-dir instead of literal path
;;;;    30-Sep-1994 (CT) Removed optional parameter `quiet' from lse-expand-token
;;;;    21-Oct-1994 (CT) lse-set-tab-increment defined
;;;;    22-Jan-1995 (CT) Moved body of lse-unkill-fill-in to lse-kill.el
;;;;    22-Jan-1995 (CT) Moved body of lse-replicate-fill-in and
;;;;                     lse-replicate-fill-in-by-older to lse-flat-fill-in.el
;;;;    22-Jan-1995 (CT) lse-replicate-fill-in chooses between
;;;;                     lse_replicate_fill_in and
;;;;                     lse_replicate_fill_in_by_older
;;;;    18-Feb-1995 (CT) kill-action implemented (in lse-kill-fill-in)
;;;;    20-Mar-1995 (CT) lse_language:last implemented (default menu selection)
;;;;    25-Mar-1995 (CT) lse-flush-replacement deletes
;;;;                     lse-flat-fill-in:replacement-overlay
;;;;    18-Apr-1995 (CT) Error corrected (token expansion)
;;;;    26-Jun-1995 (CT) lse-kill:dont-move added
;;;;    19-Aug-1995 (CT) Avoid replacement action when unreplacing a fill-in
;;;;    10-Mar-1996 (CT) lse-replicate-menu added
;;;;    30-Mar-1996 (CT) lse-lingo functions added
;;;;    11-Oct-1996 (CT) lse-goto-next-expansion and lse-goto-prev-expansion
;;;;                     added (along with lse-goto-@-expansion)
;;;;    13-Oct-1996 (CT) 'lse-deep-fill-in-id changed to 'lse-fill-in:id
;;;;    16-Oct-1996 (CT) lse-show-language added
;;;;    17-Oct-1996 (CT) lse-goto-parent-expansion-head added
;;;;    27-Mar-1997 (CT) lse-toggle-lse-split-line added
;;;;    14-Dec-1997 (CT) lse-kill-current-fill-in added
;;;;    16-Dec-1997 (CT) `(setq lse@current-expansion-indent nil)' added to
;;;;                     `lse-language:use'
;;;;    29-Dec-1997 (CT) `lse-version' added
;;;;    30-Dec-1997 (CT) `lse-set-tab-increment-i' added
;;;;    10-Jan-1998 (CT) Use [return] instead of [?\C-m]
;;;;     1-Jan-1999 (CT) `lse-kill-fill-in-join-sexp' added
;;;;     1-Jan-1999 (CT) `lse-kill-all-optional-fill-ins-line' added
;;;;     1-Jan-1999 (CT) `lse-replicate-fill-ins-line' added
;;;;    12-Jan-1999 (CT) Added `no-indent' to `lse-split-line'
;;;;                     `lse-split-line-i' added and used for key bindings
;;;;    25-May-1999 (CT) Write `;-*- unibyte: t; -*-' as first line of lsc-file
;;;;     2-Oct-2007 (CT) Added `coding: iso-8859-15;`  to first line of lsc-file
;;;;    11-Oct-2007 (CT) `lse-goto-first-fill-in` and `lse-goto-last-fill-in`
;;;;                     added
;;;;     7-Apr-2008 (CT) `lse-goto-first-fill-in` and `lse-goto-last-fill-in`
;;;;                     changed to remember `lse_last_position`
;;;;    10-Nov-2010 (CT) Use `mapc` instead of `mapcar` where appropriate
;;;;    28-Jan-2011 (CT) `unibyte` removed
;;;;    18-Feb-2012 (CT) Use `lse-tpu:put-prop:auto-save-position` instead of
;;;;                     home-grown code
;;;;    18-Feb-2012 (CT) Remove `lse-goto-last-position`,
;;;;                     use `lse-tpu:put-prop:auto-save-position`
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-interactive)

(eval-when-compile
  (require 'lse-basics)
  (require 'lse-list-util)

  (require 'lse-buffer)
  (require 'lse-buffer-list)
  (require 'lse-command)
  (require 'lse-comment)
  (require 'lse-compilation)
  (require 'lse-completion)
  (require 'lse-deep-fill-in)
  (require 'lse-define)
  (require 'lse-editing)
  (require 'lse-face)
  (require 'lse-file)
  (require 'lse-fill-in)
  (require 'lse-fill-in-info)
  (require 'lse-fill-in-history)
  (require 'lse-fill-in-marks); 29-Dec-1997
  (require 'lse-fill-in--delimiters)
  (require 'lse-fill-in--search)
  (require 'lse-flat-fill-in)
  (require 'lse-frame); 12-Aug-1996
  (require 'lse-indent)
  (require 'lse-keys)
  (require 'lse-kill)
  (require 'lse-language)
  (require 'lse-learn-key)
  (require 'lse-mark-stack)
  (require 'lse-menu); 16-Oct-1996
  (require 'lse-mode-alist)
  (require 'lse-range)
  (require 'lse-session)
  (require 'lse-tpu)
  (require 'lse-window)
  (require 'lse-tpu-keys)
)

;;; 29-Dec-1997
(defun lse-version nil
  "Print the ls-emacs version number."
  (interactive)
  (message "LS-Emacs version %s of %s by Christian Tanzer"
           lse-version lse-version-date
  )
; lse-version
)

;;;++
;;; user level commands for expansion
;;;--
(defun lse-expand-token ()
  "Expand the current token (word(s) before the cursor)."
  (interactive "*")
  (unwind-protect               ; 29-Dec-1997
      (let ((wcount 5)
            overwrite-mode      ; 15-Jun-94 ; don't overwrite during expansion
            result
            lse_current_fill-in ; 30-Sep-94 ; just in case this is called
                                ;             during expansion of a fill-in
            cp
           )
        ;; this save-*-excursion fuss is necessary to avoid change of
        ;; position in other windows showing the same buffer
        ;; (shame on emacs!!! at least I don't understand why this happens)
        (save-window-excursion            ;  7-Jan-1998
          (while (and (not result) (> wcount 0))
            (or (setq result
                   (lse_expand_token:try lse-tpu:blank-sep-word-chars wcount)
                )
                (setq result (lse_expand_token:try lse-tpu:word-chars wcount))
            )
            (setq wcount (1- wcount))
          )
          (setq cp (point))
        ); save-window-excursion
        (and cp (goto-char cp));  7-Jan-1998
        result
      ); let
    ;; any clean-up necessary goes here
  )
; lse-expand-token
)

(lse-tpu:put-prop:auto-save-position 'lse-expand-token)

(defun lse-expand ()
  "Expand fill-in (if inside) or token (if previous word is one). "
  ;; primary expansion function for user (this is bound to the appropriate key)
  (interactive "*")
  (unwind-protect               ; 29-Dec-1997
      (let ((name (lse_inside_fill-in))
            overwrite-mode  ; 15-Jun-1994 ; don't overwrite during expansion
            th tt
            p
           )
        ;; this save-*-excursion fuss is necessary to avoid change of
        ;; position in other windows showing the same buffer
        ;; (shame on emacs!!! at least I don't understand why this happens)
        (save-window-excursion
          (save-excursion
            (if (eobp)
                (save-excursion
                  (lse-fill-in-insert " \n")
                  (setq th (point-marker))
                  (lse-fill-in-insert
                       " \n"
                       (or lse_comment_head_delim "")
                       " end of buffer " (buffer-name (current-buffer))
                       (or lse_comment_tail_delim "")
                       "\n"
                  )
                  (setq tt (point-marker))
                )
            )
            (cond (name
                   (lse-flat-fill-in:expand
                       (lse-fill-in:symbol lse_current_fill-in) name
                   )
                  )
                  ((lse-expand-token)
                   (lse_inside_fill-in); 18-Apr-1995 ; otherwise
                                       ; lse_current_fill-in is sometimes wrong
                  )
                  (t (message "Nothing to expand"))
            )
            (setq p (point-marker))
            (and th tt (delete-region th tt))
            (setq th nil)
            (setq tt nil)
          ); save-excursion
        ); save-window-excursion
        (goto-char p)
        (setq p nil)
      ); let
    ;; any clean-up necessary goes here
  )
; lse-expand
)

(lse-tpu:put-prop:auto-save-position 'lse-expand)

;;;++
;;; user level commands for replacement
;;;--
(defun lse-replace-fill-in ()
  "Start or finish fill-in replacement."
  (interactive "*")
  (if lse_replaced_fill-in
      (lse_shut_fill-in_replacement)
    (lse-tpu:unselect); avoid surprises due to spurious selection
    (if (lse_start_replacement_if_in_fill-in)
        t
      (error "No fill-in to replace")
    )
  )
)

(lse-tpu:put-prop:auto-save-position 'lse-replace-fill-in)

(defun lse-flush-replacement ()
  "Flush pending replacement (use in emergencies)"
  (interactive)
  (setq lse_replaced_fill-in              nil)
  (setq lse-flat-fill-in:open-replacement nil)
  (and lse-flat-fill-in:replacement-overlay; 25-Mar-1995
       (delete-overlay lse-flat-fill-in:replacement-overlay)
  )
  (lse-tpu:update-mode-line)
; lse-flush-replacement
)
;;;++
;;; user level commands for fill-in replication
;;;--
(defun lse-replicate-fill-in ()
  "Replicate the value of the last occurence of the fill-in."
  (interactive "*")
  (if lse_replaced_fill-in             ; 22-Jan-1995
      (lse_replicate_fill_in_by_older) ; 22-Jan-1995
    (lse_replicate_fill_in)
  )
; lse-replicate-fill-in
)

(lse-tpu:put-prop:auto-save-position 'lse-replicate-fill-in)

(defun lse-replicate-fill-in-by-older ()
  "Replace current replication by next older one."
  (interactive "*")
  (lse_replicate_fill_in_by_older)
; lse-replicate-fill-in-by-older
)

(lse-tpu:put-prop:auto-save-position 'lse-replicate-fill-in-by-older)

;;; 10-Mar-1996
(defun lse-replicate-menu ()
  "Replace current fill-in by one of its previous expansions"
  (interactive "*")
  (lse_replicate_fill_in_menu)
; lse-replicate-menu
)

(lse-tpu:put-prop:auto-save-position 'lse-replicate-menu)

;;;  1-Jan-1999
(defun lse-replicate-fill-ins-line (&optional limit)
  "Replicate all fill-ins from point to `limit' (default: end of line)."
  (interactive "*")
  (or limit (setq limit (lse-tpu:line-tail-pos)))
  (while (and (lse-goto-next-fill-in t) (< (point) limit))
    (lse_replicate_fill_in)
  )
; lse-replicate-fill-ins-line
)

;;;++
;;; commands for help about fill-ins
;;;--
(defun lse-describe-fill-in ()
  "Display short help for current fill-in"
  (interactive)
  (let* ((name (lse_inside_fill-in))
         psym
        )
    (if name
        (or (lse_show_fill-in_description
               (lse-fill-in:symbol lse_current_fill-in)
            )
            (message "No description available for %s" name)
        )
      (message "Not inside fill-in")
    )
  )
; lse-describe-fill-in
)

(defun lse-help-fill-in ()
  "Display help for current fill-in"
  (interactive)
  (let* ((name (lse_inside_fill-in))
         psym
        )
    (if name
        (or (lse_show_fill-in_help (lse-fill-in:symbol lse_current_fill-in))
            (message "No help available for %s" name)
        )
      (message "Not inside fill-in")
    )
  )
; lse-help-fill-in
)

;;;++
;;; user level commands for un/re-expansion
;;;--
(defun lse-unexpand-fill-in ()
  "Undo last expansion of fill-in. This command can be repeated."
  (interactive "*")
  (if lse_replaced_fill-in
      (error "Cannot unexpand while currently replacing a fill-in")
    (lse_unfill_fill-in "un-expand")
  )
)

(defun lse-reexpand-fill-in ()
  "Redo last unexpansion. This command can be repeated."
  (interactive "*")
  (if lse_replaced_fill-in
      (error "Cannot reexpand while currently replacing a fill-in")
    (let ((last_fill-in (lse_fill-in_history:last_unexpansion))
         )
      (if (not last_fill-in)
          (error "No fill-in to re-expand")
        (lse_fill-in_history:remove_last_unexpansion)
        (lse_fill-in_history:add_expansion
             (lse_toggle_fill-in_expansion last_fill-in)
        )
        (save-excursion                         ; 12-Jun-1994 auto-replication
          (mapc 'lse_toggle_fill-in_expansion
            (lse-fill-in:descendants last_fill-in)
          )
        )
      )
    )
  )
)

;;;++
;;; user level commands for un/re-replacement
;;;--
(defun lse-unreplace-fill-in ()
  "Cancel active replacement or undo last replacement."
  (interactive "*")
  (if lse_replaced_fill-in
      (lse@shut_fill-in_replacement t nil t); 19-Aug-1995 `nil t' added
  )
  (lse_unfill_fill-in "un-replace")
  (lse-flush-replacement)
)

(defun lse-rereplace-fill-in ()
  "Redo cancelled/undone replacement."
  (interactive "*")
  (let ((last_fill-in (lse_fill-in_history:last_unexpansion))
       )
    (if (or (not last_fill-in)
            (not (eq (lse-fill-in:fill-type last_fill-in) 'lse@replaced))
        )
        (error "No fill-in to re-replace")
      (lse_fill-in_history:remove_last_unexpansion)
      (setq lse_replaced_fill-in (lse_toggle_fill-in_expansion last_fill-in))
    )
  )
)

;;;+
;;; Commands for killing and unkilling fill-ins
;;;-
(defun lse-kill-all-optional-fill-ins (&optional limit)
  "Kill all optional fill-in from point to `limit' (default: end of buffer)."
  (interactive "*")
  (or limit (setq limit (point-max)))
  (if (lse_inside_fill-in)
      (lse_kill_current_fill-in_if_optional)
  )
  (while (and (lse-goto-next-fill-in t) (< (point) limit))
    (lse_kill_current_fill-in_if_optional)
  )
; lse-kill-all-optional-fill-ins
)

;;;  1-Jan-1999
(defun lse-kill-all-optional-fill-ins-line ()
  "Kill all optional fill-ins from point to end-of-line"
  (interactive "*")
  (lse-kill-all-optional-fill-ins (lse-tpu:line-tail-pos))
; lse-kill-all-optional-fill-ins-line
)

(defvar lse-kill:tail-delim-pattern
        (concat "[" lse_fill-in_tail_delim_chars "]")
)

;;; 26-Jun-1995
(defvar lse-kill:dont-move nil)

(defun lse-kill-fill-in (&optional dont-move join-sexp)
  "Kill the next fill-in"
  (interactive "*P")
  (if (or (and (lse_inside_fill-in)
               (not (looking-at lse-kill:tail-delim-pattern))
          )
          (lse-goto-next-fill-in)
      )
      (let* ((psym    (lse-fill-in:symbol      lse_current_fill-in))
             (kaction (lse-fill-in:kill-action psym))
            )
        (lse_kill_current_fill-in)
        (if kaction         ; 18-Feb-1995
            (save-excursion ; 18-Feb-1995
              (lse-flat-fill-in:interpret-replacement_in_env kaction);18-Feb-95
            )
        )
        (if join-sexp (lse-join-sexp-boundary-maybe)); 1-Jan-1999
      )
  )
  (if (not (eq (not dont-move) (not lse-kill:dont-move))); 26-Jun-1995
      t
    (lse-goto-next-fill-in t)
  )
; lse-kill-fill-in
)

(lse-tpu:put-prop:auto-save-position 'lse-kill-fill-in)

;;;  1-Jan-1999
(defun lse-kill-fill-in-join-sexp (&optional dont-move)
  "Kill the next fill-in and lse-join-sexp-boundary-maybe"
  (interactive "*P")
  (lse-kill-fill-in dont-move t)
; lse-kill-fill-in-join-sexp
)

;;; 14-Dec-1997
(defun lse-kill-current-fill-in (&optional dont-move)
  "Kill the current fill-in if any"
  (interactive "*P")
  (if (and (lse_inside_fill-in)
               (not (looking-at lse-kill:tail-delim-pattern))
      )
      (lse-kill-fill-in dont-move)
    (lse-flat-fill-in:replace-and-delegate-key)
  )
; lse-kill-current-fill-in
)

(lse-tpu:put-prop:auto-save-position 'lse-kill-current-fill-in)

(defun lse-unkill-fill-in ()
  "Unkill the fill-in which was killed last."
  (interactive "*")
  (lse_unkill_fill_in)
; lse-unkill-fill-in
)

;;;+
;;; Interface to emacs and non-lse commands
;;;-
(defun lse-command:do (&optional cmd)
  "Display menu of commands and execute selection made by user."
  (interactive)
  (let (result
       )
    (let ((lse_completion_buffer lse_command:completion_buffer)
         )
      (if lse_command:initialized
          t
        (save-excursion
          (set-buffer (lse_completion:buffer "LSE command"))
          (lse_completion:show "" lse_command:cmd_list nil)
          (setq lse_command:completion_buffer (current-buffer))
        )
        (setq lse_command:initialized t)
      )
      (setq result
        (lse-complete cmd lse_command:cmd_list nil t nil lse_command:last t)
      )
      (and result (setq lse_command:last result))
    )
    (if result
        (let ((binding (cdr (assoc result lse_command:cmd_list)))
             )
          (cond ((commandp binding) (call-interactively binding))
                ((symbolp binding)  (funcall binding))
                ((consp binding)    (eval binding))
                (t (error "Command %s has invalid binding: %s" result binding))
          )
        )
    )
  )
)

;;;+
;;; Commands for moving to fill-ins
;;;-
;;; 11-Oct-2007
(defun lse-goto-first-fill-in (&optional quiet name)
  "Move point to first flat fill-in in buffer."
  (interactive "P")
  (let ((last-pos (point-marker)))
    (lse-tpu:move-to-beginning)
    (lse-goto-next-fill-in quiet name)
  )
; lse-goto-first-fill-in
)

(lse-tpu:put-prop:auto-save-position 'lse-goto-first-fill-in)

(defun lse-goto-last-fill-in (&optional quiet name)
  "Move point to last flat fill-in in buffer."
  (interactive "P")
  (let ((last-pos (point-marker)))
    (lse-tpu:move-to-end)
    (lse-goto-prev-fill-in quiet name)
  )
; lse-goto-last-fill-in
)

(lse-tpu:put-prop:auto-save-position 'lse-goto-last-fill-in)

(defun lse-goto-next-fill-in (&optional quiet name)
  "Move point to next flat fill-in."
  (interactive "P")
  (if (eobp)
      nil
    (lse-goto-@-fill-in 'lse_search_fill-in:forward 'eobp quiet name)
  )
)

(lse-tpu:put-prop:auto-save-position 'lse-goto-next-fill-in)

(defun lse-goto-prev-fill-in (&optional quiet name)
  "Move point to previous flat fill-in."
  (interactive "P")
  (if (bobp)
      nil
    (let ((cp (point))
          result
         )
      (if (lse_inside_fill-in)
          (progn
            (goto-char
                 (lse-range:head-pos (lse-fill-in:range lse_current_fill-in))
            )
            (if (not (bobp)) (lse-tpu:forward-char -1))
          )
      )
      (setq result
            (lse-goto-@-fill-in 'lse_search_fill-in:backward 'bobp quiet name)
      )
      (if (or (bobp) (not result))
          (goto-char cp)
      )
      result
    )
  )
)

(lse-tpu:put-prop:auto-save-position 'lse-goto-prev-fill-in)

;;; 11-Oct-1996
(defun lse-goto-@-expansion (search-fct prop-sym)
  ;; search-fct has to be either next-single-property-change or
  ;;                             previous-single-property-change
  (let* ((cp (point))
         (np (funcall search-fct cp prop-sym))
        )
    (if np
        (progn
          (goto-char  np)
          np
        )
    )
  )
; lse-goto-@-expansion
)

;;; 11-Oct-1996
(defun lse-goto-next-expansion ()
  "Goto next expansion of fill-in in buffer."
  (interactive)
  (lse-goto-@-expansion 'next-single-property-change 'lse-fill-in:id)
; lse-goto-next-expansion
)

;;; 11-Oct-1996
(defun lse-goto-prev-expansion ()
  "Goto previous expansion of fill-in in buffer."
  (interactive)
  (lse-goto-@-expansion 'previous-single-property-change 'lse-fill-in:id)
; lse-goto-next-expansion
)

;;; 17-Oct-1996
(defun lse-goto-parent-expansion-head ()
  "Goto head of expansion of parent fill-in."
  (interactive)
  (let ((pid (or (get-text-property (point) 'lse-fill-in:parent-id)
                 (and lse_replaced_fill-in
                      (lse-fill-in:parent-id lse_replaced_fill-in)
                 )
             )
        )
        (cp (point))
        np
       )
    (if pid
        (progn
          (setq np
                (text-property-any (point-min) cp 'lse-fill-in:id pid)
          )
          (if np
              (progn
                (goto-char  np)
                np
              )
          )
        )
    )
  )
; lse-goto-parent-expansion-head
)

(lse-tpu:put-prop:auto-save-position 'lse-goto-parent-expansion-head)

;;;++
;;; lse-split-line: handle comments properly
;;;--
(defun lse-split-line (&optional no-indent)
  "Split line. Takes care of comment leaders and trailers."
  (interactive "*")
  (if (bolp)
      (lse-fill-in-insert "\n"); (newline) not used to avoid marker confusion
    (if (integerp lse@current-expansion-indent)
        ;; expanding a fill-in
        (lse-newline-and-indent)
      ;; not expanding a fill-in: determine comment leading and trailer for
      ;; lse-newline-and-indent
      (let (lse@expansion-line-leading
            lse@expansion-line-leading-indent
            lse@expansion-line-trailer
            lse@expansion-line-trailer-indent
           )
        (lse_comment:setup_expansion_leading)
        (lse_comment:setup_expansion_trailer)
        (if no-indent                   ; 12-Jan-1999
            (lse-newline 1)             ; 12-Jan-1999
          (lse-newline-and-indent)
        )                               ; 12-Jan-1999
      )
    )
  )
; lse-split-line
)

(defun lse-split-line-i ()
  "Split line without adding indentation. Takes care of comment leaders and
trailers."
  (interactive "*")
  (lse-split-line 'no-indent)
; lse-split-line-i
)

;;; 27-Mar-1997
(defvar lse-split-line:old-key nil)

;;; 27-Mar-1997
(defun lse-toggle-lse-split-line nil
  "Toggle between 'newline and indent' and 'simple newline'."
  (interactive)
  (cond (lse-split-line:old-key
         (local-set-key [return]       lse-split-line:old-key)
         (local-set-key [blue return] 'lse-split-line-i); 12-Jan-1999
         (setq lse-split-line:old-key  nil)
        )
        (t
         (setq lse-split-line:old-key  (key-binding [return]))
         (local-set-key [return]      'lse-split-line-i); 12-Jan-1999
         (local-set-key [blue return]  lse-split-line:old-key)
        )
  )
  lse-split-line:old-key
; lse-toggle-lse-split-line
)

(defun lse-set-tab-increment (inc)
  "Sets tab increment to `inc'."
  (interactive "NTab increment: \n")
  (if (integerp inc)
      (progn
        (setq lse-language:tab-increment inc)
        (message (format "Tab increment of current buffer set to %d" inc))
      )
  )
; lse-set-tab-increment
)

;;; 30-Dec-1997
(defun lse-set-tab-increment-i ()
  "Sets tab increment to value of number key pressed."
  (interactive)
  (let* ((key (this-command-keys))
         (k   (if (vectorp key) (aref key (1- (length key))) key))
         (inc (- (logand k 255) ?0))
        )
    (lse-set-tab-increment inc)
  )
; lse-set-tab-increment-i
)

(defun lse-expand-or-tabulator ()
  "Expand fill-in if in any or execute lse-tabulator."
  (interactive "*")
  (if (lse_inside_fill-in)
      (lse-expand)
    (lse-tabulator)
  )
; lse-expand-or-tabulator
)

(defun lse-expand-or-goto-next ()
  "Expand fill-in if in any or goto next flat fill-in."
  (interactive "*")
  (if (lse_inside_fill-in)
      (lse-expand)
    (lse-goto-next-fill-in)
  )
; lse-expand-or-goto-next
)

;;;+
;;; Compilation and use of lse languages
;;;-
(defvar lse_language:last nil); 20-Mar-1995
(defun lse-language:read-name (&optional name)
  (or name
      (prog1
        (setq name                          ;; 20-Mar-1995 lse_language:last
           (lse-complete "" lse-language:table nil nil nil lse_language:last t)
        )
        (and name (setq lse_language:last name)); 20-Mar-1995
      )
      (if lse-emacs19-p
          (setq name (completing-read
                          "Language: " lse-language:table nil nil nil
                          'minibuffer-history
                     )
          )
        ;;        (setq name (completing-read-with-history-in
        ;;                        'minibuffer-history "Language: " lse-language:table
        ;;                   )
        ;;        )
      )
  )
  name
; lse-language:read-name
)

(defun lse-language:compile (&optional name)
  "Compile a LSE-language from source files."
  (interactive)
  (setq name (lse-language:read-name name))
  (let ((lse-language:@compiling t)
        (file-name (concat "lse-language-" name lse-language:compil_ext))
       )
    (save-window-excursion
      (set-buffer (get-buffer-create " $LSE-definition-log$"))
      (erase-buffer)
      (set-buffer (get-buffer-create " $LSE-fill-in:refs$"))
      (erase-buffer)
      (set-buffer (get-buffer-create " $LSE-fill-in:defs$"))
      (erase-buffer)

      (set-buffer (get-buffer-create file-name))
      (erase-buffer)
      (setq buffer-file-name   (concat lse-directory "/" file-name))
      (lse-language:use-loaded (lse-language:load-from-source name))
      (princ ";-*- coding: iso-8859-15; -*-" (current-buffer)); 28-Jan-2011
      (terpri                     (current-buffer)); 25-May-1999
      (mapatoms 'lse-compile@write-one-fill-in lse_fill-in_table)
      (mapatoms 'lse-compile@write-one-token   lse_token_table)
      (save-buffer nil)
      (mapc
        (function
          (lambda (x)
            (let ((b (get-buffer " $LSE-fill-in:defs$")))
              (princ x b)
              (terpri  b)
            )
          )
        )
        lse-language:fill-in-defs
      )
      (mapc
        (function
          (lambda (x)
            (let ((b (get-buffer " $LSE-fill-in:refs$")))
              (princ x b)
              (terpri  b)
            )
          )
        )
        lse-language:fill-in-refs
      )
    )
  )
  (lse-ring-bell)
; lse-language:compile
)

(defun lse-language:use (&optional name)
  "Use a specific LSE language for the current buffer."
  (interactive)
  (setq name (lse-language:read-name name))
  (let ((lsym (intern-soft (downcase name) lse-language:table))
       )
    (if (or (not lsym) (not (get lsym 'loaded)))
        (or (setq lsym (lse-language:load-compiled    name))
            (setq lsym (lse-language:load-from-source name))
            (error "Can't find language %s"           name)
        )
    )
    (lse-language:use-loaded lsym t)
  )
  (setq lse@current-expansion-indent nil)        ; 16-Dec-1997
  (setq-default lse@current-expansion-indent nil); 12-Jan-1998
; lse-language:use
)

(defun lse-language:reload (&optional name)
  "Reload a specific LSE language from sources."
  (interactive)
  (setq name (lse-language:read-name name))
  (let ((lsym (intern-soft (downcase name) lse-language:table))
        (lse-language:@compiling t)
       )
    (or (setq lsym (lse-language:load-compiled    name))
        (setq lsym (lse-language:load-from-source name))
        (error "Can't find language %s"           name)
    )
  )
  (setq lse@current-expansion-indent nil)        ; 16-Dec-1997
  (setq-default lse@current-expansion-indent nil); 12-Jan-1998
; lse-language:reload
)

;;; 13-Sep-1994
(defun lse-language:check (&optional name)
  "Check a LSE-language for references to undefined fill-ins."
  (interactive)
  (setq name (lse-language:read-name name))
  (lse-language:compile name)
  (save-excursion
    (set-buffer (get-buffer " $LSE-fill-in:refs$"))
    (write-region (point-min) (point-max) (concat "/tmp/" name ".refs") nil 1)
    (set-buffer (get-buffer " $LSE-fill-in:defs$"))
    (write-region (point-min) (point-max) (concat "/tmp/" name ".defs") nil 1)
  )
  (with-output-to-temp-buffer " $LSE-fill-in:undefined"
    (call-process
       (concat lse-script-dir "/check_lse_language")
       nil
       " $LSE-fill-in:undefined"
       t
       (concat "/tmp/" name ".defs")
       (concat "/tmp/" name ".refs")
       (concat lse-directory "/" "lse-language-" name lse-language:compil_ext)
    )
  )
; lse-language:check
)

;;; 16-Oct-1996
(defun lse-show-language ()
  "Show name of language used in current buffer."
  (interactive)
  (message "Language used is: %s" (or lse-language:name "none"))
; lse-show-language
)

;;; __END__ lse-interactive.el
