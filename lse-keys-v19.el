;-*- unibyte: t; coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work

;;;;unix_ms_filename_correspondency lse-keys-v19:el lse_kv19:el
;;;; (c) 1994 Swing Informationssysteme GmbH. All rights reserved.

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
;;;;    lse-keys-v19
;;;;
;;;; Purpose
;;;;    Provide LS-Emacs key definitions using features of Emacs version 19
;;;;
;;;; Revision Dates
;;;;    18-Jun-1994 (CT) Creation (factored out from swing-keys-v19.el)
;;;;    27-Jun-1994 (CT) lse-flush-replacement instead of (key-cmd ...) bound
;;;;                     to [blue gold C-r]
;;;;    22-Jan-1995 (CT) [gold C-s] defined as lse-replicate-fill-in
;;;;                     (instead of lse-replicate-fill-in-by-older)
;;;;    12-Mar-1995 (CT) Adaptation to emacs on X
;;;;                     This depends on the settings in .Xmodmaprc !!!!!!
;;;;    14-Mar-1995 (CT) Adaptation to emacs on X (Alt GR keys)
;;;;    19-Mar-1995 (CT) lse-emacsX-p used instead of (getenv "DISPLAY")
;;;;    19-Mar-1995 (CT) Key definitions for [?\C-i] and [gold ?\C-i] moved
;;;;                     to lse-tpu-keys.v19.el
;;;;    20-Aug-1995 (CT) Alt- and Meta-shifted control-keys defined
;;;;    10-Mar-1996 (CT) [C-y] defined as lse-replicate-menu
;;;;    30-May-1996 (CT) `lse-keys-v19:define-fkp-key' and
;;;;                     `lse-keys-v19:define-keypad-fkp' factored
;;;;    28-Sep-1996 (CT) Definition of cursor and function keys commented out
;;;;                     (`;;; ? ; ')
;;;;    29-Sep-1996 (CT) `lse-keys-v19:define-keypad-fkp' renamed to
;;;;                     `lse-keys-v19:define-X-keypad-fkp'
;;;;    29-Sep-1996 (CT) `lse-keys-v19:define-vt100-keypad-fkp' and
;;;;                     `lse-keys-v19:define-vt200-mini-keypad-fkp' factored
;;;;    13-Dec-1997 (CT) Several keydefinitions moved into
;;;;                     lse-flat-fill-in:keymap
;;;;    17-Dec-1997 (CT) `lse-keys:keep-emacs-control-keys' added
;;;;     2-Jan-1998 (CT) `lse-keys:keep-emacs-control-keys' renamed to
;;;;                     `lse-keys:override-emacs-control-keys'
;;;;     5-Jan-1998 (CT) `lse-flat-fill-in:remove-*-blank-line' added
;;;;     6-Jan-1998 (CT) Call of `lse-flat-fill-in:define-keys' moved to begin
;;;;                     of `lse-define-std-keys' (otherwise "\C-m" isn't
;;;;                     defined as lse-replicate-menu in the
;;;;                     lse-flat-fill-in:keymap)
;;;;    18-Jan-1998 (CT) `lse-flat-fill-in:remove-*-whitespace' added
;;;;    26-Feb-1998 (CT) Map `delete' instead of `kp-delete' to `remove'
;;;;                     (Mini-keypad key)
;;;;     1-Jan-1999 (CT) `lse-kill-fill-in-join-sexp' added
;;;;     1-Jan-1999 (CT) `lse-kill-all-optional-fill-ins-line' added
;;;;     1-Jan-1999 (CT) `lse-replicate-fill-ins-line' added
;;;;    28-Dec-1999 (CT) `lse-insert-bquotes' passed to
;;;;                     `lse-flat-fill-in:define-keys'
;;;;     3-Jan-2000 (CT) Removed `lse-flat-fill-in:define-key' for `[f1]'
;;;;    21-Apr-2000 (CT) Removed fill-in bindings for control keys
;;;;    17-Jun-2001 (CT) `lse-keys-v19:define-super-function-keys-as-keypad'
;;;;                     added
;;;;    13-Sep-2002 (CT) `lse-flat-fill-in:define-key` modified
;;;;    13-Sep-2002 (CT) `[tab]` defined as `lse-goto-next-fill-in`
;;;;     8-Oct-2002 (CT) `[C-tab]` instead of `[tab]` defined as
;;;;                     `lse-goto-next-fill-in`
;;;;     6-Nov-2002 (CT) `[?\A-s]` removed
;;;;    12-Dec-2002 (CT) `[?\A-s]` re-added (needed for repeated replication)
;;;;     2-Oct-2007 (CT) lse-unset-emacs-function-key-definitions added and
;;;;                     called
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-keys-v19)

;;;  2-Oct-2007
(defun lse-unset-emacs-function-key-definitions ()
  (mapcar 'global-unset-key
    '([f2] [f3] [f4] [f5] [f6] [f7] [f8] [f9]
      [f10] [f11] [f12] [f13] [f14] [f15] [f16] [f17] [f18] [f19] [f20]
      [?\C-x ?4] [?\C-x ?4] [?\C-x ?6]
     )
  )
; lse-unset-emacs-function-key-definitions
)
(lse-unset-emacs-function-key-definitions)

;;; 13-Sep-2002
(defvar lse-keys:override-emacs-control-keys nil
  "Set this variable to bind control keys to fill-in functions."
)

;;; 30-May-1996
(defun lse-keys-v19:define-fkp-key (key def)
  ;; (global-unset-key key)
  (define-key function-key-map key def)
; lse-keys-v19:define-fkp-key
)

;;;  8-Sep-2002
(defun lse-keys-v19:define-fkp-key+mods (key def)
  "Define all modifier combinations for `key` in `function-key-map`"
  t ;;; XXX
; lse-keys-v19:define-fkp-key+mods
)
;;; 17-Jun-2001
(defun lse-keys-v19:define-super-function-keys-as-keypad ()
  "Define super-shifted keys as aliases for numeric keypad keys"
  (lse-keys-v19:define-fkp-key [s-f1]       [kp-1])
  (lse-keys-v19:define-fkp-key [s-f2]       [kp-2])
  (lse-keys-v19:define-fkp-key [s-f3]       [kp-3])
  (lse-keys-v19:define-fkp-key [s-f4]       [kp-4])
  (lse-keys-v19:define-fkp-key [s-f5]       [kp-5])
  (lse-keys-v19:define-fkp-key [s-f6]       [kp-6])
  (lse-keys-v19:define-fkp-key [s-f7]       [kp-7])
  (lse-keys-v19:define-fkp-key [s-f8]       [kp-8])
  (lse-keys-v19:define-fkp-key [s-f9]       [kp-9])
  (lse-keys-v19:define-fkp-key [s-f10]      [kp-0])
  (lse-keys-v19:define-fkp-key [s-f18]      [pf3])
  (lse-keys-v19:define-fkp-key [s-break]    [pf4])
  (lse-keys-v19:define-fkp-key [?\s--]      [kp-subtract])
  (lse-keys-v19:define-fkp-key [?\s-.]      [kp-decimal])
; lse-keys-v19:define-super-function-keys-as-keypad
)

;;; 30-May-1996
(defun lse-keys-v19:define-X-keypad-fkp ()
  (interactive)
  (lse-keys-v19:define-fkp-key [kp-f1]        [gold])
  (lse-keys-v19:define-fkp-key [s-kp-f1]      [gray]); 30-Aug-2002
  (lse-keys-v19:define-fkp-key [kp-f2]        [blue])
  (lse-keys-v19:define-fkp-key [s-kp-f2]      [pink]); 30-Aug-2002
  (lse-keys-v19:define-fkp-key [kp-f3]        [pf3])
  (lse-keys-v19:define-fkp-key [kp-f4]        [pf4])

  ;;; XXX (lse-keys-v19:define-fkp-key+mods [f34] [backspace-append])
  ;;; XXX (lse-keys-v19:define-fkp-key+mods [f35] [delete-append])

  (lse-keys-v19:define-super-function-keys-as-keypad); 17-Jun-2001
; lse-keys-v19:define-X-keypad-fkp
)

;;; 29-Sep-1996
(defun lse-keys-v19:define-vt100-keypad-fkp ()
  (lse-keys-v19:define-fkp-key "\eOP"   [gold])
  (lse-keys-v19:define-fkp-key "\eOQ"   [blue])
  (lse-keys-v19:define-fkp-key "\eOR"   [pf3])
  (lse-keys-v19:define-fkp-key "\eOS"   [pf4])
  (lse-keys-v19:define-fkp-key "\eOM"   [kp-enter])
  (lse-keys-v19:define-fkp-key "\eOl"   [kp-separator])
  (lse-keys-v19:define-fkp-key "\eOm"   [kp-subtract])
  (lse-keys-v19:define-fkp-key "\eO?"   [kp-subtract]); allows KP_MinPLus in Linux keytable
  (lse-keys-v19:define-fkp-key "\eOn"   [kp-decimal])
  (lse-keys-v19:define-fkp-key "\eOp"   [kp-0])
  (lse-keys-v19:define-fkp-key "\eOq"   [kp-1])
  (lse-keys-v19:define-fkp-key "\eOr"   [kp-2])
  (lse-keys-v19:define-fkp-key "\eOs"   [kp-3])
  (lse-keys-v19:define-fkp-key "\eOt"   [kp-4])
  (lse-keys-v19:define-fkp-key "\eOu"   [kp-5])
  (lse-keys-v19:define-fkp-key "\eOv"   [kp-6])
  (lse-keys-v19:define-fkp-key "\eOw"   [kp-7])
  (lse-keys-v19:define-fkp-key "\eOx"   [kp-8])
  (lse-keys-v19:define-fkp-key "\eOy"   [kp-9])
; lse-keys-v19:define-vt100-keypad-fkp
)

;;; 29-Sep-1996
(defun lse-keys-v19:define-vt200-mini-keypad-fkp ()
  (lse-keys-v19:define-fkp-key "\e[1~"  [find])   ; e1
  (lse-keys-v19:define-fkp-key "\e[2~"  [insert]) ; e2
  (lse-keys-v19:define-fkp-key "\e[3~"  [remove]) ; e3
  (lse-keys-v19:define-fkp-key "\e[4~"  [select]) ; e4
  (lse-keys-v19:define-fkp-key "\e[5~"  [prior])  ; e5
  (lse-keys-v19:define-fkp-key "\e[6~"  [next])   ; e6
; lse-keys-v19:define-vt200-mini-keypad-fkp
)

;;; Rename some function keys
(if lse-emacsX-p
    (progn
      (lse-keys-v19:define-fkp-key [f11]       [help])
      (lse-keys-v19:define-fkp-key [f15]       [help])
      (lse-keys-v19:define-fkp-key [f12]       [do])
      (lse-keys-v19:define-fkp-key [f16]       [do])
      (lse-keys-v19:define-fkp-key [f17]       [red])
      (lse-keys-v19:define-fkp-key [s-f17]     [green]); 30-Aug-2002

      (lse-keys-v19:define-X-keypad-fkp)
    )
  ;; (not lse-emacsX-p)
  (lse-keys-v19:define-fkp-key "\e[28~" [help])
  (lse-keys-v19:define-fkp-key "\e[29~" [do])
  (lse-keys-v19:define-fkp-key "\e[31~" [red])    ; f17
  (lse-keys-v19:define-fkp-key "\e[32~" [green])  ; f18
  (lse-keys-v19:define-fkp-key "\e[33~" [pink])   ; f19
  (lse-keys-v19:define-fkp-key "\e[34~" [?\e])    ; use f20 as escape

  (lse-keys-v19:define-vt100-keypad-fkp)
  (lse-keys-v19:define-vt200-mini-keypad-fkp)
)

(defmacro lse-key-cmd (&rest args)
  (`'(lambda () (interactive) (,@ args)))
)

;;; 22-Oct-2002
(defun lse-key-std-tab-g ()
  (interactive)
  (global-set-key [tab]   'lse-tabulator)
  (global-set-key [C-tab] 'lse-goto-next-fill-in)
; lse-key-std-tab-g
)

;;; 22-Oct-2002
(defun lse-key-template-tab-g ()
  (interactive)
  (global-set-key [tab]   'lse-goto-next-fill-in)
  (global-set-key [C-tab] 'lse-tabulator)
; lse-key-template-tab-g
)

;;; 22-Oct-2002
(defun lse-key-std-tab-l ()
  (interactive)
  (local-set-key  [tab]   'lse-tabulator)
  (local-set-key  [C-tab] 'lse-goto-next-fill-in)
; lse-key-std-tab-l
)

;;; 22-Oct-2002
(defun lse-key-template-tab-l ()
  (interactive)
  (local-set-key  [tab]   'lse-goto-next-fill-in)
  (local-set-key  [C-tab] 'lse-tabulator)
; lse-key-template-tab-l
)

(defun lse-define-std-keys ()
  ;;++ 13-Dec-1997
  ;; 'lse-flat-fill-in:define-keys defines all keys bound to the enumerated
  ;; functions as lse-flat-fill-in:replace-and-delegate-key in the
  ;; text-property keymap of flat fill-ins (lse-flat-fill-in:keymap)
  ;;
  ;; if you want a key/function to start replacement automatically, just add
  ;; the function to this list
  (lse-flat-fill-in:define-keys
      'lse-insert-backquote-quote
      'lse-insert-bars
      'lse-insert-bquotes
      'lse-insert-braces
      'lse-insert-brackets
      'lse-insert-buffer
      'lse-insert-buffer-name
      'lse-insert-comma
      'lse-insert-dd-mm-yyyy+blank
      'lse-insert-dd-mmm-yyyy+blank
      'lse-insert-double-backquote-quote
      'lse-insert-dquotes
      'lse-insert-file
      'lse-insert-key-definition
      'lse-insert-key-name
      'lse-insert-parentheses
      'lse-insert-semicolon
      'lse-insert-squotes
      'lse-insert-user-full-name
      'lse-insert-user-initials
      'lse-insert-user-name
      'lse-tpu:duplicate-previous-line
      'lse-tpu:duplicate-word-in-previous-line
      'lse-tpu:paste-region
      'lse-tpu:quoted-insert
      'lse-tpu:special-insert
      'lse-tpu:undelete-char
      'lse-tpu:undelete-line
      'lse-tpu:undelete-word
      'quoted-insert
      'self-insert-command
  )
  ;;--

  (lse-key-std-tab-g); 22-Oct-2002
  (global-set-key [          ?\A-b] 'lse-goto-parent-expansion-head); 17-Oct-1996
  (global-set-key [          ?\s-b] 'lse-goto-parent-expansion-head); 17-Oct-1996
  (global-set-key [          ?\s-n] 'lse-goto-next-expansion); 11-Oct-1996
  (global-set-key [          ?\M-n] 'lse-fill-in-marks:goto-next-head); 29-Dec-1997
  (global-set-key [       ?\M-\s-n] 'lse-fill-in-marks:goto-next-tail); 29-Dec-1997
  (global-set-key [          ?\s-p] 'lse-goto-prev-expansion); 11-Oct-1996
  (global-set-key [          ?\M-p] 'lse-fill-in-marks:goto-prev-head); 29-Dec-1997
  (global-set-key [       ?\M-\s-p] 'lse-fill-in-marks:goto-prev-tail); 29-Dec-1997

  (global-set-key [          ?\A-e] 'lse-expand-token)
  (global-set-key [gold      ?\A-e] 'lse-unexpand-fill-in); 28-Dec-1997
  (global-set-key [blue      ?\A-e] 'lse-reexpand-fill-in);  2-Jan-1998

  (global-set-key [blue      ?\A-f] 'lse-window:restore-temp-hidden)

  (global-set-key [          ?\A-k] 'lse-kill-fill-in)
  (global-set-key [       ?\s-\A-k] 'lse-kill-fill-in-join-sexp);  1-Jan-1999
  (global-set-key [gold      ?\A-k] 'lse-unkill-fill-in); 28-Dec-1997
  (global-set-key [blue gold ?\A-k] 'lse-kill-all-optional-fill-ins);  2-Jan-1998
  (global-set-key [red       ?\A-k] 'lse-kill-all-optional-fill-ins-line); 1-Jan-1999

  (global-set-key [          ?\A-n] 'lse-goto-next-fill-in); 11-Oct-1996
  (global-set-key [gold      ?\A-n] 'lse-goto-last-position); 28-Dec-1997

  (global-set-key [          ?\A-p] 'lse-goto-prev-fill-in); 11-Oct-1996
  (global-set-key [gold      ?\A-p] 'lse-goto-last-position); 28-Dec-1997

  (global-set-key [          ?\A-r] 'lse-replace-fill-in); 17-Dec-1997 ALT
  (global-set-key [gold      ?\A-r] 'lse-unreplace-fill-in); 28-Dec-1997
  (global-set-key [blue      ?\A-r] 'lse-rereplace-fill-in);  2-Jan-1998
  (global-set-key [    ?\A-\M-\C-r] 'lse-flush-replacement); 20-Aug-1995
  (global-set-key [          ?\A-s] 'lse-replicate-fill-in); 12-Dec-2002

  (global-set-key [blue gold ?\A-s] 'lse-replicate-fill-ins-line);  1-Jan-1999

  ;; 13-Dec-1997
  ;; Keys in text-property keymap for flat fill-ins
  (lse-flat-fill-in:define-key [backspace]       'lse-kill-current-fill-in)
  (lse-flat-fill-in:define-key [delete]          'lse-kill-current-fill-in)
  (lse-flat-fill-in:define-key [help]            'lse-help-fill-in)
  (lse-flat-fill-in:define-key [return]          'lse-flat-fill-in:open-line); 4-Jan-1998
  (lse-flat-fill-in:define-key [tab]             'lse-expand)
  (lse-flat-fill-in:define-key [C-tab]           'lse-tabulator); 22-Oct-2002
  (lse-flat-fill-in:define-key [C-backspace]     'lse-flat-fill-in:remove-prev-blank-line); 13-Sep-2002
  (lse-flat-fill-in:define-key [M-backspace]     'lse-flat-fill-in:remove-leading-whitespace); 13-Sep-2002
  (lse-flat-fill-in:define-key [C-delete]        'lse-flat-fill-in:remove-next-blank-line); 13-Sep-2002
  (lse-flat-fill-in:define-key [M-delete]        'lse-flat-fill-in:remove-trailing-whitespace); 13-Sep-2002
  (lse-flat-fill-in:define-key [?\A-e]           'lse-expand)
  (lse-flat-fill-in:define-key [?\A-i]           'lse-expand)
  (lse-flat-fill-in:define-key [?\A-j]           'lse-flat-fill-in:remove-leading-whitespace); 18-Jan-1998
  (lse-flat-fill-in:define-key [?\C-k]           'lse-flat-fill-in:remove-next-blank-line); 13-Sep-2002
  (lse-flat-fill-in:define-key [?\C-m]           'lse-flat-fill-in:open-line); 4-Jan-1998
  (lse-flat-fill-in:define-key [?\A-o]           'lse-describe-fill-in)
  (lse-flat-fill-in:define-key [?\C-o]           'lse-flat-fill-in:open-line)
  (lse-flat-fill-in:define-key [?\A-q]           'lse-replicate-menu); 10-Mar-1996
  (lse-flat-fill-in:define-key [?\A-r]           'lse-replace-fill-in)
  (lse-flat-fill-in:define-key [?\A-s]           'lse-replicate-fill-in)
  (lse-flat-fill-in:define-key [?\A-u]           'lse-flat-fill-in:remove-prev-blank-line); 5-Jan-1998
  (lse-flat-fill-in:define-key [blue ?\ ]        'lse-flat-fill-in:align-to-previous-word)
  (lse-flat-fill-in:define-key [gold ?\ ]        'lse-flat-fill-in:align-to-next-word)
  (lse-flat-fill-in:define-key [gold ?\A-o]      'lse-help-fill-in);  2-Jan-1998
  (lse-flat-fill-in:define-key [mouse-2]         'lse-flat-fill-in:replace-and-mouse-yank)
  ;; (lse-flat-fill-in:define-key [right]        'lse-goto-next-fill-in)
  ;; (lse-flat-fill-in:define-key [down]         'lse-goto-next-fill-in)
  ;; (lse-flat-fill-in:define-key [left]         'lse-goto-prev-fill-in)
  ;; (lse-flat-fill-in:define-key [up]           'lse-goto-prev-fill-in)

  (if (and (boundp 'lse-keys:override-emacs-control-keys)
           lse-keys:override-emacs-control-keys
      ); 17-Dec-1997
      (progn
        (global-set-key              [?\C-e] 'lse-expand-token)
        (global-set-key              [?\C-k] 'lse-kill-fill-in)
        (global-set-key              [?\C-n] 'lse-goto-next-fill-in)
        (global-set-key              [?\C-p] 'lse-goto-prev-fill-in)
        (lse-flat-fill-in:define-key [?\C-e] 'lse-expand)
        (lse-flat-fill-in:define-key [?\C-o] 'lse-describe-fill-in)
        (lse-flat-fill-in:define-key [?\C-q] 'lse-replicate-menu); 10-Mar-1996
        (lse-flat-fill-in:define-key [?\C-r] 'lse-replace-fill-in)
        (lse-flat-fill-in:define-key [?\C-s] 'lse-replicate-fill-in)
        (global-set-key [gold      ?\C-e] 'lse-unexpand-fill-in)
        (global-set-key [blue      ?\C-e] 'lse-reexpand-fill-in)
        (global-set-key [blue      ?\C-f] 'lse-window:restore-temp-hidden)
        (global-set-key [gold      ?\C-k] 'lse-unkill-fill-in)
        (global-set-key [blue gold ?\C-k] 'lse-kill-all-optional-fill-ins)
        (global-set-key [    ?\A-\M-\C-k] 'lse-kill-all-optional-fill-ins); 20-Aug-1995
        (global-set-key [       ?\A-\C-k] 'lse-unkill-fill-in); 20-Aug-1995
        (global-set-key [gold      ?\C-n] 'lse-goto-last-position)
        (global-set-key [       ?\A-\C-n] 'lse-goto-last-position); 20-Aug-1995
        (global-set-key [gold      ?\C-p] 'lse-goto-last-position)
        (global-set-key [       ?\A-\C-p] 'lse-goto-last-position); 20-Aug-1995
        (global-set-key [gold      ?\C-r] 'lse-unreplace-fill-in)
        (global-set-key [       ?\A-\C-r] 'lse-unreplace-fill-in); 20-Aug-1995
        (global-set-key [blue      ?\C-r] 'lse-rereplace-fill-in)
        (global-set-key [       ?\M-\C-r] 'lse-rereplace-fill-in); 20-Aug-1995
        (lse-flat-fill-in:define-key [gold ?\C-o]    'lse-help-fill-in)
        (lse-flat-fill-in:define-key [gold ?\C-s]    'lse-replicate-fill-in); 22-Jan-1995
      )
  )
; lse-define-std-keys
)

(defun lse-key-name (key-seq)
  (interactive "kPress key")
  (concat "`" (key-description key-seq) "'")
; lse-key-name
)

(defun lse-insert-key-name (key)
  "The name of key is inserted into current buffer"
  (interactive "kKey ")
  (insert (key-description key))
; lse-insert-key-name
)

(defun lse-insert-key-definition (key)
  "The definition of key is inserted into current buffer"
  (interactive "kKey to describe ")
  (let ((binding (key-binding key))
       )
    (if (or (null binding) (integerp binding))
        (message "%s is undefined" (key-description key))
      (insert
        (format "%s"
                (if (symbolp binding) binding (prin1-to-string binding))
        )
      )
    )
  )
; lse-insert-key-definition
)
