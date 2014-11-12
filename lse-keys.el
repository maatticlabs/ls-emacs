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
;;;;    lse-keys
;;;;
;;;; Purpose
;;;;    Provide standard key definitions for LS-Emacs
;;;;
;;;; Revision Dates
;;;;    18-Jun-1994 (CT) Creation
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
;;;;     2-Oct-2007 (CT) `lse-unset-emacs-function-key-definitions` added and
;;;;                     called
;;;;     3-Oct-2007 (CT) `lse-keys-v19:define-vt100-keypad-fkp`,
;;;;                     `lse-keys-v19:define-vt200-mini-keypad-fkp`,
;;;;                     `lse-keys:override-emacs-control-keys`,
;;;;                     `lse-keys-v19:define-super-function-keys-as-keypad`,
;;;;                     and `lse-unset-emacs-function-key-definitions`
;;;;                     removed
;;;;     3-Oct-2007 (CT) `lse-keys:emacs-bindings-to-unset` and
;;;;                     `lse-keys:function-key-map-bindings` added und
;;;;                     passed to `mapcar` to allow customizations
;;;;     5-Oct-2007 (CT) `lse-keys-v19:define-fkp-key` removed
;;;;    11-Oct-2007 (CT) `[M-home]` and `[M-end]` added to
;;;;                     `lse-keys:emacs-bindings-to-unset`
;;;;    29-Sep-2008 (CT) `lse-key:toggle-tab` added
;;;;    29-Jul-2009 (CT) Modernize use of backquotes
;;;;    13-Nov-2009 (CT) Explicitly turn on `normal-erase-is-backspace-mode`
;;;;    18-Nov-2009 (CT) `lse-fill-in-marks:goto-open-head` and
;;;;                     `lse-fill-in-marks:goto-open-tail` added
;;;;    18-Feb-2012 (CT) s/lse-goto-last-position/lse-tpu:goto-last-position/
;;;;    24-Feb-2012 (CT) Add `[modeline ...mouse...]` to
;;;;                     `lse-keys:emacs-bindings-to-unset`
;;;;    21-Oct-2014 (CT) Add `[?\C-x?5?*]` to `lse-keys:emacs-bindings-to-unset`
;;;;    12-Nov-2014 (CT) Remove support for ancient Emacs versions
;;;;    12-Nov-2014 (CT) Fold lse-keys-v19.el in here
;;;;    12-Nov-2014 (CT) Move some functions to `lse-tpu-keys.el`
;;;;    13-Nov-2014 (CT) Use `lse-keys/define`
;;;;    ««revision-date»»···
;;;;--

(provide 'lse-keys)

(require 'lse-tpu-keys)

;;; 13-Nov-2009
(if (fboundp 'normal-erase-is-backspace-mode)
    (normal-erase-is-backspace-mode 1)
)
;;;  3-Oct-2007
(defvar lse-keys:emacs-bindings-to-unset
    '([f2] [f3] [f4] [f5] [f6] [f7] [f8] [f9]
      [f10] [f11] [f12] [f13] [f14] [f15] [f16] [f17] [f18] [f19] [f20]
      [?\C-x?4]
      [?\C-x?5?0] [?\C-x?5?\C-f]  [?\C-x?5?\C-o]
      [?\C-x?5?b] [?\C-x?5?d] [?\C-x?5?f] [?\C-x?5?m] [?\C-x?5?r]
      [?\C-x?6]
      [M-home] [M-end]
      [modeline C-mouse-2]
      [mode-line C-mouse-3]
      [modeline mouse-2]
      [modeline mouse-3]
     )
  "Override this in your .emacs file, if you want a different set of
  keybindings defined by standard Emacs to be globally unset."
)

;;;  3-Oct-2007
(defvar lse-keys:function-key-map-bindings
    '( ;; if you change this, please update the copy in lse-config.el, too
       ([pause]         [gold])
       ([scroll]        [blue]); Windows NT
       ([scroll_lock]   [blue]); GNU/Linux (Gentoo)
       ([print]         [red])
       ([f12]           [do])
       ([s-pause]       [gray])
       ([s-scroll]      [pink])
       ([s-scroll_lock] [pink])
       ([s-print]       [green])
     )
  "Override this in your .emacs file to define which keys to use for [gold],
  [blue], [red], ..."
)

;;; 29-Sep-2008
(defvar lse-key:toggle-tab-p nil)
(make-variable-buffer-local 'lse-key:toggle-tab-p)

;;; 22-Oct-2002
(defun lse-key-std-tab-g ()
  (interactive)
  (lse-keys/define #'global-set-key
    '(
      ([tab]   lse-tabulator)
      ([C-tab] lse-goto-next-fill-in)
    )
  )
; lse-key-std-tab-g
)

;;; 22-Oct-2002
(defun lse-key-template-tab-g ()
  (interactive)
  (lse-keys/define #'global-set-key
    '(
      ([tab]   lse-goto-next-fill-in)
      ([C-tab] lse-tabulator)
    )
  )
; lse-key-template-tab-g
)

;;; 22-Oct-2002
(defun lse-key-std-tab-l ()
  (interactive)
  (lse-keys/define #'local-set-key
    '(
      ([tab]   lse-tabulator)
      ([C-tab] lse-goto-next-fill-in)
    )
  )
; lse-key-std-tab-l
)

;;; 22-Oct-2002
(defun lse-key-template-tab-l ()
  (interactive)
  (lse-keys/define #'local-set-key
    '(
      ([tab]   lse-goto-next-fill-in)
      ([C-tab] lse-tabulator)
    )
  )
; lse-key-template-tab-l
)

;;; 29-Sep-2008
(defun lse-key:toggle-tab ()
  "Toggle between normal tab-binding `lse-tabulator` and `lse-goto-next-fill-in`."
  (interactive)
  (let ((binding (key-binding [tab])))
    (setq lse-key:toggle-tab-p (not (eq binding 'lse-tabulator)))
    (if lse-key:toggle-tab-p
        (lse-key-std-tab-l)
      (lse-key-template-tab-l)
    )
  )
  lse-key:toggle-tab-p
; lse-key:toggle-tab
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
  (lse-keys/define #'global-set-key
    '(
      ([          ?\A-b] lse-goto-parent-expansion-head); 17-Oct-1996
      ([          ?\s-b] lse-goto-parent-expansion-head); 17-Oct-1996
      ([          ?\s-n] lse-goto-next-expansion); 11-Oct-1996
      ([          ?\M-n] lse-fill-in-marks:goto-next-head); 29-Dec-1997
      ([       ?\M-\s-n] lse-fill-in-marks:goto-next-tail); 29-Dec-1997
      ([          ?\s-p] lse-goto-prev-expansion); 11-Oct-1996
      ([          ?\M-p] lse-fill-in-marks:goto-prev-head); 29-Dec-1997
      ([       ?\M-\s-p] lse-fill-in-marks:goto-prev-tail); 29-Dec-1997

      ([          ?\A-e] lse-expand-token)
      ([gold      ?\A-e] lse-unexpand-fill-in); 28-Dec-1997
      ([blue      ?\A-e] lse-reexpand-fill-in);  2-Jan-1998

      ([blue      ?\A-f] lse-window:restore-temp-hidden)
      ([          ?\M-f] lse-fill-in-marks:goto-open-tail); 18-Nov-2009
      ([          ?\M-b] lse-fill-in-marks:goto-open-head); 18-Nov-2009

      ([          ?\A-k] lse-kill-fill-in)
      ([       ?\s-\A-k] lse-kill-fill-in-join-sexp);  1-Jan-1999
      ([gold      ?\A-k] lse-unkill-fill-in); 28-Dec-1997
      ([blue gold ?\A-k] lse-kill-all-optional-fill-ins);  2-Jan-1998
      ([red       ?\A-k] lse-kill-all-optional-fill-ins-line); 1-Jan-1999

      ([          ?\A-n] lse-goto-next-fill-in)
      ([gold      ?\A-n] lse-tpu:goto-last-position)

      ([          ?\A-p] lse-goto-prev-fill-in); 11-Oct-1996
      ([gold      ?\A-p] lse-tpu:goto-last-position)

      ([          ?\A-r] lse-replace-fill-in); 17-Dec-1997 ALT
      ([gold      ?\A-r] lse-unreplace-fill-in); 28-Dec-1997
      ([blue      ?\A-r] lse-rereplace-fill-in);  2-Jan-1998
      ([    ?\A-\M-\C-r] lse-flush-replacement); 20-Aug-1995
      ([          ?\A-s] lse-replicate-fill-in); 12-Dec-2002

      ([blue gold ?\A-s] lse-replicate-fill-ins-line);  1-Jan-1999
    )
  )

  ;; 13-Dec-1997
  ;; Keys in text-property keymap for flat fill-ins
  (lse-keys/define #'lse-flat-fill-in:define-key
    '(
      ([backspace]       lse-kill-current-fill-in)
      ([delete]          lse-kill-current-fill-in)
      ([help]            lse-help-fill-in)
      ([return]          lse-flat-fill-in:open-line); 4-Jan-1998
      ([tab]             lse-expand)
      ([C-tab]           lse-tabulator); 22-Oct-2002
      ([C-backspace]     lse-flat-fill-in:remove-prev-blank-line); 13-Sep-2002
      ([M-backspace]     lse-flat-fill-in:remove-leading-whitespace); 13-Sep-2002
      ([C-delete]        lse-flat-fill-in:remove-next-blank-line); 13-Sep-2002
      ([M-delete]        lse-flat-fill-in:remove-trailing-whitespace); 13-Sep-2002
      ([?\A-e]           lse-expand)
      ([?\A-i]           lse-expand)
      ([?\A-j]           lse-flat-fill-in:remove-leading-whitespace); 18-Jan-1998
      ([?\C-k]           lse-flat-fill-in:remove-next-blank-line); 13-Sep-2002
      ([?\C-m]           lse-flat-fill-in:open-line); 4-Jan-1998
      ([?\A-o]           lse-describe-fill-in)
      ([?\C-o]           lse-flat-fill-in:open-line)
      ([?\A-q]           lse-replicate-menu); 10-Mar-1996
      ([?\A-r]           lse-replace-fill-in)
      ([?\A-s]           lse-replicate-fill-in)
      ([?\A-u]           lse-flat-fill-in:remove-prev-blank-line); 5-Jan-1998
      ([blue ?\ ]        lse-flat-fill-in:align-to-previous-word)
      ([gold ?\ ]        lse-flat-fill-in:align-to-next-word)
      ([gold ?\A-o]      lse-help-fill-in);  2-Jan-1998
      ([mouse-2]         lse-flat-fill-in:replace-and-mouse-yank)
    )
  )
; lse-define-std-keys
)

;;;  3-Oct-2007
(mapc #'global-unset-key lse-keys:emacs-bindings-to-unset)

(mapc (function (lambda (x) (apply 'define-key function-key-map x)))
  lse-keys:function-key-map-bindings
)

;;; __END__ lse-keys.el
