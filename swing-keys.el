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
;;;;    swing-keys
;;;;
;;;; Purpose
;;;;    Key definitions for swing emacs lse.
;;;;
;;;; Revision Dates
;;;;    26-May-1994 (CT) Creation (of comment)
;;;;    26-May-1994 (CT) Bindings for lse-expand-or-tabulator and lse-open-line added
;;;;    29-May-1994 (CT) Require-List moved to swing-default
;;;;    30-May-1994 (CT) Binding for lse-window:restore-temp-hidden and for
;;;;                     lse-shell-command added
;;;;    11-Jun-1994 (CT) swing-describe-key-briefly used instead of
;;;;                     describe-key-briefly
;;;;    12-Jun-1994 (CT) swing-insert-key-definition bound to [GOLD ??]
;;;;                     lse-insert-key-name       bound to [BLUE ??]
;;;;    18-Jun-1994 (CT) lse-keys factored out
;;;;    19-Jun-1994 (CT) lse-tpu-keys factored out
;;;;    21-Oct-1994 (CT) lse-set-tab-increment defined
;;;;    20-Feb-1995 (CT) [red pf4] defined as lse-tpu:copy-current-line
;;;;    20-Feb-1995 (CT) [red] "y" defined as lse-compile-defun
;;;;    26-Feb-1995 (CT) [red kp-6] defined as lse-tpu:copy-current-defun
;;;;    13-Mar-1995 (CT) Define ALT-GR keys for X mode
;;;;    14-Mar-1995 (CT) Mapping of Alt GR keys moved to lse-keys-v19.el
;;;;    17-Mar-1995 (CT) [red ?^] and [red ?$] added
;;;;    19-Mar-1995 (CT) [red gold ?^] and [red gold ?$] added
;;;;    15-Oct-1995 (CT) [gold ?$] added
;;;;     5-Mar-1997 (CT) [red kp-7] defined as lse-show-length
;;;;    29-Dec-1997 (CT) `global-set-smk' used for some red keys
;;;;     2-Jan-1998 (CT) [red ?\{] and [blue ?\C-i ?\{] added
;;;;    10-Jan-1998 (CT) Moved most Control-Keys to Alt-Keys
;;;;    25-Feb-1998 (CT) Mini keypad keys renamed
;;;;    27-Sep-2000 (CT) [blue red] defined as `ps-print-region-with-faces'
;;;;    10-Oct-2000 (CT) [green red] defined as `ps-print-buffer-with-faces'
;;;;    11-Nov-2001 (CT) [green ?^] defined as `global-hl-line-mode'
;;;;    31-Aug-2002 (CT) `swing-redefine-std-emacs-keys` removed
;;;;    31-Aug-2002 (CT) A few key definitions changed
;;;;     8-Sep-2002 (CT) `gold red` bindings added
;;;;    19-Jan-2011 (CT) [blue ?\C-i delete] defined as `lse-close-line-down`
;;;;    18-Feb-2012 (CT) Add  and use `swing-define-goto-last-position-keys`
;;;;    19-Feb-2012 (CT) Use `global-set-asp` instead of `global-set-smk`
;;;;    19-Feb-2012 (CT) Add and use `swing-define-goto-char-key`
;;;;    19-Feb-2012 (CT) Bind `lse-tpu:goto-opening-char`
;;;;    20-Feb-2012 (CT) Bind `lse-tpu:goto-next-occurrence-current-word`
;;;;    12-Mar-2012 (CT) Bind `lse-tpu:goto-closing-char`
;;;;    12-Nov-2014 (CT) Remove support for ancient Emacs versions
;;;;    12-Nov-2014 (CT) Fold swing-keys-v19.el in here
;;;;    13-Nov-2014 (CT) Use `lse-keys/define`
;;;;    20-Nov-2014 (CT) Use `global-set-key`, not `global-set-smk`
;;;;    ««revision-date»»···
;;;;--

(fset 'key-cmd 'lse-key-cmd)


(defun swing-define-gold-keys ()
  "Define Gold-Keys and related keys"
  (global-set-key [gold ?$]        'lse-check-isbn); 15-Oct-1995
; swing-define-gold-keys
)

;;; 18-Feb-2012
(defun swing-define-goto-last-position-key (key)
  (unless (consp key) (setq key (list key)))
  (global-set-key (vconcat [gold red] key) 'lse-tpu:goto-last-position)
  (global-set-key (vconcat [red gold] key) 'lse-tpu:goto-last-position)
; swing-define-goto-last-position-key
)

;;; 18-Feb-2012
(defun swing-define-goto-last-position-keys (&rest arg)
  (mapc 'swing-define-goto-last-position-key arg)
; swing-define-goto-last-position-keys
)

;;; 19-Feb-2012
(defun swing-define-goto-char-key (key &optional next-fct prev-fct)
  (unless (consp key) (setq key (list key)))
  (global-set-asp (vconcat [red]   key) (or next-fct 'lse-tpu:goto-next-char))
  (global-set-asp (vconcat [green] key) (or prev-fct 'lse-tpu:goto-prev-char))
; swing-define-goto-char-key
)

(defun swing-define-red-keys ()
  (swing-define-goto-last-position-keys
    '(left) '(right) '(up) '(down)
  )
  (mapc 'swing-define-goto-char-key
    '( ?\; ?\: ?\. ?\, ?\_ ?\- ?\+ ?\* ?\/ ?\= ?\? ?\!
       ?\% ?\& ?\~ ?\# ?\' ?\` ?\"
       ?\< ?\> ?\| ?\\ ?\« ?\»
       ?\} ?\] ?\)
     )
  )
  (mapc
      (function
        (lambda (key)
          (swing-define-goto-char-key key
            'lse-tpu:goto-prev-char
            'lse-tpu:goto-next-char
          )
        )
      )
    '( ?\{ ?\[ ?\(
     )
  )
  (lse-keys/define #'global-set-asp
    '(
      ([gray     ?\(]           lse-tpu:goto-opening-char)
      ([gray     ?\)]           lse-tpu:goto-closing-char)
      ([gray     ?\<]           lse-tpu:goto-opening-char)
      ([gray     ?\>]           lse-tpu:goto-closing-char)
      ([gray     ?\[]           lse-tpu:goto-opening-char)
      ([gray     ?\]]           lse-tpu:goto-closing-char)
      ([gray     ?\{]           lse-tpu:goto-opening-char)
      ([gray     ?\}]           lse-tpu:goto-closing-char)
      ([gray     ?\«]           lse-tpu:goto-opening-char)
      ([green    ?a]            end-of-defun)
      ([green    ?d]            up-list)
      ([green    ?f]            backward-sexp)
      ([green    ?n]            backward-list)
      ([red      ?a]            beginning-of-defun)
      ([red      ?b]            backward-sexp)
      ([red      ?d]            down-list)
      ([red      ?e]            end-of-defun)
      ([red      ?f]            forward-sexp)
      ([red      ?h]            backward-up-list)
      ([red      ?n]            forward-list)
      ([red      ?p]            backward-list)
      ([red      ?u]            up-list)
      ([red   ?\C-n]            lse-tpu:goto-next-occurrence-current-word)
      ([red   ?\C-p]            lse-tpu:goto-prev-occurrence-current-word)
    )
  )
  (lse-keys/define #'global-set-key
    '(
      ([blue     red]           ps-print-region-with-faces); 27-Sep-2000
      ([gold red ?$]            lse-tpu:remove-from-eol); 17-Mar-1995
      ([gold red ?\C-k]         yank)
      ([gold red ?^]            lse-tpu:remove-from-bol); 17-Mar-1995
      ([gold red select]        mark-defun)
      ([green    ?^]            global-hl-line-mode); 11-Nov-2001
      ([green    red]           ps-print-buffer-with-faces); 10-Oct-2000
      ([red      ?$]            lse-tpu:add-at-eol); 17-Mar-1995
      ([red      ?\A-i]         indent-sexp)
      ([red      ?\C-k]         kill-sexp)
      ([red      ?\C-l]         lse-show-length); 31-Aug-2002
      ([red      ?^]            lse-tpu:add-at-bol); 17-Mar-1995
      ([red      ?y]            lse-compile-defun)
      ([red      insert]        lse-tpu:copy-current-line); 31-Aug-2002
      ([red      kp-6]          lse-tpu:copy-current-defun)
      ([red      kp-7]          lse-show-length);  5-Mar-1997
      ([red      pf4]           lse-tpu:copy-current-line)
      ([red      select]        mark-sexp)
      ([red gold ?$]            lse-tpu:remove-from-eol); 17-Mar-1995
      ([red gold ?\C-k]         yank)
      ([red gold ?^]            lse-tpu:remove-from-bol); 17-Mar-1995
      ([red gold select]        mark-defun)
    )
  )
; swing-define-red-keys
)

(defun swing-define-blue-tab-keys ()
  "Define keys of BLUE-TAB keymap"
  (lse-keys/define #'global-set-key
    '(
      ([blue ?\C-i ?\ ]    just-one-space)
      ([blue ?\C-i ?t]     lse-tpu:toggle-newline-and-indent)
      ([blue ?\C-i ?<]     lse-set-tab-increment)
      ([blue ?\C-i ?\{]    lse-clean-empty-braces);   2-Jan-1998
      ([blue ?\C-i ?\[]    lse-clean-empty-brackets); 2-Jan-1998
      ([blue ?\C-i ?\(]    lse-clean-empty-parens);   2-Jan-1998
      ([blue ?\C-i ?\C-i]  lse-set-tab-increment)
      ([blue ?\C-i ?\C-m]  delete-blank-lines)
      ([blue ?\C-i ?\177]  lse-close-line-up)
      ([blue ?\C-i delete] lse-close-line-down)

      ([blue ?\C-i kp-0]   split-line)
      ([blue ?\C-i home]   back-to-indentation)
    )
  )
; swing-define-blue-tab-keys
)

(swing-define-gold-keys)
(swing-define-red-keys)
(swing-define-blue-tab-keys)

;;; __END__ swing-keys.el
