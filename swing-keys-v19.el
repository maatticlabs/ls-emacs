;-*- coding: iso-8859-15; -*-

;;;;unix_ms_filename_correspondency swing-keys-v19.el swi_kv19.el
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
;;;;    swing-keys-v19
;;;;
;;;; Purpose
;;;;    Key definitions for swing emacs lse.
;;;;
;;;; Revision Dates
;;;;    26-May-1994 (CT) Creation (of comment)
;;;;    26-May-1994 (CT) Bindings for lse-expand-or-tabulator and lse-open-line added
;;;;    29-May-1994 (CT) require-list moved to swing-default
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
;;;;    19-Feb-2012 (CT) Add bindins for `lse-tpu:goto-opening-char`
;;;;    ««revision-date»»···
;;;;--
(fset 'key-cmd 'lse-key-cmd)


(defun swing-define-gold-keys ()
  "Define Gold-Keys and related keys"
  ;; 25-Feb-1998 ;; `find' isn't on the keyboard anymore
  ;; (global-set-key [gold find]      'tags-search)
  (global-set-key [gold ?$]        'lse-check-isbn); 15-Oct-1995
; swing-define-gold-keys
)

;;; 18-Feb-2012
(defun swing-define-goto-last-position-key (key)
  (unless (consp key) (setq key (list key)))
  (global-set-smk (vconcat [gold red] key) 'lse-tpu:goto-last-position)
  (global-set-smk (vconcat [red gold] key) 'lse-tpu:goto-last-position)
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
    ?a ?b ?c ?d ?e ?f ?g ?i ?h ?n ?p ?r ?s ?u
    ?\C-a ?\C-e
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
  (global-set-asp [red      ?a]            'beginning-of-defun)
  (global-set-asp [green    ?a]            'end-of-defun)
  (global-set-asp [red      ?b]            'backward-sexp)
  (global-set-asp [red      ?d]            'down-list)
  (global-set-asp [green    ?d]            'up-list)
  (global-set-asp [red      ?e]            'end-of-defun)
  (global-set-asp [red      ?f]            'forward-sexp)
  (global-set-asp [green    ?f]            'backward-sexp)
  (global-set-asp [red      ?h]            'backward-up-list)
  (global-set-asp [red      ?n]            'forward-list)
  (global-set-asp [green    ?n]            'backward-list)
  (global-set-asp [red      ?p]            'backward-list)
  (global-set-key [red      ?t]            'transpose-sexps)
  (global-set-asp [red      ?u]            'up-list)
  (global-set-key [red      ?y]            'lse-compile-defun)
  (global-set-key [red      ?^]            'lse-tpu:add-at-bol); 17-Mar-1995
  (global-set-key [green    ?^]            'global-hl-line-mode); 11-Nov-2001
  (global-set-key [red      ?$]            'lse-tpu:add-at-eol); 17-Mar-1995
  (global-set-asp [gray     ?\(]           'lse-tpu:goto-opening-char)
  (global-set-asp [gray     ?\<]           'lse-tpu:goto-opening-char)
  (global-set-asp [gray     ?\[]           'lse-tpu:goto-opening-char)
  (global-set-asp [gray     ?\{]           'lse-tpu:goto-opening-char)
  (global-set-key [red gold ?^]            'lse-tpu:remove-from-bol); 17-Mar-1995
  (global-set-key [red gold ?$]            'lse-tpu:remove-from-eol); 17-Mar-1995
  (global-set-key [gold red ?^]            'lse-tpu:remove-from-bol); 17-Mar-1995
  (global-set-key [gold red ?$]            'lse-tpu:remove-from-eol); 17-Mar-1995
  (global-set-key [red      select]        'mark-sexp)
  (global-set-key [red      kp-decimal]    'mark-sexp)
  (global-set-key [red      kp-6]          'lse-tpu:copy-current-defun)
  (global-set-key [red      kp-7]          'lse-show-length);  5-Mar-1997
  (global-set-key [red      pf4]           'lse-tpu:copy-current-line)
  (global-set-key [red      insert]        'lse-tpu:copy-current-line); 31-Aug-2002
  (global-set-key [red gold select]        'mark-defun)
  (global-set-key [red gold kp-decimal]    'mark-defun)
  (global-set-key [gold red select]        'mark-defun)
  (global-set-key [gold red kp-decimal]    'mark-defun)
  (global-set-key [red      ?\A-i]         'indent-sexp)
  (global-set-key [red      ?\A-k]         'kill-sexp)
  (global-set-key [red gold ?\A-k]         'yank)
  (global-set-key [gold red ?\A-k]         'yank)
  (global-set-key [red      ?\C-l]         'lse-show-length); 31-Aug-2002
  (global-set-key [blue     red]           'ps-print-region-with-faces); 27-Sep-2000
  (global-set-key [green    red]           'ps-print-buffer-with-faces); 10-Oct-2000
; swing-define-red-keys
)

(defun swing-define-blue-tab-keys ()
  "Define keys of BLUE-TAB keymap"
  (global-set-key [blue ?\C-i ?\ ]    'just-one-space)
  (global-set-key [blue ?\C-i ?t]     'lse-tpu:toggle-newline-and-indent)
  (global-set-key [blue ?\C-i ?<]     'lse-set-tab-increment)
  (global-set-key [blue ?\C-i ?\{]    'lse-clean-empty-braces);   2-Jan-1998
  (global-set-key [blue ?\C-i ?\[]    'lse-clean-empty-brackets); 2-Jan-1998
  (global-set-key [blue ?\C-i ?\(]    'lse-clean-empty-parens);   2-Jan-1998
  (global-set-key [blue ?\C-i ?\C-i]  'lse-set-tab-increment)
  (global-set-key [blue ?\C-i ?\C-m]  'delete-blank-lines)
  (global-set-key [blue ?\C-i ?\177]  'lse-close-line-up)
  (global-set-key [blue ?\C-i delete] 'lse-close-line-down)

  (global-set-key [blue ?\C-i kp-0]   'split-line)
  (global-set-key [blue ?\C-i kp-3]   'lse-tpu:trim-line-end)
  ;; 25-Feb-1998 ;; `home' instead of `insert'
  (global-set-key [blue ?\C-i home]   'back-to-indentation)
; swing-define-blue-tab-keys
)

;;; __END__ swing-keys-v19.el
