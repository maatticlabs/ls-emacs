;-*- unibyte: t; coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work

;;;;unix_ms_filename_correspondency lse-tpu-keys-v19:el lse_tpk9:el
;;;; Copyright (C) 1994-2009 Mag. Christian Tanzer. All rights reserved.
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
;;;;    lse-tpu-keys-v19
;;;;
;;;; Purpose
;;;;    Provide key bindings for lse-tpu functions
;;;;
;;;; Revision Dates
;;;;    19-Jun-1994 (CT) Creation
;;;;    16-Oct-1994 (CT) [blue gold] "o" defined as lse-make-directory
;;;;    21-Oct-1994 (CT) [blue gold ?1] and friends defined
;;;;    17-Dec-1994 (CT) [blue gold right] and [blue gold left] redefined
;;;;                     (to lse-indent-line-by-word and
;;;;                         lse-deindent-line-by-word)
;;;;     6-Jan-1995 (CT) [blue gold ?*] defined as lse-show-windows
;;;;    20-Feb-1995 (CT) [blue] "y" defined as lse-compile
;;;;                     C-f defined as lse-compilation:next-error
;;;;    20-Feb-1995 (CT) [blue gold find] defined as lse-grep
;;;;    11-Mar-1995 (CT) [gold ^] and [blue ^] defined (german keyboard
;;;;                     compatibility)
;;;;    19-Mar-1995 (CT) Key definitions for [?\C-i] and [gold ?\C-i] moved
;;;;                     in here from lse-keys-v19.el
;;;;     9-Jun-1995 (CT) [gold insert] added (lse-indent:goto-indent-pos)
;;;;     9-Jun-1995 (CT) [blue gold y] defined as lse-set-compile-command
;;;;    28-Jun-1995 (CT) [?\C-\ ] and [?\C-\.] defined as universal-argument
;;;;    18-Jul-1995 (CT) [A-right] and [A-left] (and other Alt-shifted keys)
;;;;                     defined
;;;;    19-Jul-1995 (CT) Definitions of [gold ?+] and [blue ?+] interchanged
;;;;    20-Aug-1995 (CT) Alt-shifted keys for lse-tpu:pan-right and
;;;;                     lse-tpu:pan-left defined
;;;;    15-Sep-1995 (CT) Alt-shifted keys for lse-align-and-up and
;;;;                     lse-align-and-down defined
;;;;    30-Mar-1996 (CT) [?\C-y] and [gold ?\C-y] defined
;;;;    26-Apr-1996 (CT) [blue ?'], [blue ?\"] and their gold-versions
;;;;                     defined as lse-insert/remove-[double]-backquote-quote
;;;;    28-Apr-1996 (CT) [A-|] defined
;;;;    14-Jun-1996 (CT) `(Replace-Binding 'list-buffers 'lse-show-buffers)'
;;;;                     commented out (caused strange bug in Emacs 19.30)
;;;;    29-Sep-1996 (CT) Commented out keydefinitions for lse-lingo functions
;;;;     3-Oct-1996 (CT) Define alt-shifted keypad keys
;;;;    28-Oct-1996 (CT) [gold ?\C-w] defined as lse-toggle-read-only
;;;;     4-Nov-1996 (CT) [gold ?\C-d] and [?\A-d] defined as
;;;;                     ispell-complete-word
;;;;     5-Mar-1997 (CT) Use `lse-frame:set-width' instead of `vt-wide' and
;;;;                     `vt-narrow'
;;;;    27-Mar-1997 (CT) `lse-toggle-lse-split-line' added
;;;;     7-Apr-1997 (CT) `lse-select-current-line' added
;;;;    22-May-1997 (CT) [blue ?=] defined as lse-split-window-horizontally
;;;;                     (was defined as lse-split-window)
;;;;    15-Jul-1997 (CT) [?\A-^] added
;;;;    18-Dec-1997 (CT) Super-Bindings for `delete-*-append' added
;;;;    29-Dec-1997 (CT) [?\A-g] defined
;;;;    29-Dec-1997 (CT) [?\s-d] defined as 'hippie-expand
;;;;    29-Dec-1997 (CT) `global-set-smk' added
;;;;    30-Dec-1997 (CT) `global-mak-smk' and `lse-tpu-keys:shifted' added
;;;;    30-Dec-1997 (CT) `lse-move-key-to-prefix' added
;;;;     4-Jan-1998 (CT) `(lse-copy-key-in-minibuffer-maps [?\C-m] [return])'
;;;;                         suddenly [return] didn't work in minibuffer maps
;;;;                         (it was defined as 'newline for some reason)
;;;;    10-Jan-1998 (CT) Moved most Control-Keys to Alt-Keys
;;;;    13-Jan-1998 (CT) Corrected `lse-tpu-keys:shifted'
;;;;    12-Feb-1998 (CT) [?\s-c], [?\s-v], and [?\s-x] defined analogously to
;;;;                     the corresponding control-keys in Windows programs
;;;;    22-Mar-1998 (CT) Substituted definitions for [<color> ?ß] by
;;;;                     definitions for [<color> ?:] (ß didn't work)
;;;;     9-Apr-1998 (CT) lse-frame:make-small added
;;;;    29-Apr-1998 (CT) Replace-Binding for `print-buffer' and
;;;;                     `print-region' (by `lpr-*')
;;;;    10-Jun-1998 (CT) [?\A-'] bound to 'lse-insert-bquotes
;;;;    12-Jan-1999 (CT) Bound `lse-split-line-i' instead of `lse-split-line'
;;;;    27-Jul-1999 (CT) [?\H-v] and [?\H-V] bound to
;;;;                     'lse-align-to-next-word-and-up and
;;;;                     'lse-align-to-previous-word-and-down
;;;;    28-Dec-1999 (CT) Definition of [blue gold kp-7] changed to
;;;;                     `lse-scroll-to-top'
;;;;    28-Dec-1999 (CT) [blue up] and [blue down] redefined to
;;;;                     `window-all-frames' functions
;;;;     1-Jan-2000 (CT) `repeat' bound to [\?C-z]
;;;;     1-Jan-2000 (CT) Bindings for `lse-*-register' added
;;;;     3-Jan-2000 (CT) Binding for `dabbrev-completion' added
;;;;    20-Jan-2000 (CT) [?\s-|] bound to `lse-insert-bars'
;;;;    20-Jan-2000 (CT) [?\s-<] bound to `lse-insert-angles'
;;;;    17-Jun-2001 (CT) [?\A-\s-.] and [?\C-\s-.] bound to `lse-tpu:unselect'
;;;;     6-Jan-2002 (CT) Redefined [?\C-k] to `lse-tpu:delete-tail-of-line`
;;;;     6-Jan-2002 (CT) Don't redefine [?\C-d] (define [?\s-\C-d], instead)
;;;;     6-Jan-2002 (CT) [?\C-w] bound to `lse-tpu:delete-prev-bs-word`
;;;;     6-Jan-2002 (CT) Gold [?\A-j], [?\C-k], [?\C-w] bindings to undelete
;;;;                     functions added
;;;;    29-May-2002 (CT) [home] bound to `beginning-of-line` for minibuffers
;;;;    25-Aug-2002 (CT) lse-define-cursor-movements and
;;;;                     lse-define-deletion-keys added
;;;;    25-Aug-2002 (CT) Various cleanups
;;;;    29-Aug-2002 (CT) `@set-smk` factored and used inside
;;;;                     `lse-define-deletion-keys`
;;;;    30-Aug-2002 (CT) `lse-define-insertion-keys` added
;;;;    30-Aug-2002 (CT)  Various key definitions added
;;;;     1-Sep-2002 (CT) `lse-tpu:define-keypad-num`,
;;;;                     `lse-tpu:define-keypad-app`,
;;;;                     `lse-tpu:define-electric-inserts`, and
;;;;                     `lse-tpu:redefine-some-control-keys` factored and
;;;;                     called conditionally by `lse-define-tpu-keys`
;;;;     7-Sep-2002 (CT) Bind `lse-frame:set-width:nnn` instead of lambda
;;;;                     functions
;;;;     8-Sep-2002 (CT) Bind `lse-frame:set-height:nnn` instead of lambda
;;;;                     functions
;;;;    13-Sep-2002 (CT) `[C-tab]` and `[\?A-\ ]` defined as `lse-tabulator`
;;;;                     (after defining `[tab]` as `lse-goto-next-fill-in`)
;;;;    22-Oct-2002 (CT) Definition of `[C-tab]` removed (done in
;;;;                     lse-keys-v19.el)
;;;;    12-Nov-2002 (MG) Added binding for `C-,` to `lse-tpu:unselect`
;;;;    12-Nov-2002 (CT) Added bindings for `[gold \?C-,]` and `[blue \?C-,]`
;;;;    14-Nov-2002 (CT) Added bindings for `[gold \?C-n]` and `[gold \?C-p]`
;;;;    21-Nov-2002 (CT) More control-key bindings moved into
;;;;                     `lse-tpu:redefine-some-control-keys`
;;;;    21-Nov-2002 (CT) Swapped meaning of `C` and `M` prefixes for
;;;;                     insertion and deletion keys
;;;;    31-Mar-2003 (CT) `[?\C-x ?5 ?1]` undefined (don't want
;;;;                     `delete-other-frames` bound to an easily reachable
;;;;                     key combination)
;;;;    18-May-2003 (CT) `[M-up]` and `[M-Down]` defined as previous/next
;;;;                     paragraph
;;;;     2-May-2006 (CT) Added bindings for `[gold] "S"` and `[blue gold] "D"`
;;;;    28-Mar-2007 (CT) Added bindings for "`[gold] J"` and `[blue] "J"`
;;;;     2-Oct-2007 (CT) Binding for `[insert]` enabled
;;;;     3-Oct-2007 (CT) `lse-tpu:app-keypad-p`,
;;;;                     `lse-tpu:define-keypad-app`,
;;;;                     `lse-create-lse-keymaps`, `lse-create-map`, and
;;;;                     `lse-create-sparse-map` removed
;;;;     4-Oct-2007 (CT) Use `lse-tpu:next-end-of-line` instead of
;;;;                     `lse-tpu:end-of-line`
;;;;     7-Oct-2007 (CT) Binding for `lse-tpu:replace:goto-next`
;;;;                     (`[blue ?\C-n]`) and `lse-tpu:replace:goto-prev`
;;;;                     (`[blue ?\C-p]`) added
;;;;     9-Oct-2007 (CT) s/lse-tpu:word-search-.*ward/lse-tpu:change-search-mode/
;;;;    11-Oct-2007 (CT) Added bindings for `[A-end]` and `[A-home]`
;;;;     8-Dec-2007 (CT) Added bindings (`[?\s-q]`, `<blue> <gold> q`) for
;;;;                     `lse-insert-buffer-name-plus-extension` added
;;;;     3-Apr-2008 (CT) Added bindings for `lse-scroll-to-bottom`
;;;;     8-Dec-2009 (CT) Binding for `[blue gold ?*]` changed from
;;;;                     `lse-show-windows` to `lse-frame:list:show`
;;;;     9-Dec-2009 (CT) Bindings of `[blue gold ?*]` and `[blue ?*]` swapped
;;;;    ««Revision-date»»···
;;;;--
(provide 'lse-tpu-keys-v19)

(defun lse-define-alpha-key (map prefix alpha command)
  ;; Defines a key bound to a alphabetic key.
  ;; Both lower and upper case are defined.
  (let* ((chr     (if (stringp alpha) (string-to-char alpha) alpha))
         (upkey   (vconcat prefix (list (upcase   chr))))
         (downkey (vconcat prefix (list (downcase chr))))
       )
    (if (lookup-key map upkey)
        ;; minimize number of definitions!
        ;; originally this was unconditionally: (define-key map upkey command)
        (define-key map upkey nil)
      )
    (define-key map downkey command)
  )
; lse-define-alpha-key
)

(defun gset-alpha-key (prefix alpha command)
  (lse-define-alpha-key global-map prefix alpha command)
; gset-alpha-key
)

(defun lse-tpu-keys:shifted (k)
  (if (symbolp k)
      (intern (concat "S-" (symbol-name k)))
    (if (integerp k)
        (let* ((k-modifiers  (event-modifiers  k))
               (k-unmodified (event-basic-type k))
              )
          (cond ((memq 'shift k-modifiers)
                 (delq 'shift k-modifiers)
                 (event-convert-list (append k-modifiers (list k-unmodified)))
                )
                ((< k-unmodified ?\ ) ; if it is a control control-char
                 (event-convert-list (append 'shift k-modifiers k-unmodified))
                )
                ((and (>= k-unmodified ?a) (<= k-unmodified ?z))
                 (event-convert-list
                   (append k-modifiers (list (upcase k-unmodified)))
                 )
                )
                (t nil)
          )
        )
    )
  )
; lse-tpu-keys:shifted
)

;;; 29-Aug-2002
(defun @set-smk (key command key-set-fct)
  "Define a key for which the shifted version activates the mark."
  (if (symbolp command)
      (progn
        (let* ((i    (1- (length key)))
               (k    (aref key i))
               (sk   (lse-tpu-keys:shifted k))
               (skey (copy-sequence key))
              )
          (aset skey i sk)
          (apply key-set-fct (list key  command))
          (if sk
              (apply key-set-fct (list skey command))
          )
        )
      )
    (error "Key %s is not bound to a lisp-symbol, but to %s" key command)
  )
; @set-smk
)

;;; 29-Aug-2002
(defun global@set-smk (key command)
  "Define a global key for which the shifted version activates the mark."
  (@set-smk key command 'global-set-key)
; global@set-smk
)

;;; 13-Apr-1998
(defun set-smk (key command key-set-fct)
  "Define a key for which the shifted version activates the mark."
  (@set-smk key command key-set-fct); 29-Aug-2002
  (put command 'shift-mark t)
; set-smk
)

;;; 13-Apr-1998
(defun local-set-smk (key command)
  "Define a local key for which the shifted version activates the mark."
  (set-smk key command 'local-set-key)
)

;;; 29-Dec-1997
(defun global-set-smk (key command)
  "Define a global key for which the shifted version activates the mark."
  (set-smk key command 'global-set-key)
; global-set-smk
)

;;; 30-Dec-1997
(defun global-mak-smk (key)
  "Define the shifted version of a global `key' to activate the mark."
  (let* ((i       (1- (length key)))
         (k       (aref key i))
         (sk      (lse-tpu-keys:shifted k))
         (skey    (copy-sequence key))
         (command (global-key-binding key))
        )
    (if (symbolp command)
        (progn
          (aset skey i sk)
          (if sk
              (global-set-key skey command)
          )
          (put command 'shift-mark t)
        )
      (error "Key %s is not bound to a lisp-symbol, but to %s" key command)
    )
  )
; global-mak-smk
)

(defun lse-toggle-key-with-gold (key &optional keymap)
  "Interchanges definition of KEY with that of GOLD KEY."
  (interactive "kPress key to be toggled ")
  (let* ((map          (or keymap global-map))
         (gold-key     (vconcat [gold] key))
         (binding      (lookup-key map key))
         (gold-binding (lookup-key map gold-key))
        )
    (define-key map key      gold-binding)
    (define-key map gold-key binding)
    (message "Toggled `%s'" key)
  )
; lse-toggle-key-with-gold
)

;;; 30-Dec-1997
(defun lse-move-key-to-prefix (keymap prefix key)
  "Moves the definition of KEY in KEYMAP to [PREFIX KEY]."
  (let* ((map          (or keymap global-map))
         (pref-key     (vconcat prefix key))
         (binding      (lookup-key map key))
         (pref-binding (lookup-key map pref-key))
        )
    (if binding
        (if (and pref-binding (not (eq binding pref-binding)))
            (message "Key %s is already bound to `%s'" pref-key pref-binding)
          (define-key map pref-key binding)
          (define-key map key      nil)
        )
    )
  )
; lse-move-key-to-prefix
)

;;; 30-Dec-1997
(defun lse-move-keys-to-prefix (keymap prefix &rest keys)
  "Moves the definition of KEYS in KEYMAP to [PREFIX KEY]."
  (mapcar (function (lambda (k) (lse-move-key-to-prefix keymap prefix k)))
          keys
  )
; lse-move-keys-to-prefix
)
(defun lse-iterate-keys-bound-to-function (binding do)
  "Calls `do' for every key of current keymap bound to `binding'."
  (mapcar do (where-is-internal binding))
; lse-iterate-keys-bound-to-function
)
;;; example call:
;;;   (lse-iterate-keys-bound-to-function 'self-insert-command 'test)
;;;   (defun test (k)
;;;      (message "Key %s" k)
;;;      (sit-for 2)
;;;   )
;;; or even better
;;;  (lse-iterate-keys-bound-to-function
;;;     'self-insert-command
;;;     (function (lambda (k)
;;;                 (message "Key %s" k)
;;;                 (sit-for 2)
;;;               )
;;;     )
;;;  )
(defun lse-replace-binding-for-all-keys (old-binding new-binding)
  (lse-iterate-keys-bound-to-function
     old-binding (function (lambda (k) (global-set-key k new-binding)))
  )
; lse-replace-binding-for-all-keys
)

;;;  4-Jan-1998
(defun lse-copy-key-def (map from to)
  (let ((binding (lookup-key map from)))
    (if binding (define-key map to binding))
  )
; lse-copy-key-def
)

;;;  4-Jan-1998
(defun lse-copy-key-in-minibuffer-maps (from to)
  (lse-copy-key-def minibuffer-local-map              from to)
  (lse-copy-key-def minibuffer-local-ns-map           from to)
  (lse-copy-key-def minibuffer-local-completion-map   from to)
  (lse-copy-key-def minibuffer-local-must-match-map   from to)
  (lse-copy-key-def read-expression-map               from to)
; lse-copy-key-in-minibuffer-maps
)

(defun lse-define-key-in-minibuffer-maps       (key binding)
  (define-key minibuffer-local-map              key binding)
  (define-key minibuffer-local-ns-map           key binding)
  (define-key minibuffer-local-completion-map   key binding)
  (define-key minibuffer-local-must-match-map   key binding)
  (define-key read-expression-map               key binding)
; lse-define-key-in-minibuffer-maps
)

(defun lse-define-key-in-all-maps (key binding)
  (global-set-key                           key binding)
  (lse-define-key-in-minibuffer-maps        key binding)
; lse-define-key-in-all-maps
)

;;;  1-Sep-2002
(defun lse-tpu:define-keypad-num ()
  (global-set-key  [kp-0]            'lse-insert-num-0)
  (global-set-key  [kp-1]            'lse-insert-num-1)
  (global-set-key  [kp-2]            'lse-insert-num-2)
  (global-set-key  [kp-3]            'lse-insert-num-3)
  (global-set-key  [kp-4]            'lse-insert-num-4)
  (global-set-key  [kp-5]            'lse-insert-num-5)
  (global-set-key  [kp-6]            'lse-insert-num-6)
  (global-set-key  [kp-7]            'lse-insert-num-7)
  (global-set-key  [kp-8]            'lse-insert-num-8)
  (global-set-key  [kp-9]            'lse-insert-num-9)
  (global-set-key  [kp-subtract]     'lse-insert-num-minus)
  (global-set-key  [kp-separator]    'lse-insert-num-comma)
  (global-set-key  [kp-decimal]      'lse-insert-num-point)
; lse-tpu:define-keypad-num
)

;;;  1-Sep-2002
(defun lse-tpu:define-electric-inserts ()
  (global-set-key  "("               'lse-insert-parentheses)
  (global-set-key  "["               'lse-insert-brackets)
  (global-set-key  "{"               'lse-insert-braces)
  (global-set-key  ","               'lse-insert-comma)
  (global-set-key  ";"               'lse-insert-semicolon)
  (global-set-key  "\""              'lse-insert-dquotes)
; lse-tpu:define-electric-inserts
)

;;;  1-Sep-2002
(defun lse-tpu:redefine-some-control-keys ()
  (lse-define-key-in-all-maps [?\C-w]    'lse-tpu:delete-prev-bs-word);  6-Jan-2002
  (global-set-smk             [?\C-f]    'lse-tpu:search-forward);  5-Oct-2007
  (global-set-smk             [?\s-F]    'lse-tpu:search-reverse);  5-Oct-200)
  (global-set-smk             [?\C-n]    'lse-tpu:search-again-forward); 31-Aug-2002
  (global-set-smk             [?\C-o]    'lse-open-line); 31-Aug-2002
  (global-set-smk             [?\C-p]    'lse-tpu:search-again-reverse); 31-Aug-2002
  (global-set-key             [?\C-,]    'lse-tpu:unselect); 12-Nov-2002
  (global-set-key             [?\C-']    'lse-insert-backquote-quote); 10-Jan-1998
  (global-set-key             [?\C-|]    'lse-fill-range); 10-Jan-1998
  (global-set-key             [?\C->]    'lse-unset-selective-display);  8-Sep-2002
  (global-set-key             [?\C-^]    'lse-frame:set-width:80); 10-Jan-1998
  (global-set-key             [?\C-`]    'lse-frame:set-width:132);  7-Sep-2002
  (global-set-key             [?\C-!]    'lse-frame:set-height:48);  8-Sep-2002
  (global-set-key             [?\C-\s-.] 'lse-tpu:unselect); 17-Jun-2001
  (global-set-key             [?\C-:]    'lse-tpu:replace); 30-Aug-2002
  (if (fboundp 'repeat)
      (global-set-key  [?\C-z] 'repeat);  1-Jan-2000
  )
; lse-tpu:redefine-some-control-keys
)

(defun lse-define-tpu-keys ()
  "Redefine keys defined by standard EMACS modes"
  (lse-tpu:define-keypad-num);  3-Oct-2007
  (if lse-tpu:electric-inserts-p
      (lse-tpu:define-electric-inserts)
  )
  (if lse-tpu:use-control-keys-p
      (lse-tpu:redefine-some-control-keys)
  )
  (lse-define-key-in-all-maps        [?\A-_]     'undo); 10-Jan-1998
  (lse-define-key-in-all-maps        [?\C-d]     'lse-tpu:delete-next-char);  6-Jan-2002
  (lse-define-key-in-minibuffer-maps [?\A-g]     'abort-recursive-edit); 10-Jan-1998
  (lse-define-key-in-all-maps        [?\A-j]     'lse-tpu:delete-prev-word); 10-Jan-1998
  (lse-define-key-in-all-maps        [?\s-\A-j]  'lse-tpu:delete-prev-word-append); 10-Jan-1998
  (lse-define-key-in-all-maps        [?\C-k]     'lse-tpu:delete-tail-of-line);  6-Jan-2002
  (lse-define-key-in-minibuffer-maps [?\A-e]     'lse-tpu:current-end-of-line)

  (lse-define-key-in-minibuffer-maps [up]        'lse-tpu:previous-history-element)
  (lse-define-key-in-minibuffer-maps [down]      'lse-tpu:next-history-element)
  (lse-define-key-in-minibuffer-maps [tab]       'minibuffer-complete); 29-Dec-1997
  (lse-define-key-in-minibuffer-maps [do]        'lse-tpu:previous-history-element)
  (lse-define-key-in-minibuffer-maps [find]      'previous-matching-history-element)
  (lse-define-key-in-minibuffer-maps [gold find] 'next-matching-history-element)
  (lse-define-key-in-minibuffer-maps [home]      'beginning-of-line); 29-May-2002

  (lse-copy-key-in-minibuffer-maps   [?\C-m]     [return]);  4-Jan-1998
  (lse-copy-key-in-minibuffer-maps   [?\C-m]     [?\A-m]);  10-Jan-1998

  ;;  1-Sep-2002
  ;; Define [?\A-<i>] as lse-set-tab-increment-i
  (let ((i 1))
    (while (<= i 9)
      (global-set-key
        (car (read-from-string (format "[?\\A-%d]" i)))
        'lse-set-tab-increment-i
      )
      (setq i (1+ i))
    )
  )
  (global-set-key  [?\A-']           'lse-insert-bquotes); 10-Jun-1998
  (global-set-key  [?\A-|]           'lse-fill-range); 28-Apr-1996
  (global-set-key  [?\A-<]           'lse-set-selective-display);    8-Sep-2002
  (global-set-key  [?\A-^]           'lse-frame:set-width:80); 15-Jul-1997
  (global-set-key  [?\A-`]           'lse-frame:set-width:132);  7-Sep-2002
  (global-set-key  [?\A-!]           'lse-frame:set-height:48);  8-Sep-2002
  (global-set-key  [?\A-\s-.]        'lse-tpu:unselect); 17-Jun-2001
  (global-set-key  [?\A-,]           'lse-tpu:select); 12-Nov-2002

  (global-mak-smk  [?\C-a])
  (global-set-key  [?\A-a]           'lse-tpu:toggle-overwrite-mode)
  (global-set-key  [?\s-a]           'delete-selection-mode); 28-Dec-1997
  (global-set-key  [?\s-c]           'lse-tpu:copy-region); 12-Feb-1998
  (global-set-key  [?\A-d]           'dabbrev-expand)
  (global-set-key  [?\C-\A-d]        'dabbrev-completion);  3-Jan-2000
  (global-set-key  [?\s-\A-d]        'ispell-complete-word);  6-Jan-2002
  (global-set-key  [?\s-d]           'hippie-expand)       ; 29-Dec-1997
  (global-mak-smk  [?\C-e])
  (global-set-smk  [?\A-f]           'lse-compilation:next-error) ; 20-Feb-1995
  (global-set-key  [?\A-g]           'keyboard-quit); 29-Dec-1997
  (global-set-smk  [?\A-h]           'lse-tpu:next-beginning-of-line)
  (global-set-key  [?\A-i]           'lse-tabulator); 19-Mar-1995
  (global-set-key  [?\C-i]           'lse-tabulator); 13-Sep-2002
  (global-set-key  [?\A-l]           'lse-tpu:insert-formfeed)
  (global-set-key  [?\M-l]           'goto-line); 31-Aug-2002
  (global-set-key  [?\A-q]           'lse-insert-buffer-name); 28-Apr-1996
  (global-set-key  [?\s-q]           'lse-insert-buffer-name-plus-extension);  8-Dec-2007
  (global-set-key  [?\M-r]           'lse-scroll-to-top);  1-Sep-2002
  (global-set-key  [?\M-R]           'lse-scroll-to-bottom);   3-Apr-2008
  (global-set-key  [?\A-t]           'transpose-chars); 10-Jan-1998
  (global-set-key  [?\A-u]           'lse-tpu:delete-head-of-line)
  (global-set-key  [?\s-\A-u]        'lse-tpu:delete-head-of-line-append); 17-Dec-1997
  (global-set-key  [?\A-v]           'lse-align-and-down); 15-Sep-1995
  (global-set-key  [?\H-V]           'lse-align-to-previous-word-and-down) ; 27-Jul-1999
  (global-set-key  [?\H-v]           'lse-align-to-next-word-and-up) ; 27-Jul-1999
  (global-set-key  [?\M-v]           'lse-align-and-up); 15-Sep-1995
  (global-set-key  [?\s-v]           'lse-tpu:paste-region); 12-Feb-1998
  (global-set-key  [?\A-w]           'redraw-display)
  (global-set-key  [?\s-x]           'lse-tpu:cut-region); 12-Feb-1998
  (global-set-key  [?\C-x ?5 ?3]     'lse-frame:make-small);  9-Apr-1998
  (global-unset-key [?\C-x ?5 ?1]); 31-Mar-2003
  (global-set-key  [?\A-\\]          'quoted-insert)
  (global-set-key  [?\C-\.]          'universal-argument) ; 28-Jun-1995
  (global-set-key  [?\A-\.]          'universal-argument) ; 30-Dec-1997
  (global-set-key  [?\A-\-]          'negative-argument)  ; 30-Dec-1997
  (global-set-key  [?\C-\#]          'lse-number-to-register);  1-Jan-2000
  (global-set-key  [?\s-\#]          'lse-increment-register);  1-Jan-2000
  (global-set-key  [?\M-\#]          'lse-increment-register);  1-Jan-2000
  (global-set-key  [?\A-\#]          'lse-insert-register);     1-Jan-2000
  (global-set-key  [?\s-|]           'lse-insert-bars); 20-Jan-2000
  (global-set-key  [?\s-<]           'lse-insert-angles); 20-Jan-2000
  (global-set-key  [?\A-:]           'lse-tpu:replace-all); 30-Aug-2002
  (global-set-key  [?\A-\ ]          'lse-tabulator); 13-Sep-2002
  (local-unset-key [?\C-i])
  (local-unset-key "\177")
  (local-unset-key [backspace])

  (define-key      help-map [red]    'lse-tpu-keys:show-list-sexp-keys)
  (define-key      help-map [?W]     'lse-tpu-keys:show-keys-matching); 28-Dec-97
  (global-set-key  [do]              'repeat-complex-command)

  (global-set-smk  [home]            'lse-tpu:next-beginning-of-line)
  (global-set-key  [red home]        'lse-scroll-to-top);  3-Apr-2008
  (global-set-smk  [end]             'lse-tpu:next-end-of-line)
  (global-set-key  [red end]         'lse-scroll-to-bottom);  3-Apr-2008
  (global-set-key  [cancel]          'lse-tpu:unselect)
  (global-set-key  [select]          'lse-tpu:select)
  (global-set-key  [delete]          'lse-tpu:delete-next-char); 25-Feb-1998
  (global-set-smk  [prior]           'lse-previous-screen-2)
  (global-set-smk  [next]            'lse-next-screen-2)
  (global-set-key  [A-prior]         'lse-scroll-other-window-back); 18-Jul-95
  (global-set-key  [A-next]          'lse-scroll-other-window-forw); 18-Jul-95

  (global-set-smk  [up]              'lse-tpu:previous-line)
  (global-set-smk  [down]            'lse-tpu:next-line)
  (global-set-smk  [left]            'lse-tpu:backward-char)
  (global-set-smk  [right]           'lse-tpu:forward-char)

  (global-set-key  [A-right]         'lse-indent-line-by-word);   18-Jul-1995
  (global-set-key  [A-left]          'lse-deindent-line-by-word); 18-Jul-1995
  (global-set-key  [A-up]            'enlarge-window);            18-Jul-1995
  (global-set-key  [A-down]          'shrink-window);             18-Jul-1995
  (global-set-key  [A-S-right]       'lse-tpu:pan-right);         20-Aug-1995
  (global-set-key  [A-S-left]        'lse-tpu:pan-left);          20-Aug-1995

  (global-set-key  [A-return]        'lse-split-line-i);          27-Mar-1997
  (global-set-key  [gold A-return]   'lse-toggle-lse-split-line); 27-Mar-1997
; lse-define-tpu-keys
)

(defun lse-replace-std-emacs-bindings ()
  (fset 'Replace-Binding 'lse-replace-binding-for-all-keys)

  (Replace-Binding 'delete-backward-char      'lse-tpu:delete-prev-char)
  (Replace-Binding 'backward-kill-word        'lse-tpu:delete-prev-word);  6-Jan-2002
  (Replace-Binding 'delete-char               'lse-tpu:delete-next-char)
  (Replace-Binding 'delete-other-windows      'lse-delete-other-windows)
  (Replace-Binding 'delete-window             'lse-delete-window)
  (Replace-Binding 'find-alternate-file       'lse-visit-alternate-file)
  (Replace-Binding 'find-file                 'lse-visit-file)
  (Replace-Binding 'find-file-other-window    'lse-visit-file-other-window)
  (Replace-Binding 'kill-buffer               'lse-kill-buffer)
  ;;(Replace-Binding 'list-buffers              'lse-show-buffers)
  (Replace-Binding 'revert-buffer             'lse-revert-buffer)
  (Replace-Binding 'set-visited-file-name     'lse-change-output-file)
  (Replace-Binding 'shell-command             'lse-shell-command)
  (Replace-Binding 'split-window              'lse-split-window)
  (Replace-Binding 'split-window-horizontally 'lse-split-window-horizontally)
  (Replace-Binding 'split-window-vertically   'lse-split-window)
  (Replace-Binding 'switch-to-buffer          'lse-goto-buffer)
  (Replace-Binding 'switch-to-buffer-other-window
                                              'lse-goto-buffer-other-window)
  (Replace-Binding 'toggle-read-only          'lse-set-buffer-nowrite)

  (Replace-Binding 'print-buffer              'lpr-buffer); 29-Apr-1998
  (Replace-Binding 'print-region              'lpr-region); 29-Apr-1998

  (fmakunbound 'Replace-Binding)

  ;;  1-Sep-2002
  ;; Define [?\M-<i>] as 'negative-digit-argument
  (let ((i 0))
    (while (<= i 9)
      (global-set-key
        (car (read-from-string (format "[?\\M-%d]" i)))
        'negative-digit-argument
      )
      (setq i (1+ i))
    )
  )

; lse-replace-std-emacs-bindings
)

(defun lse-define-tpu-gold-keys ()
  "Define Gold-Keys and related keys"
  (lse-define-key-in-minibuffer-maps [gold ??]   'minibuffer-completion-help)

  (global-set-key [gold ?*]           'lse-show-buffers)
  (global-set-key [gold ?,]           'self-insert-command)
  (global-set-key [gold ?.]           'self-insert-command)
  (global-set-key [gold ?/]           'lse-align-and-up)
  (global-set-key [gold ?<]           'lse-tpu:pan-left)
  (global-set-key [gold ?>]           'lse-tpu:pan-right)
  (global-set-key [gold ?=]           'lse-split-window)
  (global-set-key [gold ??]           'lse-insert-key-definition)
  (global-set-key [gold ?\ ]          'lse-align-to-next-word)
  (global-set-key [gold ?\"]          'self-insert-command)
  (global-set-key [gold ?\']          'lse-insert-squotes)
  (global-set-key [gold ?\(]          'self-insert-command)
  (global-set-key [gold ?\)]          'backward-sexp)
  (global-set-key [gold ?\;]          'self-insert-command)
  (global-set-key [gold ?\[]          'self-insert-command)
  (global-set-key [gold ?\\]          'self-insert-command)
  (global-set-key [gold ?\]]          'backward-sexp)
  (global-set-key [gold ?`]           'lse-frame:set-width:132); 5-Mar-1997
  (global-set-key [gold ?^]           'lse-frame:set-width:132); 5-Mar-1997
  (global-set-key [gold ?!]           'lse-frame:set-height:72);  8-Sep-2002
  (global-set-key [gold ?:]           'lse-frame:set-height:30);  8-Sep-2002
  (global-set-key [gold ?{]           'self-insert-command)
  (global-set-key [gold ?|]           'lse-fill-range)
  (global-set-key [gold ?}]           'backward-sexp)
  (global-set-key [gold ?~]           'vt-narrow)
  (global-set-key [gold ?+]           'lse-push-window-configuration)

  (gset-alpha-key [gold] "A"          'occur)
  (gset-alpha-key [gold] "B"          'lse-goto-last-mark-global)
  (gset-alpha-key [gold] "C"          'lse-tpu:capitalize-weakly)
  (gset-alpha-key [gold] "D"          'lse-insert-dd-mmm-yyyy+blank)
  (gset-alpha-key [gold] "E"          'undefined)
  (gset-alpha-key [gold] "F"          'lse-goto-buffer)
  (gset-alpha-key [gold] "G"          'lse-goto-mark-and-pop-global)
  (gset-alpha-key [gold] "H"          'lse-goto-home-mark-global)
  (gset-alpha-key [gold] "I"          'lse-visit-file)
  (gset-alpha-key [gold] "J"          'lse-frame:toggle-menu-bar); 28-Mar-2007
  (gset-alpha-key [gold] "K"          'undefined)
  (gset-alpha-key [gold] "L"          'lse-tpu:change-case-lower)
  (gset-alpha-key [gold] "M"          'lse-push-mark-global)
  (gset-alpha-key [gold] "N"          'lse-goto-next-buffer)
  (gset-alpha-key [gold] "O"          'lse-change-output-file)
  (gset-alpha-key [gold] "P"          'lse-insert-buffer)
  (gset-alpha-key [gold] "Q"          'lse-insert-buffer-name)
  (gset-alpha-key [gold] "R"          'lse-tpu:toggle-rectangle)
  (gset-alpha-key [gold] "S"          'lse-insert-time+blank);  2-May-2006
  (gset-alpha-key [gold] "T"          'lse-toggle-mark-global)
  (gset-alpha-key [gold] "U"          'lse-tpu:change-case-upper)
  (gset-alpha-key [gold] "V"          'lse-align-to-next-word-and-up)
  (gset-alpha-key [gold] "W"          'lse-write-buffer)
  (gset-alpha-key [gold] "X"          'suspend-emacs)
  (gset-alpha-key [gold] "Y"          'eval-last-sexp)
  (gset-alpha-key [gold] "Z"          'lse-insert-user-initials)

  (global-set-key [gold ?\A-d]        'ispell-complete-word);  4-Nov-1996
  (global-set-key [gold ?\C-d]        'lse-tpu:undelete-char);  6-Jan-2002
  (global-set-key [gold ?\C-f]        'lse-tpu:goto-pos-before-search); 31-Aug-2002
  (global-set-key [gold ?\A-i]        'lse-indent-line); 19-Mar-1995
  (global-set-key [gold tab]          'lse-indent-line); 19-Mar-1995
  (global-set-key [gold ?\A-j]        'lse-tpu:undelete-word);  6-Jan-2002
  (global-set-key [gold ?\C-k]        'lse-tpu:undelete-line);  6-Jan-2002
  (global-set-key [gold ?\C-l]        'lse-scroll-to-top); 31-Aug-2002
  (global-set-key [gold ?\M-l]        'lse-show-position); 31-Aug-2002
  (global-set-key [gold ?\A-m]        'newline)
  (global-set-key [gold ?\C-n]        'lse-tpu:goto-pos-before-search); 14-Nov-2002
  (global-set-key [gold A-return]     'lse-toggle-lse-split-line); 27-Mar-1997
  (global-set-key [gold ?\C-p]        'lse-tpu:goto-pos-before-search); 14-Nov-2002
  (global-set-key [gold ?\M-r]        'move-to-window-line);  1-Sep-2002
  (global-set-key [gold ?\A-u]        'lse-tpu:undelete-line);  8-Sep-2002
  (global-set-key [gold ?\C-w]        'lse-tpu:undelete-word);  8-Sep-2002
  (global-set-key [gold ?\C-,]        'lse-tpu:exchange-point-and-mark); 12-Nov-2002

  (global-set-key [gold gold]         'universal-argument)
  (global-set-key [gold blue]         'universal-argument)

  (global-set-key [gold right]        (lse-key-cmd (lse-indent-rigidly (lse-tab-increment))))
  (global-set-key [gold left]         (lse-key-cmd (lse-indent-rigidly (- (lse-tab-increment)))))
  (global-set-key [gold up]           'lse-previous-window)
  (global-set-key [gold down]         'lse-next-window)

  ;; 25-Feb-1998 ;; `home' instead of `insert'
  (global-set-key [gold home]         'lse-select-current-line); 7-Apr-1997
  (global-set-key [gold delete]       'lse-tpu:undelete-char)
  (global-set-key [gold select]       'lse-tpu:unselect)
  (global-set-key [gold cancel]       'lse-tpu:exchange-point-and-mark)
  (global-set-key [gold prior]        'lse-previous-screen)
  (global-set-key [gold next]         'lse-next-screen)

  (global-set-key [gold do]           'lse-command:do); 31-Aug-2002
  (global-set-key [red  do]           'undo);           31-Aug-2002
; lse-define-tpu-gold-keys
)

(defun lse-define-tpu-blue-keys ()
  "Define BLUE-keys"
  (global-set-key [blue blue]        'lse-ring-bell)

  (global-set-key [blue find]        'find-tag)
  (global-set-key [blue gold find]   'lse-grep); 20-Feb-1995
  (global-set-key [blue home]        'lse-indent:goto-indent-pos); 7-Apr-1997 ;  9-Jun-1995
  (global-set-key [blue insert]      'lse-tpu:duplicate-previous-bs-word)
  (global-set-key [blue delete]      'lse-tpu:duplicate-previous-char); 25-Feb-1998
  (global-set-key [blue cancel]      'lse-tpu:exchange-point-and-mark); 26-Aug-2002
  (global-set-key [blue gold cancel] 'lse-blink-select-mark); 26-Aug-2002
  (global-set-key [blue select]      'lse-tpu:exchange-point-and-mark);  8-Sep-2002
  (global-set-key [blue gold select] 'lse-select-current-bs-word)

  (global-set-key [blue prior]       'lse-scroll-window-back)
  (global-set-key [blue next]        'lse-scroll-window-forw)
  (global-set-key [blue gold prior]  'lse-scroll-other-window-back)
  (global-set-key [blue gold next]   'lse-scroll-other-window-forw)

  (global-set-key [blue right]       (lse-key-cmd (lse-indent-rigidly (*  3 (lse-tab-increment)))))
  (global-set-key [blue left]        (lse-key-cmd (lse-indent-rigidly (* -3 (lse-tab-increment)))))
  (global-set-key [blue gold right]  'lse-indent-line-by-word)
  (global-set-key [blue gold left]   'lse-deindent-line-by-word)
  (global-set-key [blue up]          'lse-previous-window-all-frames); 28-Dec-1999 lse-previous-window
  (global-set-key [blue down]        'lse-next-window-all-frames); 28-Dec-1999 lse-next-window
  (global-set-key [blue gold up]     'enlarge-window)
  (global-set-key [blue gold down]   'shrink-window)
  (global-set-key [blue help]        'describe-key-briefly)
  (global-set-key [blue gold help]   'help-command)
  (global-set-key [blue do]          'execute-extended-command)

  (gset-alpha-key [blue]      "A"    'occur)
  (gset-alpha-key [blue]      "B"    'lse-goto-last-mark-window)
  (gset-alpha-key [blue gold] "B"    'lse-goto-last-mark-buffer)
  (gset-alpha-key [blue]      "C"    'lse-tpu:capitalize-strongly)
  (gset-alpha-key [blue gold] "C"    'lse-tpu:change-case); 31-Aug-2002
  (gset-alpha-key [blue]      "D"    'lse-insert-dd-mm-yyyy+blank)
  (gset-alpha-key [blue gold] "D"    'lse-insert-yyyy/mm/dd+blank);  2-May-2006
  (gset-alpha-key [blue gold] "E"    'lse-kill-buffer)
  (gset-alpha-key [blue]      "F"    'lse-goto-buffer+maybe-create)
  (gset-alpha-key [blue gold] "F"    'lse-goto-buffer-other-window)
  (gset-alpha-key [blue]      "G"    'lse-goto-mark-and-pop-window)
  (gset-alpha-key [blue gold] "G"    'lse-goto-mark-and-pop-buffer)
  (gset-alpha-key [blue]      "H"    'lse-goto-home-mark-window)
  (gset-alpha-key [blue gold] "H"    'lse-set-home-mark-window)
  (gset-alpha-key [blue]      "I"    'lse-visit-file-other-window)
  (gset-alpha-key [blue gold] "I"    'lse-insert-file)
  (gset-alpha-key [blue]      "J"    'lse-menu:toggle-menu-bar); 28-Mar-2007
  (gset-alpha-key [blue]      "L"    'lse-learn-key)
  (gset-alpha-key [blue gold] "L"    'lse-learn-named-key)
  (gset-alpha-key [blue]      "M"    'lse-push-mark-window)
  (gset-alpha-key [blue gold] "M"    'lse-push-mark-buffer)
  (gset-alpha-key [blue]      "N"    'lse-goto-prev-buffer)
  (gset-alpha-key [blue]      "O"    'lse-change-output-file-current)
  (gset-alpha-key [blue gold] "O"    'lse-make-directory)
  (gset-alpha-key [blue]      "P"    'lse-insert-buffer)
  (gset-alpha-key [blue]      "Q"    'lse-tpu:special-insert); 31-Aug-2002
  (gset-alpha-key [blue gold] "Q"    'lse-insert-buffer-name-plus-extension);  8-Dec-2007
  (gset-alpha-key [blue gold] "R"    'lse-revert-buffer);  8-Sep-2002
  (gset-alpha-key [blue]      "S"    'undefined)
  (gset-alpha-key [blue]      "T"    'lse-toggle-mark-window)
  (gset-alpha-key [blue gold] "T"    'lse-toggle-mark-buffer)
  (gset-alpha-key [blue]      "V"    'lse-align-to-previous-word-and-down)
  (gset-alpha-key [blue]      "W"    'lse-write-current-buffer)
  (gset-alpha-key [blue gold] "W"    'lse-save-some-buffers); 5-Apr-1998
  (gset-alpha-key [blue]      "X"    'lse-shell-command)
  (gset-alpha-key [blue]      "Y"    'lse-compile); 20-Feb-1995
  (gset-alpha-key [blue gold] "Y"    'lse-set-compile-command);  9-Jun-1995
  (gset-alpha-key [blue]      "Z"    'lse-insert-user-full-name)
  (gset-alpha-key [blue gold] "Z"    'lse-insert-user-name)

  (global-set-key [blue      ?#]     'lse-count-matches)
  (global-set-key [blue      ?*]     'lse-frame:list:show);  8-Dec-2009
  (global-set-key [blue gold ?*]     'lse-show-all-buffers);  9-Dec-2009
  (global-set-key [blue      ?/]     'lse-align-and-down)
  (global-set-key [blue      ?=]     'lse-split-window-horizontally); 22-May-97
  (global-set-key [blue      ??]     'lse-insert-key-name); 12-Jun-1994
  (global-set-key [blue      ?\ ]    'lse-align-to-previous-word)
  (global-set-key [blue      ?\(]    'lse-select-paren-range)
  (global-set-key [blue      ?\<]    'lse-select-angle-range)
  (global-set-key [blue      ?\[]    'lse-select-bracket-range)
  (global-set-key [blue      ?\{]    'lse-select-brace-range)
  (global-set-key [blue      ?|]     'lse-insert-bars)
  (global-set-key [blue      ?+]     'lse-pop+restore-window-configuration)
  (global-set-key [blue      ?`]     'lse-frame:set-width:80); 5-Mar-1997
  (global-set-key [blue      ?^]     'lse-frame:set-width:80); 5-Mar-1997
  (global-set-key [blue      ?!]     'lse-frame:set-height:48);  8-Sep-2002
  (global-set-key [blue      ?:]     'lse-frame:set-height:48);  8-Sep-2002
  (global-set-key [blue gold ?`]     'lse-frame:set-width:162);   5-Mar-1997
  (global-set-key [blue gold ?^]     'lse-frame:set-width:162);   5-Mar-1997
  (global-set-key [blue gold ?!]     'lse-frame:set-height:30);  8-Sep-2002
  (global-set-key [blue gold ?:]     'lse-frame:set-height:48);  8-Sep-2002
  (global-set-key [blue      ?']     'lse-insert-backquote-quote); 26-Apr-1996
  (global-set-key [blue      ?\"]    'lse-insert-double-backquote-quote); 26-Apr-1996
  (global-set-key [blue      ?~]     'lse-tpu:trim-line-ends)
  (global-set-key [blue gold ?-]     'lse-delete-other-windows)
  (global-set-key [blue gold ?=]     'lse-delete-window)
  (global-set-key [blue gold ?\ ]    'set-fill-column)
  (global-set-key [blue gold ?\(]    'lse-remove-parentheses)
  (global-set-key [blue gold ?\<]    'lse-select-guillemot-range)
  (global-set-key [blue gold ?\[]    'lse-remove-brackets)
  (global-set-key [blue gold ?\{]    'lse-remove-braces)
  (global-set-key [blue gold ?\"]    'lse-remove-double-backquote-quote); 26-Apr-1996
  (global-set-key [blue gold ?']     'lse-remove-backquote-quote); 26-Apr-1996
  (global-set-key [blue gold ?~]     'lse-fill-range)
  (global-set-key [blue ?-]          'negative-argument)
  (global-set-key [blue      ?\C-f]  'lse-tpu:change-search-mode);  9-Oct-2007
  (global-set-key [blue      ?\M-f]  'lse-grep); 31-Aug-2002
  (global-set-key [blue gold ?\M-f]  'find-tag); 31-Aug-2002
  (global-set-key [blue gold ?\C-f]  'lse-tpu:change-search-mode);  9-Oct-2007
  (global-set-key [blue gold ?\A-i]  'auto-fill-mode)
  (global-set-key [blue gold ?\C-i]  'auto-fill-mode)
  (global-set-key [blue      ?\A-j]  'lse-tpu:delete-prev-word-append)
  (global-set-key [blue      ?\A-m]  'lse-split-line-i)
  (global-set-key [blue      ?\C-m]  'lse-split-line-i); 20-Jan-1998
  (global-set-key [blue gold ?\A-m]  'lse-tpu:toggle-newline-and-indent)
  (global-set-key [blue gold ?\C-m]  'lse-tpu:toggle-newline-and-indent)
  (global-set-key [blue      ?\C-n]  'lse-tpu:replace:goto-next);  7-Oct-2007
  (global-set-key [blue      ?\C-p]  'lse-tpu:replace:goto-prev);  7-Oct-2007
  (global-set-key [blue      ?\A-w]  'lse-set-buffer-nowrite)
  (global-set-key [blue gold ?\A-w]  'lse-set-buffer-write)
  (global-set-key [blue      backspace] 'lse-tpu:delete-prev-char-append)
  (global-set-key [blue      ?\C-:]     'lse-tpu:replace-all); 30-Aug-2002
  (global-set-key [blue      ?\C-,]     'lse-tpu:exchange-point-and-mark); 12-Nov-2002
; lse-define-tpu-blue-keys
)

;;; 25-Aug-2002
(defun lse-define-cursor-movements ()
  "Define cursor key combinations for char, word, and line movements"
  (global-set-smk [left]            'lse-tpu:backward-char)
  (global-set-smk [M-left]          'lse-tpu:next-beginning-of-line)
  (global-set-smk [C-M-left]        'lse-tpu:previous-end-of-line)
  (global-set-smk [C-left]          'lse-tpu:goto-prev-bs-word-head)
  (global-set-smk [s-left]          'lse-tpu:goto-prev-word-head)
  (global-set-smk [C-s-left]        'lse-tpu:goto-prev-bs-word-tail)

  (global-set-smk [right]           'lse-tpu:forward-char)
  (global-set-smk [M-right]         'lse-tpu:forward-line)
  (global-set-smk [C-M-right]       'lse-tpu:next-end-of-line)
  (global-set-smk [C-right]         'lse-tpu:goto-next-word-head)
  (global-set-smk [s-right]         'lse-tpu:goto-next-bs-word-tail)
  (global-set-smk [C-s-right]       'lse-tpu:goto-next-word-tail)

  (global-set-smk [C-up]            'lse-tpu:previous-paragraph); 18-May-2003
  (global-set-smk [M-up]            'lse-tpu:previous-paragraph); 18-May-2003
  (global-set-smk [C-down]          'lse-tpu:next-paragraph);     18-May-2003
  (global-set-smk [M-down]          'lse-tpu:next-paragraph);     18-May-2003

  (global-set-key [gold M-left]     'lse-select-current-line)
  (global-set-key [gold C-left]     'lse-select-current-bs-word)
  (global-set-key [gold s-left]     'lse-select-current-word)

  (global-set-key [gold M-right]    'lse-select-current-line)
  (global-set-key [gold C-right]    'lse-select-current-word)
  (global-set-key [gold s-right]    'lse-select-current-bs-word)

  (global-set-smk [A-end]           'lse-goto-last-fill-in);     11-Oct-2007
  (global-set-smk [C-end]           'lse-tpu:move-to-end);       31-Aug-2002
  (global-set-smk [A-home]          'lse-goto-first-fill-in);    11-Oct-2007
  (global-set-smk [C-home]          'lse-tpu:move-to-beginning); 31-Aug-2002
  (global-set-smk [C-next]          'lse-tpu:page-forward);      31-Aug-2002
  (global-set-smk [C-prior]         'lse-tpu:page-backward);      8-Sep-2002
; lse-define-cursor-movements
)

;;; 25-Aug-2002
(defun lse-define-deletion-keys ()
  "Define delete/backspace key combinations for char, word, and line deletions"
  (global@set-smk [backspace]         'lse-tpu:delete-prev-char)
  (global@set-smk [M-backspace]       'lse-tpu:delete-head-of-line)
  (global@set-smk [?\e M-backspace]   'lse-tpu:delete-to-prev-tail-of-line)
  (global@set-smk [C-M-backspace]     'lse-tpu:delete-to-prev-tail-of-line)
  (global@set-smk [C-backspace]       'lse-tpu:delete-prev-bs-word)
  (global@set-smk [s-backspace]       'lse-tpu:delete-prev-word)
  (global@set-smk [C-s-backspace]     'lse-tpu:delete-prev-bs-word-tail)

  (global@set-smk [delete]            'lse-tpu:delete-next-char)
  (global@set-smk [M-delete]          'lse-tpu:delete-next-line)
  (global@set-smk [?\e M-delete]      'lse-tpu:delete-tail-of-line)
  (global@set-smk [C-M-delete]        'lse-tpu:delete-tail-of-line)
  (global@set-smk [C-delete]          'lse-tpu:delete-next-word)
  (global@set-smk [s-delete]          'lse-tpu:delete-next-bs-word-tail)
  (global@set-smk [C-s-delete]        'lse-tpu:delete-next-word-tail)

  (global@set-smk [f34]               'lse-tpu:delete-prev-char-append)
  (global@set-smk [M-f34]             'lse-tpu:delete-head-of-line-append)
;  (global@set-smk [?\e M-f34]         'lse-tpu:delete-to-prev-tail-of-line-append)
;  (global@set-smk [C-M-f34]           'lse-tpu:delete-to-prev-tail-of-line-append)
;  (global@set-smk [C-f34]             'lse-tpu:delete-prev-bs-word-append)
  (global@set-smk [s-f34]             'lse-tpu:delete-prev-word-append)
;  (global@set-smk [C-s-f34]           'lse-tpu:delete-prev-bs-word-tail-append)

  (global@set-smk [f35]               'lse-tpu:delete-next-char-append)
  (global@set-smk [M-f35]             'lse-tpu:delete-next-line-append)
  (global@set-smk [?\e M-f35]         'lse-tpu:delete-tail-of-line-append)
  (global@set-smk [C-M-f35]           'lse-tpu:delete-tail-of-line-append)
  (global@set-smk [C-f35]             'lse-tpu:delete-next-word-append)
;  (global@set-smk [s-f35]             'lse-tpu:delete-next-bs-word-tail-append)
;  (global@set-smk [C-s-f35]           'lse-tpu:delete-next-word-tail-append)

  (global-set-key [gold delete]       'lse-tpu:undelete-char)
  (global-set-key [gold M-delete]     'lse-tpu:undelete-line)
  (global-set-key [gold C-M-delete]   'lse-tpu:undelete-line)
  (global-set-key [gold C-delete]     'lse-tpu:undelete-word)
  (global-set-key [gold s-delete]     'lse-tpu:undelete-word)
  (global-set-key [gold C-s-delete]   'lse-tpu:undelete-word)

  (global-set-key [A-delete]          'lse-tpu:cut-region)
  (global-set-key [A-backspace]       'lse-tpu:cut-region)
  (global-set-key [blue A-delete]     'lse-tpu:copy-region)
  (global-set-key [blue A-backspace]  'lse-tpu:copy-region)
  (global-set-key [A-f34]             'lse-tpu:cut-append-region)
  (global-set-key [A-f35]             'lse-tpu:cut-append-region)
  (global-set-key [blue A-f34]        'lse-tpu:copy-append-region)
  (global-set-key [blue A-f35]        'lse-tpu:copy-append-region)
; lse-define-deletion-keys
)

;;; 30-Aug-2002
(defun lse-define-insertion-keys ()
  "Define insert key combinations for char, word, and line insertions"
  (global-set-key [M-insert]         'lse-tpu:duplicate-previous-line)
  (global-set-key [C-insert]         'lse-tpu:duplicate-previous-word)
  (global-set-key [s-insert]         'lse-tpu:duplicate-previous-bs-word)
  (global-set-key [C-M-insert]       'lse-tpu:duplicate-word-in-previous-line)

  (global-set-key [insert]           'lse-tpu:paste-region); 2-Oct-2007
  (global-set-key [gold insert]      'lse-tpu:paste-region)
  (global-set-key [red  insert]      'lse-tpu:paste-region);  3-Apr-2008
; lse-define-insertion-keys
)
