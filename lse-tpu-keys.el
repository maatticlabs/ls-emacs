;-*- coding: utf-8 -*-

;;;; Copyright (C) 1994-2017 Mag. Christian Tanzer. All rights reserved.
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
;;;;    lse-tpu-keys
;;;;
;;;; Purpose
;;;;    Provide standard key definitions for LS-Emacs
;;;;
;;;; Revision Dates
;;;;    18-Jun-1994 (CT) Creation
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
;;;;    18-Dec-1997 (CT) `lse-tpu-keys:where-is*' and
;;;;                     `lse-tpu-keys:show-*-keys' added
;;;;    28-Dec-1997 (CT) `lse-tpu-keys:matching-commands' and
;;;;                     `lse-tpu-keys:show-keys-matching' added
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
;;;;     1-Sep-2002 (CT) `lse-tpu:app-keypad-p`,
;;;;                     `lse-tpu:electric-inserts-p`, and
;;;;                     `lse-tpu:use-control-keys-p` added
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
;;;;    23-Jul-2010 (CT) Binding for `[gold ?#]` to
;;;;                     `lse-insert-date-time-comment` added
;;;;    10-Nov-2010 (CT) Use `mapc` instead of `mapcar` where appropriate
;;;;    17-Feb-2012 (CT) Bind `[mouse-3]` to `imenu`
;;;;    19-Feb-2012 (CT) Add `global-set-asp` and `local-set-asp`
;;;;    19-Feb-2012 (CT) Add bindings for `lse-select-next-bracketed-range`
;;;;    19-Feb-2012 (CT) Use `global-set-asp` where appropriate
;;;;    12-Mar-2012 (CT) Bind `forward-list`, `backward-list`,
;;;;                     `lse-tpu:goto-opening-char`,
;;;;                     `lse-tpu:goto-closing-char`, and
;;;;                     `lse-tpu:goto-next-occurrence-current-char`
;;;;     2-Jul-2012 (CT) Bind `lse-copy-current-word`,
;;;;                     `lse-copy-current-bs-word`
;;;;    21-Nov-2013 (CT) Add bindings for typographic quotes `“`, `‘`, `‚`, `„`
;;;;    30-Dec-2013 (CT) Replace bindings for `split-window-below`
;;;;    30-Jan-2014 (CT) Add binding for `recenter` (to `super-r`)
;;;;    20-Oct-2014 (CT) Replace bindings for `mouse-yank-primary`
;;;;    21-Oct-2014 (CT) Add lse-frame bindings ([?\C-x?5?1], ...)
;;;;    22-Oct-2014 (CT) Replace bindings for
;;;;                     `electric-newline-and-maybe-indent`
;;;;     7-Nov-2014 (CT) Factor `lse-tpu:define-mouse-keys`,
;;;;                     unset destructive `mode-line` mouse bindings
;;;;     7-Nov-2014 (CT) Add binding for `lse-menu:set-font` [S-down-mouse-2]
;;;;    12-Nov-2014 (CT) Remove support for ancient Emacs versions
;;;;    12-Nov-2014 (CT) Fold lse-tpu-keys-v19.el in here
;;;;    12-Nov-2014 (CT) Move some functions in here from lse-keys.el
;;;;    12-Nov-2014 (CT) Add and use `lse-keys/define`, `lse-keys/define-in-map`
;;;;    14-Nov-2014 (CT) Remove `lse-tpu:change-search-mode`
;;;;    15-Nov-2014 (CT) Bind (control F) to 'lse-tpu:search-reverse
;;;;    15-Nov-2014 (CT) Bind (control t) to 'lse-tpu:goto-next-occurrence-char,
;;;;                          (control T) to 'lse-tpu:goto-prev-occurrence-char
;;;;    17-Nov-2014 (CT) Bind (control *) to `lse-tpu:repeat-factor:set`
;;;;                     ditto for (control +)
;;;;    17-Nov-2014 (CT) Bind (control =) to 'lse-tpu:ccp-buffer-index:set
;;;;    18-Nov-2014 (CT) Bind (meta -) to `lse-number-at-point:decrement`,
;;;;                     (meta +), to `lse-number-at-point:increment`,
;;;;                     remove bindings for `lse-*-register` functions
;;;;    19-Nov-2014 (CT) Bind `lse-frame` functions for `:set-height` and
;;;;                     `:set-width` to `[C-x 5]`, remove old bindings for them
;;;;    19-Nov-2014 (CT) Add bindings for `iconify-frame`, `lower-frame`
;;;;    20-Nov-2014 (CT) Remove functions for shifted keys activating mark
;;;;                     (use standard Emacs' `shift-select-mode` instead)
;;;;    12-Dec-2014 (CT) Bind (control t) to 'transpose-chars, again
;;;;                     bind `C-,` to 'lse-tpu:goto-next-occurrence-char,
;;;;                     bind `C-;` to 'lse-tpu:goto-prev-occurrence-char
;;;;    12-Dec-2014 (CT) Add `lse-key/define-in-function-key-map`
;;;;    15-Jul-2015 (CT) Remove binding of `[S-delete]`
;;;;     7-Nov-2015 (CT) Bind `lse-tpu:join-line-head`, `lse-tpu:join-line-tail`
;;;;     6-Dec-2015 (CT) Bind `browse-url-at-point` to [A-/]
;;;;     7-Dec-2015 (CT) Bind `lse-insert-yyyy-mm-dd-time+blank`to [C-#],
;;;;                     bind `lse-insert-yyyy-mm-dd-time-comment` to
;;;;                     [blue gold ?#]
;;;;    21-Feb-2016 (CT) Bind `lse-tpu:highlight-search` and friends to [s-h],
;;;;                     [s-u], [gold s-h], [gold s-u]
;;;;    24-Nov-2016 (CT) Don't replace 'mouse-yank-primary binding for Emacs 25
;;;;    18-Aug-2017 (CT) Bind `lse-tabulator` to [C-\ ]
;;;;    18-Aug-2017 (CT) Bind `lse-tpu:select` to [?\A-\s-,]
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-tpu-keys)

(defvar lse-tpu:electric-inserts-p nil
  "Define some printable keys electrically, e.g., electric `,` inserts `, `;
electric `(` inserts `()` and positions point between the parentheses..."
)

;;;  1-Sep-2002
(defvar lse-tpu:use-control-keys-p nil
  "Bind some control keys to lse-tpu specific functions."
)

(defun lse-key-name (key-seq)
  (interactive "kPress key ")
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

(defmacro lse-key-cmd (&rest args)
  `(lambda () (interactive) ,@args)
)

;;; 12-Dec-2014
(defun lse-key/define-in-function-key-map (key binding)
  (global-unset-key key)
  (define-key function-key-map key binding)
; lse-key/define-in-function-key-map
)

;;; 12-Nov-2014
(defun lse-keys/define (cmd bindings)
  (mapc (function (lambda (x) (apply cmd x))) bindings)
; lse-keys/define
)

;;; 12-Nov-2014
(defun lse-keys/define-in-map (cmd map bindings)
  (mapc (function (lambda (x) (apply cmd map x))) bindings)
; lse-keys/define-in-map
)

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
    (when (integerp k)
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

;;; 19-Feb-2012
(defun global-set-asp (key command)
  "Define a global key marked for 'auto-save-position"
  (global-set-key key command)
  (lse-tpu:put-prop:auto-save-position command)
; global-set-asp
)

;;; 19-Feb-2012
(defun local-set-asp (key command)
  "Define a local key marked for 'auto-save-position"
  (local-set-key key command)
  (lse-tpu:put-prop:auto-save-position command)
; local-set-asp
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
  (mapc
    (function (lambda (k) (lse-move-key-to-prefix keymap prefix k)))
    keys
  )
; lse-move-keys-to-prefix
)
(defun lse-iterate-keys-bound-to-function (binding do)
  "Calls `do' for every key of current keymap bound to `binding'."
  (mapc do (where-is-internal binding))
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
  (lse-keys/define #'global-set-key
    '(
      ([kp-0]            lse-insert-num-0)
      ([kp-1]            lse-insert-num-1)
      ([kp-2]            lse-insert-num-2)
      ([kp-3]            lse-insert-num-3)
      ([kp-4]            lse-insert-num-4)
      ([kp-5]            lse-insert-num-5)
      ([kp-6]            lse-insert-num-6)
      ([kp-7]            lse-insert-num-7)
      ([kp-8]            lse-insert-num-8)
      ([kp-9]            lse-insert-num-9)
      ([kp-subtract]     lse-insert-num-minus)
      ([kp-separator]    lse-insert-num-comma)
      ([kp-decimal]      lse-insert-num-point)
    )
  )
; lse-tpu:define-keypad-num
)

;;;  1-Sep-2002
(defun lse-tpu:define-electric-inserts ()
  (lse-keys/define #'global-set-key
    '(
      ("("               lse-insert-parentheses)
      ("["               lse-insert-brackets)
      ("{"               lse-insert-braces)
      (","               lse-insert-comma)
      (";"               lse-insert-semicolon)
      ("\""              lse-insert-dquotes)
      ("“"               lse-insert-tdquotes)
      ("‘"               lse-insert-tsquotes)
      ("”"               lse-insert-tgdquotes)
      ("’"               lse-insert-tgsquotes)
    )
  )
; lse-tpu:define-electric-inserts
)

;;;  7-Nov-2014
(defun lse-tpu:define-mouse-keys ()
  (lse-keys/define #'global-set-key
    '(
      ([S-down-mouse-2]  lse-menu:set-font)
      ([S-down-mouse-3]  imenu)
    )
  )
; lse-tpu:define-mouse-keys
)

;;;  1-Sep-2002
(defun lse-tpu:redefine-some-control-keys ()
  (lse-define-key-in-all-maps [?\C-w] 'lse-tpu:delete-prev-bs-word);  6-Jan-2002
  (lse-keys/define #'global-set-asp
    '(
      ([(control f)]        lse-tpu:search-forward);             5-Oct-2007
      ([(control F)]        lse-tpu:search-reverse);            15-Nov-2014
      ([(control n)]        lse-tpu:search-again-forward);      31-Aug-2002
      ([(control p)]        lse-tpu:search-again-reverse);      31-Aug-2002
      ([(control \,)]       lse-tpu:goto-next-occurrence-char); 12-Dec-2014
      ([(control \;)]       lse-tpu:goto-prev-occurrence-char); 12-Dec-2014
      ([(super f)]          lse-tpu:search-reverse);             5-Oct-2007
    )
  )
  (lse-keys/define #'global-set-key
    '(
      ([(control o)]        lse-open-line);                     31-Aug-2002
      ([(control t)]        transpose-chars);                   12-Dec-2014
      ([(control \')]       lse-insert-backquote-quote);        10-Jan-1998
      ([(control \|)]       lse-fill-range);                    10-Jan-1998
      ([(control \>)]       lse-unset-selective-display);        8-Sep-2002
      ([(control \*)]       lse-tpu:repeat-factor:set);         17-Nov-2014
      ([(control \+)]       lse-tpu:repeat-factor:set);         17-Nov-2014
      ([(control \=)]       lse-tpu:ccp-buffer-index:set);      17-Nov-2014
      ([(control \/)]       lse-tpu:search-history-index:set);  21-Feb-2016
      ([(control super \.)] lse-tpu:unselect);                  17-Jun-2001
      ([(control \:)]       lse-tpu:replace);                   30-Aug-2002
      ([(super r)]          recenter);                          30-Jan-2014
    )
  )
  (if (fboundp 'repeat)
      (global-set-key  [(control z)] 'repeat);                   1-Jan-2000
  )
; lse-tpu:redefine-some-control-keys
)

(defun lse-define-tpu-keys ()
  "Redefine keys defined by standard EMACS modes"
  (lse-tpu:define-keypad-num)                                     ;  3-Oct-2007
  (lse-tpu:define-mouse-keys)
  (if lse-tpu:electric-inserts-p
      (lse-tpu:define-electric-inserts)
  )
  (if lse-tpu:use-control-keys-p
      (lse-tpu:redefine-some-control-keys)
  )
  (lse-keys/define #'lse-define-key-in-all-maps
    '(
      ([?\A-_]     undo)                                          ; 10-Jan-1998
      ([?\C-d]     lse-tpu:delete-next-char)                      ;  6-Jan-2002
      ([?\A-g]     abort-recursive-edit)                          ; 10-Jan-1998
      ([?\A-j]     lse-tpu:delete-prev-word)                      ; 10-Jan-1998
      ([?\C-k]     lse-tpu:delete-tail-of-line)                   ;  6-Jan-2002
    )
  )

  (lse-keys/define #'lse-define-key-in-minibuffer-maps
    '(
      ([up]        lse-tpu:previous-history-element)
      ([down]      lse-tpu:next-history-element)
      ([tab]       minibuffer-complete)
      ([do]        lse-tpu:previous-history-element)
      ([find]      previous-matching-history-element)
      ([gold find] next-matching-history-element)
      ([home]      beginning-of-line)
    )
  )

  (lse-copy-key-in-minibuffer-maps   [?\C-m]     [return])        ;  4-Jan-1998
  (lse-copy-key-in-minibuffer-maps   [?\C-m]     [?\A-m])         ; 10-Jan-1998

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

  (lse-keys/define #'global-set-asp
    '(
      ([?\A-<]           lse-tpu:goto-opening-char)
      ([?\A->]           lse-tpu:goto-closing-char)
      ([?\A-\(]          lse-tpu:goto-opening-char)
      ([?\A-\)]          lse-tpu:goto-closing-char)
      ([?\A-\[]          lse-tpu:goto-opening-char)
      ([?\A-\]]          lse-tpu:goto-closing-char)
      ([?\A-\{]          lse-tpu:goto-opening-char)
      ([?\A-\}]          lse-tpu:goto-closing-char)
      ([?\A-f]           lse-compilation:next-error)              ; 20-Feb-1995
      ([?\C-<]           lse-tpu:goto-next-occurrence-current-char)
      ([?\C->]           lse-tpu:goto-prev-occurrence-current-char)
      ([?\C-\(]          forward-list)
      ([?\C-\)]          backward-list)
      ([?\M-l]           goto-line)                               ; 31-Aug-2002
    )
  )
  (lse-keys/define #'global-set-key
    '(
      ([?\A-']           lse-insert-bquotes)                      ; 10-Jun-1998
      ([?\A-,]           lse-tpu:select)                          ; 12-Nov-2002
      ([?\A-\s-,]        lse-tpu:select)                          ; 18-Aug-2017
      ([?\A-:]           lse-tpu:replace-all)                     ; 30-Aug-2002
      ([?\A-\ ]          lse-tabulator)                           ; 13-Sep-2002
      ([?\A-\-]          negative-argument)                       ; 30-Dec-1997
      ([?\A-\.]          universal-argument)                      ; 30-Dec-1997
      ([?\A-\\]          quoted-insert)
      ([?\A-/]           browse-url-at-point)                     ;  6-Dec-2015
      ([?\A-\s-.]        lse-tpu:unselect)                        ; 17-Jun-2001
      ([?\A-a]           lse-tpu:toggle-overwrite-mode)
      ([?\A-d]           dabbrev-expand)
      ([?\A-g]           keyboard-quit)                           ; 29-Dec-1997
      ([?\A-h]           lse-tpu:next-beginning-of-line)
      ([?\A-i]           lse-tabulator)                           ; 19-Mar-1995
      ([?\A-l]           lse-tpu:insert-formfeed)
      ([?\A-q]           lse-insert-buffer-name)                  ; 28-Apr-1996
      ([?\A-t]           transpose-chars)                         ; 10-Jan-1998
      ([?\A-u]           lse-tpu:delete-head-of-line)
      ([?\A-v]           lse-align-and-down)                      ; 15-Sep-1995
      ([?\A-w]           redraw-display)
      ([?\A-z]           lse-tpu:goto-last-position)
      ([?\A-|]           lse-fill-range)                          ; 28-Apr-1996
      ([?\C-\.]          universal-argument)                      ; 28-Jun-1995
      ([?\C-^]           lse-tpu:join-line-head)                  ;  7-Nov-2015
      ([?\C-$]           lse-tpu:join-line-tail)                  ;  7-Nov-2015
      ([?\C-#]           lse-insert-yyyy-mm-dd-time+blank)        ;  7-Dec-2015
      ([?\C-\A-d]        dabbrev-completion)                      ;  3-Jan-2000
      ([?\C-i]           lse-tabulator)                           ; 13-Sep-2002
      ([?\C-\ ]          lse-tabulator)                           ; 18-Aug-2017
      ([?\C-x?5?1]       lse-frame:make-full-height)              ; 21-Oct-2014
      ([?\C-x?5?2]       lse-frame:make-std)                      ; 21-Oct-2014
      ([?\C-x?5?3]       lse-frame:make-small)                    ;  9-Apr-1998
      ([?\C-x?5?\-]      lse-frame:set-width:std)                 ; 19-Nov-2014
      ([?\C-x?5?\–]      lse-frame:set-width:wide)                ; 19-Nov-2014
      ([?\C-x?5?\—]      lse-frame:set-width:double)              ; 19-Nov-2014
      ([?\C-x?5?\:]      lse-frame:set-height:full)               ; 19-Nov-2014
      ([?\C-x?5?\,]      lse-frame:set-height:large)              ; 19-Nov-2014
      ([?\C-x?5?\.]      lse-frame:set-height:small)              ; 19-Nov-2014
      ([?\C-x?5?^]       lse-frame:fix-position)                  ; 21-Oct-2014
      ([?\C-x?5?h?f]     lse-frame:set-height:full)               ; 19-Nov-2014
      ([?\C-x?5?h?l]     lse-frame:set-height:large)              ; 19-Nov-2014
      ([?\C-x?5?h?n]     lse-frame:set-height:std)                ; 19-Nov-2014
      ([?\C-x?5?h?s]     lse-frame:set-height:small)              ; 19-Nov-2014
      ([?\C-x?5?i]       iconify-frame)                           ; 19-Nov-2014
      ([?\C-x?5?l]       lower-frame)                             ; 19-Nov-2014
      ([?\C-x?5?w?d]     lse-frame:set-width:double)              ; 19-Nov-2014
      ([?\C-x?5?w?s]     lse-frame:set-width:std)                 ; 19-Nov-2014
      ([?\C-x?5?w?w]     lse-frame:set-width:wide)                ; 19-Nov-2014
      ([?\C-x?5?\C-w]    delete-frame)                            ; 20-Nov-2014
      ([?\H-V]           lse-align-to-previous-word-and-down)     ; 27-Jul-1999
      ([?\H-v]           lse-align-to-next-word-and-up)           ; 27-Jul-1999
      ([?\M-\-]          lse-number-at-point:decrement)           ; 18-Nov-2014
      ([?\M-\+]          lse-number-at-point:increment)           ; 18-Nov-2014
      ([?\M-R]           lse-scroll-to-bottom)                    ;  3-Apr-2008
      ([?\M-r]           lse-scroll-to-top)                       ;  1-Sep-2002
      ([?\M-v]           lse-align-and-up)                        ; 15-Sep-1995
      ([?\s-\-]          lse-number-at-point:decrement)           ; 18-Nov-2014
      ([?\s-\+]          lse-number-at-point:increment)           ; 18-Nov-2014
      ([?\s-<]           lse-insert-angles)                       ; 20-Jan-2000
      ([?\s-\A-d]        ispell-complete-word)                    ;  6-Jan-2002
      ([?\s-a]           delete-selection-mode)                   ; 28-Dec-1997
      ([?\s-c]           lse-tpu:copy-region)                     ; 12-Feb-1998
      ([?\s-d]           hippie-expand)                           ; 29-Dec-1997
      ([?\s-h]           lse-tpu:highlight-search)               ; 21-Feb-2016
      ([?\s-q]           lse-insert-buffer-name-plus-extension)   ;  8-Dec-2007
      ([?\s-u]           lse-tpu:unhighlight-search)             ; 21-Feb-2016
      ([?\s-v]           lse-tpu:paste-region)                    ; 12-Feb-1998
      ([?\s-x]           lse-tpu:cut-region)                      ; 12-Feb-1998
      ([?\s-|]           lse-insert-bars)                         ; 20-Jan-2000
    )
  )

  (local-unset-key [?\C-i])
  (local-unset-key "\177")
  (local-unset-key [backspace])

  ;;; Remove binding for `[S-delete]` (bound by Emacs to `kill-region`)
  ;;; because it interferes with shift-translation of `[delete]` (which allows
  ;;; for keeping the region active while deleting the next character)
  (global-unset-key [S-delete])

  (lse-keys/define-in-map 'define-key help-map
    '(
      ([red]             lse-tpu-keys:show-list-sexp-keys)
      ([?W]              lse-tpu-keys:show-keys-matching)         ; 28-Dec-1997
      ([(\')]            lse-key-name)
    )
  )

  (lse-keys/define #'global-set-asp
    '(
      ([next]            lse-next-screen-2)
      ([prior]           lse-previous-screen-2)
    )
  )

  (lse-keys/define #'global-set-key
    '(
      ([cancel]          lse-tpu:unselect)
      ([delete]          lse-tpu:delete-next-char)                ; 25-Feb-1998
      ([do]              repeat-complex-command)
      ([select]          lse-tpu:select)
      ([A-S-left]        lse-tpu:pan-left)                        ; 20-Aug-1995
      ([A-S-right]       lse-tpu:pan-right)                       ; 20-Aug-1995
      ([A-down]          shrink-window)                           ; 18-Jul-1995
      ([A-left]          lse-deindent-line-by-word)               ; 18-Jul-1995
      ([A-next]          lse-scroll-other-window-forw)            ; 18-Jul-1995
      ([A-prior]         lse-scroll-other-window-back)            ; 18-Jul-1995
      ([A-return]        lse-split-line-i)                        ; 27-Mar-1997
      ([A-right]         lse-indent-line-by-word)                 ; 18-Jul-1995
      ([A-up]            enlarge-window)                          ; 18-Jul-1995
      ([gold A-return]   lse-toggle-lse-split-line)               ; 27-Mar-1997
      ([red end]         lse-scroll-to-bottom)                    ;  3-Apr-2008
      ([red home]        lse-scroll-to-top)                       ;  3-Apr-2008
    )
  )

; lse-define-tpu-keys
)

(defun lse-replace-std-emacs-bindings ()
  (lse-replace-binding-for-all-keys 'delete-backward-char      'lse-tpu:delete-prev-char)
  (lse-replace-binding-for-all-keys 'backward-kill-word        'lse-tpu:delete-prev-word);  6-Jan-2002
  (lse-replace-binding-for-all-keys 'delete-char               'lse-tpu:delete-next-char)
  (lse-replace-binding-for-all-keys 'delete-other-windows      'lse-delete-other-windows)
  (lse-replace-binding-for-all-keys 'delete-window             'lse-delete-window)
  (lse-replace-binding-for-all-keys 'find-alternate-file       'lse-visit-alternate-file)
  (lse-replace-binding-for-all-keys 'find-file                 'lse-visit-file)
  (lse-replace-binding-for-all-keys 'find-file-other-window    'lse-visit-file-other-window)
  (lse-replace-binding-for-all-keys 'join-line                 'lse-tpu:join-line-head)
  (lse-replace-binding-for-all-keys 'delete-indentation        'lse-tpu:join-line-head)
  (lse-replace-binding-for-all-keys 'kill-buffer               'lse-kill-buffer)
  (lse-replace-binding-for-all-keys 'revert-buffer             'lse-revert-buffer)
  (lse-replace-binding-for-all-keys 'set-visited-file-name     'lse-change-output-file)
  (lse-replace-binding-for-all-keys 'shell-command             'lse-shell-command)
  (lse-replace-binding-for-all-keys 'split-window              'lse-split-window)
  (lse-replace-binding-for-all-keys 'split-window-below        'lse-split-window)
  (lse-replace-binding-for-all-keys 'split-window-horizontally 'lse-split-window-horizontally)
  (lse-replace-binding-for-all-keys 'split-window-vertically   'lse-split-window)
  (lse-replace-binding-for-all-keys 'switch-to-buffer          'lse-goto-buffer)
  (lse-replace-binding-for-all-keys 'switch-to-buffer-other-window
                                                               'lse-goto-buffer-other-window)
  (lse-replace-binding-for-all-keys 'toggle-read-only          'lse-set-buffer-nowrite)

  (lse-replace-binding-for-all-keys 'print-buffer              'lpr-buffer); 29-Apr-1998
  (lse-replace-binding-for-all-keys 'print-region              'lpr-region); 29-Apr-1998

  (when (and lse-emacs24-p (not lse-emacs25-p))
    (lse-replace-binding-for-all-keys 'mouse-yank-primary 'lse-tpu:mouse-paste); 20-Oct-2014
  )
  (when (fboundp 'electric-newline-and-maybe-indent)
    (lse-replace-binding-for-all-keys
      'electric-newline-and-maybe-indent 'newline-and-indent
    )
  )
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

  (lse-keys/define #'global-set-key
    '(
      ([gold ?*]           lse-show-buffers)
      ([gold ?,]           self-insert-command)
      ([gold ?.]           self-insert-command)
      ([gold ?/]           lse-align-and-up)
      ([gold ?<]           lse-tpu:pan-left)
      ([gold ?>]           lse-tpu:pan-right)
      ([gold ?=]           lse-split-window)
      ([gold ??]           lse-insert-key-definition)
      ([gold ?\ ]          lse-align-to-next-word)
      ([gold ?\"]          self-insert-command)
      ([gold ?\']          lse-insert-squotes)
      ([gold ?“]           self-insert-command)
      ([gold ?‘]           self-insert-command)
      ([gold ?”]           self-insert-command)
      ([gold ?’]           self-insert-command)
      ([gold ?\(]          self-insert-command)
      ([gold ?\)]          backward-sexp)
      ([gold ?\;]          self-insert-command)
      ([gold ?\[]          self-insert-command)
      ([gold ?\\]          self-insert-command)
      ([gold ?\]]          backward-sexp)
      ([gold ?{]           self-insert-command)
      ([gold ?|]           lse-fill-range)
      ([gold ?}]           backward-sexp)
      ([gold ?~]           vt-narrow)
      ([gold ?+]           lse-push-window-configuration)
      ([gold ?#]           lse-insert-date-time-comment); 23-Jul-2010
    )
  )

  (lse-keys/define #'gset-alpha-key
    '(
      ([gold] "A"          occur)
      ([gold] "B"          lse-goto-last-mark-global)
      ([gold] "C"          lse-tpu:capitalize-weakly)
      ([gold] "D"          lse-insert-dd-mmm-yyyy+blank)
      ([gold] "E"          undefined)
      ([gold] "F"          lse-goto-buffer)
      ([gold] "G"          lse-goto-mark-and-pop-global)
      ([gold] "H"          lse-goto-home-mark-global)
      ([gold] "I"          lse-visit-file)
      ([gold] "J"          lse-frame:toggle-menu-bar); 28-Mar-2007
      ([gold] "K"          lse-insert-user-full-name); 13-Nov-2014
      ([gold] "L"          lse-tpu:change-case-lower)
      ([gold] "M"          lse-push-mark-global)
      ([gold] "N"          lse-goto-next-buffer)
      ([gold] "O"          lse-change-output-file)
      ([gold] "P"          lse-insert-buffer)
      ([gold] "Q"          lse-insert-buffer-name)
      ([gold] "R"          lse-tpu:toggle-rectangle)
      ([gold] "S"          lse-insert-yyyy-mm-dd+blank); 13-Nov-2014
      ([gold] "T"          lse-toggle-mark-global)
      ([gold] "U"          lse-tpu:change-case-upper)
      ([gold] "V"          lse-align-to-next-word-and-up)
      ([gold] "W"          lse-write-buffer)
      ([gold] "X"          suspend-emacs)
      ([gold] "Y"          eval-last-sexp)
      ([gold] "Z"          lse-insert-user-initials)
    )
  )

  (lse-keys/define #'global-set-key
    '(
      ([gold ?\A-d]    ispell-complete-word);  4-Nov-1996
      ([gold ?\C-d]    lse-tpu:undelete-char);  6-Jan-2002
      ([gold ?\C-f]    lse-tpu:goto-pos-before-search); 31-Aug-2002
      ([gold ?\s-h]    lse-tpu:highlight-last-search); 21-Feb-2016
      ([gold ?\A-i]    lse-indent-line); 19-Mar-1995
      ([gold tab]      lse-indent-line); 19-Mar-1995
      ([gold ?\A-j]    lse-tpu:undelete-word);  6-Jan-2002
      ([gold ?\C-k]    lse-tpu:undelete-line);  6-Jan-2002
      ([gold ?\C-l]    lse-scroll-to-top); 31-Aug-2002
      ([gold ?\M-l]    lse-show-position); 31-Aug-2002
      ([gold ?\A-m]    newline)
      ([gold ?\C-n]    lse-tpu:goto-pos-before-search); 14-Nov-2002
      ([gold A-return] lse-toggle-lse-split-line); 27-Mar-1997
      ([gold ?\C-p]    lse-tpu:goto-pos-before-search); 14-Nov-2002
      ([gold ?\M-r]    move-to-window-line);  1-Sep-2002
      ([gold ?\A-u]    lse-tpu:undelete-line);  8-Sep-2002
      ([gold ?\s-u]    lse-tpu:unhighlight-last-search); 21-Feb-2016
      ([gold ?\C-w]    lse-tpu:undelete-word);  8-Sep-2002
      ([gold ?\C-,]    lse-tpu:exchange-point-and-mark); 12-Nov-2002

      ([gold gold]     universal-argument)
      ([gold blue]     universal-argument)

      ([gold up]       lse-previous-window)
      ([gold down]     lse-next-window)

      ;; 25-Feb-1998 ;; `home' instead of `insert'
      ([gold home]     lse-select-current-line); 7-Apr-1997
      ([gold delete]   lse-tpu:undelete-char)
      ([gold select]   lse-tpu:unselect)
      ([gold cancel]   lse-tpu:exchange-point-and-mark)
      ([gold prior]    lse-previous-screen)
      ([gold next]     lse-next-screen)

      ([gold do]       lse-command:do); 31-Aug-2002
      ([red  do]       undo);           31-Aug-2002
    )
  )
  (global-set-key [gold right] (lse-key-cmd (lse-indent-rigidly (lse-tab-increment))))
  (global-set-key [gold left]  (lse-key-cmd (lse-indent-rigidly (- (lse-tab-increment)))))
; lse-define-tpu-gold-keys
)

(defun lse-define-tpu-blue-keys ()
  "Define BLUE-keys"
  (lse-keys/define #'global-set-key
    '(
      ([blue blue]        lse-ring-bell)

      ([blue find]        find-tag)
      ([blue gold find]   lse-grep); 20-Feb-1995
      ([blue home]        lse-indent:goto-indent-pos); 7-Apr-1997 ;  9-Jun-1995
      ([blue insert]      lse-tpu:duplicate-previous-bs-word)
      ([blue delete]      lse-tpu:duplicate-previous-char); 25-Feb-1998
      ([blue cancel]      lse-tpu:exchange-point-and-mark); 26-Aug-2002
      ([blue gold cancel] lse-blink-select-mark); 26-Aug-2002
      ([blue select]      lse-tpu:exchange-point-and-mark);  8-Sep-2002
      ([blue gold select] lse-select-current-bs-word)

      ([blue prior]       lse-scroll-window-back)
      ([blue next]        lse-scroll-window-forw)
      ([blue gold prior]  lse-scroll-other-window-back)
      ([blue gold next]   lse-scroll-other-window-forw)

      ([blue gold right]  lse-indent-line-by-word)
      ([blue gold left]   lse-deindent-line-by-word)
      ([blue up]          lse-previous-window-all-frames); 28-Dec-1999 lse-previous-window
      ([blue down]        lse-next-window-all-frames); 28-Dec-1999 lse-next-window
      ([blue gold up]     enlarge-window)
      ([blue gold down]   shrink-window)
      ([blue help]        describe-key-briefly)
      ([blue gold help]   help-command)
      ([blue do]          execute-extended-command)
    )
  )
  (global-set-key [blue right]       (lse-key-cmd (lse-indent-rigidly (*  3 (lse-tab-increment)))))
  (global-set-key [blue left]        (lse-key-cmd (lse-indent-rigidly (* -3 (lse-tab-increment)))))

  (lse-keys/define #'gset-alpha-key
    '(
      ([blue]      "A"    occur)
      ([blue]      "B"    lse-goto-last-mark-window)
      ([blue gold] "B"    lse-goto-last-mark-buffer)
      ([blue]      "C"    lse-tpu:capitalize-strongly)
      ([blue gold] "C"    lse-tpu:change-case); 31-Aug-2002
      ([blue]      "D"    lse-insert-dd-mm-yyyy+blank)
      ([blue gold] "D"    lse-insert-yyyy/mm/dd+blank);  2-May-2006
      ([blue gold] "E"    lse-kill-buffer)
      ([blue]      "F"    lse-goto-buffer+maybe-create)
      ([blue gold] "F"    lse-goto-buffer-other-window)
      ([blue]      "G"    lse-goto-mark-and-pop-window)
      ([blue gold] "G"    lse-goto-mark-and-pop-buffer)
      ([blue]      "H"    lse-goto-home-mark-window)
      ([blue gold] "H"    lse-set-home-mark-window)
      ([blue]      "I"    lse-visit-file-other-window)
      ([blue gold] "I"    lse-insert-file)
      ([blue]      "J"    lse-menu:toggle-menu-bar); 28-Mar-2007
      ([blue]      "L"    lse-learn-key)
      ([blue gold] "L"    lse-learn-named-key)
      ([blue]      "M"    lse-push-mark-window)
      ([blue gold] "M"    lse-push-mark-buffer)
      ([blue]      "N"    lse-goto-prev-buffer)
      ([blue]      "O"    lse-change-output-file-current)
      ([blue gold] "O"    lse-make-directory)
      ([blue]      "P"    lse-insert-buffer)
      ([blue]      "Q"    lse-tpu:special-insert); 31-Aug-2002
      ([blue gold] "Q"    lse-insert-buffer-name-plus-extension);  8-Dec-2007
      ([blue gold] "R"    lse-revert-buffer);  8-Sep-2002
      ([blue]      "S"    lse-insert-yyyymmdd+blank); 13-Nov-2014
      ([blue gold] "S"    lse-insert-time+blank);     13-Nov-2014
      ([blue]      "T"    lse-toggle-mark-window)
      ([blue gold] "T"    lse-toggle-mark-buffer)
      ([blue]      "V"    lse-align-to-previous-word-and-down)
      ([blue]      "W"    lse-write-current-buffer)
      ([blue gold] "W"    lse-save-some-buffers); 5-Apr-1998
      ([blue]      "X"    lse-shell-command)
      ([blue]      "Y"    lse-compile); 20-Feb-1995
      ([blue gold] "Y"    lse-set-compile-command);  9-Jun-1995
      ([blue]      "Z"    lse-insert-user-full-name)
      ([blue gold] "Z"    lse-insert-user-name)
    )
  )

  (lse-keys/define #'global-set-asp
    '(
      ([blue      ?\(]    lse-select-bracketed-range)
      ([blue      ?\<]    lse-select-bracketed-range)
      ([blue      ?\C-n]  lse-tpu:replace:goto-next);  7-Oct-2007
      ([blue      ?\C-p]  lse-tpu:replace:goto-prev);  7-Oct-2007
      ([blue      ?\[]    lse-select-bracketed-range)
      ([blue      ?\{]    lse-select-bracketed-range)
      ([blue      ?\«]    lse-select-bracketed-range)
      ([pink      ?\(]    lse-select-next-bracketed-range)
      ([pink      ?\<]    lse-select-next-bracketed-range)
      ([pink      ?\[]    lse-select-next-bracketed-range)
      ([pink      ?\{]    lse-select-next-bracketed-range)
      ([pink      ?\«]    lse-select-next-bracketed-range)
    )
  )
  (lse-keys/define #'global-set-key
    '(
      ([blue      ?#]        lse-count-matches)
      ([blue gold ?#]        lse-insert-yyyy-mm-dd-time-comment)
      ([blue      ?']        lse-insert-backquote-quote); 26-Apr-1996
      ([blue      ?*]        lse-frame:list:show);  8-Dec-2009
      ([blue      ?+]        lse-pop+restore-window-configuration)
      ([blue      ?/]        lse-align-and-down)
      ([blue      ?=]        lse-split-window-horizontally); 22-May-97
      ([blue      ??]        lse-insert-key-name); 12-Jun-1994
      ([blue      ?\ ]       lse-align-to-previous-word)
      ([blue      ?\"]       lse-insert-double-backquote-quote); 26-Apr-1996
      ([blue      ?\A-m]     lse-split-line-i)
      ([blue      ?\A-w]     lse-set-buffer-nowrite)
      ([blue gold ?\C-w]     delete-frame); 13-Nov-2014
      ([blue      ?\C-,]     lse-tpu:exchange-point-and-mark); 12-Nov-2002
      ([blue      ?\C-:]     lse-tpu:replace-all); 30-Aug-2002
      ([blue      ?\C-m]     lse-split-line-i); 20-Jan-1998
      ([blue      ?\M-f]     lse-grep); 31-Aug-2002
      ([blue      ?|]        lse-insert-bars)
      ([blue      ?~]        delete-trailing-whitespace)
      ([blue      ?-]        negative-argument)
      ([blue gold ?']        lse-remove-backquote-quote); 26-Apr-1996
      ([blue gold ?*]        lse-show-all-buffers);  9-Dec-2009
      ([blue gold ?-]        lse-delete-other-windows)
      ([blue gold ?=]        lse-delete-window)
      ([blue gold ?\ ]       set-fill-column)
      ([blue gold ?\"]       lse-remove-double-backquote-quote); 26-Apr-1996
      ([blue gold ?\(]       lse-remove-parentheses)
      ([blue gold ?\A-i]     auto-fill-mode)
      ([blue gold ?\A-m]     lse-tpu:toggle-newline-and-indent)
      ([blue gold ?\A-w]     lse-set-buffer-write)
      ([blue gold ?\C-i]     auto-fill-mode)
      ([blue gold ?\C-m]     lse-tpu:toggle-newline-and-indent)
      ([blue gold ?\M-f]     find-tag); 31-Aug-2002
      ([blue gold ?\[]       lse-remove-brackets)
      ([blue gold ?\{]       lse-remove-braces)
      ([blue gold ?~]        lse-fill-range)
    )
  )
; lse-define-tpu-blue-keys
)

;;; 25-Aug-2002
(defun lse-define-cursor-movements ()
  "Define cursor key combinations for char, word, and line movements"
  (lse-keys/define #'global-set-key
    '(
      ([down]            lse-tpu:next-line)
      ([end]             lse-tpu:next-end-of-line)
      ([home]            lse-tpu:next-beginning-of-line)

      ([left]            lse-tpu:backward-char)
      ([M-left]          lse-tpu:next-beginning-of-line)
      ([C-M-left]        lse-tpu:previous-end-of-line)
      ([C-left]          lse-tpu:goto-prev-bs-word-head)
      ([s-left]          lse-tpu:goto-prev-word-head)
      ([C-s-left]        lse-tpu:goto-prev-bs-word-tail)

      ([right]           lse-tpu:forward-char)
      ([M-right]         lse-tpu:forward-line)
      ([C-M-right]       lse-tpu:next-end-of-line)
      ([C-right]         lse-tpu:goto-next-word-head)
      ([s-right]         lse-tpu:goto-next-bs-word-tail)
      ([C-s-right]       lse-tpu:goto-next-word-tail)

      ([up]              lse-tpu:previous-line)
    )
  )

  (lse-keys/define #'global-set-asp
    '(
      ([C-up]            lse-tpu:previous-paragraph); 18-May-2003
      ([M-up]            lse-tpu:previous-paragraph); 18-May-2003
      ([C-down]          lse-tpu:next-paragraph);     18-May-2003
      ([M-down]          lse-tpu:next-paragraph);     18-May-2003

      ([gold M-left]     lse-select-current-line)
      ([gold C-left]     lse-select-current-bs-word)
      ([gold s-left]     lse-select-current-word)
      ([blue C-left]     lse-copy-current-bs-word)
      ([blue s-left]     lse-copy-current-word)

      ([gold M-right]    lse-select-current-line)
      ([gold C-right]    lse-select-current-word)
      ([gold s-right]    lse-select-current-bs-word)
      ([blue C-right]    lse-copy-current-word)
      ([blue s-right]    lse-copy-current-bs-word)

      ([A-end]           lse-goto-last-fill-in);     11-Oct-2007
      ([C-end]           lse-tpu:move-to-end);       31-Aug-2002
      ([A-home]          lse-goto-first-fill-in);    11-Oct-2007
      ([C-home]          lse-tpu:move-to-beginning); 31-Aug-2002
      ([C-next]          lse-tpu:page-forward);      31-Aug-2002
      ([C-prior]         lse-tpu:page-backward);      8-Sep-2002
    )
  )
; lse-define-cursor-movements
)

;;; 25-Aug-2002
(defun lse-define-deletion-keys ()
  "Define delete/backspace key combinations for char, word, line, and region
deletions."
  (lse-keys/define #'global-set-key
    '(
      ([backspace]         lse-tpu:delete-prev-char)
      ([A-backspace]       lse-tpu:cut-region)
      ([M-backspace]       lse-tpu:delete-head-of-line)
      ([?\e C-backspace]   lse-tpu:delete-to-prev-tail-of-line)
      ([C-M-backspace]     lse-tpu:delete-to-prev-tail-of-line)
      ([C-backspace]       lse-tpu:delete-prev-bs-word)
      ([s-backspace]       lse-tpu:delete-prev-word)
      ([C-s-backspace]     lse-tpu:delete-prev-bs-word-tail)

      ([delete]            lse-tpu:delete-next-char)
      ([A-delete]          lse-tpu:cut-region)
      ([M-delete]          lse-tpu:delete-next-line)
      ([?\e C-delete]      lse-tpu:delete-tail-of-line)
      ([C-M-delete]        lse-tpu:delete-tail-of-line)
      ([C-delete]          lse-tpu:delete-next-word)
      ([s-delete]          lse-tpu:delete-next-bs-word-tail)
      ([C-s-delete]        lse-tpu:delete-next-word-tail)

      ([gold delete]       lse-tpu:undelete-char)
      ([gold A-delete]     lse-tpu:paste-region)
      ([gold M-delete]     lse-tpu:undelete-line)
      ([gold C-M-delete]   lse-tpu:undelete-line)
      ([gold C-delete]     lse-tpu:undelete-word)
      ([gold s-delete]     lse-tpu:undelete-word)
      ([gold C-s-delete]   lse-tpu:undelete-word)

      ([blue A-delete]     lse-tpu:copy-region)
      ([blue A-backspace]  lse-tpu:copy-region)
    )
  )
; lse-define-deletion-keys
)

;;; 30-Aug-2002
(defun lse-define-insertion-keys ()
  "Define insert key combinations for char, word, and line insertions"
  (lse-keys/define #'global-set-key
    '(
      ([insert]           lse-tpu:paste-region); 2-Oct-2007
      ([gold insert]      lse-tpu:paste-region)
      ([red  insert]      lse-tpu:paste-region); 3-Apr-2008
      ([M-insert]         lse-tpu:duplicate-previous-line)
      ([C-insert]         lse-tpu:duplicate-previous-word)
      ([s-insert]         lse-tpu:duplicate-previous-bs-word)
      ([C-M-insert]       lse-tpu:duplicate-word-in-previous-line)
    )
  )
; lse-define-insertion-keys
)

;;; 18-Dec-1997
(defun lse-tpu-keys:where-is-1 (k show-all fmt)
  (if (string-match "-delegate" (symbol-name k))
      nil
    (let* ((keys  (where-is-internal k overriding-local-map nil nil))
           (keys1 (mapconcat 'key-description keys ", "))
          )
      (if (> (length keys1) 0)
          (princ (format (concat (or fmt "%-25s") " %s\n") k keys1))
        (and show-all
             (princ (format (concat (or fmt "%-25s") " M-x %s RET\n") k k))
        )
      )
    )
  )
; lse-tpu-keys:where-is-1
)

;;; 18-Dec-1997
(defun lse-tpu-keys:where-is (show-all fmt &rest arg)
  (let ((bf "*Help*")
        (iterator
          (function (lambda (k) (lse-tpu-keys:where-is-1 k show-all fmt)))
        )
        lse-tpu:quiet-replace
       )
    (with-output-to-temp-buffer bf
      (if (and arg (listp (car arg)))
          (mapc (function (lambda (l) (mapcar iterator l))) arg)
        (mapc iterator arg)
      )
    )
    (save-current-buffer
      (set-buffer (get-buffer-create bf))
      (lse-tpu:replace-all "menu-bar[-a-zA-Z0-9 ]*" "Menu")
      (lse-tpu:replace-all "down-mouse-3[-a-zA-Z0-9 ]*" "Mouse-Menu")
      (sort-lines nil (point-min) (point-max))
    )
  )
; lse-tpu-keys:where-is
)

;;; 18-Dec-1997
(defun lse-tpu-keys:show-sexp-keys (show-all)
  "Display key bindings for all sexp related keys."
  (interactive "P")
  (lse-tpu-keys:where-is show-all "%-30s"
    (append
      (lse-tpu-keys:matching-commands "defun?$")
      (lse-tpu-keys:matching-commands "sexps?$")
    )
  )
; lse-tpu-keys:show-sexp-keys
)

;;; 18-Dec-1997
(defun lse-tpu-keys:show-list-keys (show-all)
  "Display key bindings for all list related keys."
  (interactive "P")
  (lse-tpu-keys:where-is show-all nil
                         (lse-tpu-keys:matching-commands "list$")
  )
; lse-tpu-keys:show-list-keys
)

;;; 18-Dec-1997
(defun lse-tpu-keys:show-list-sexp-keys (show-all)
  "Display key bindings for all list and sexp related keys."
  (interactive "P")
  (lse-tpu-keys:where-is show-all nil
                         (append
                           (lse-tpu-keys:matching-commands "defun?$")
                           (lse-tpu-keys:matching-commands "list$")
                           (lse-tpu-keys:matching-commands "sexps?$")
                         )
  )
; lse-tpu-keys:show-list-sexp-keys
)

;;; 18-Dec-1997
(defun lse-tpu-keys:show-fill-in-keys (show-all)
  "Display key bindings for all fill-in related keys."
  (interactive "P")
  (lse-tpu-keys:where-is show-all "%-40s"
                         (append
                           (lse-tpu-keys:matching-commands "-fill-in")
                           (lse-tpu-keys:matching-commands "token")
                           '(lse-expand
                             lse-flush-replacement
                             lse-goto-next-expansion
                             lse-goto-parent-expansion-head
                             lse-goto-prev-expansion
                             lse-replicate-menu
                            )
                         )
  )
; lse-tpu-keys:show-fill-in-keys
)

;;; 28-Dec-1997
(defun lse-tpu-keys:show-keys-matching (show-all pat)
  "Display key bindings for all functions matching `pat'"
  (interactive "P\nsShow keys matching: ")
  (lse-tpu-keys:where-is show-all "%-40s" (lse-tpu-keys:matching-commands pat))
; lse-tpu-keys:show-keys-matching
)

;;; 28-Dec-1997
(defun lse-tpu-keys:matching-commands (pat)
  (let (result)
    (mapatoms
      (function
        (lambda (s)
          (if (and (commandp s)
                   (string-match pat (symbol-name s))
              )
              (setq result (cons s result))
          )
        )
      )
    )
    result
  )
; lse-tpu-keys:matching-commands
)

;;; __END__ lse-tpu-keys.el
