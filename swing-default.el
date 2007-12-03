;-*- unibyte: t; coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work

;;;;unix_ms_filename_correspondency swing-default.el swi_dflt.el
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
;;;;    swing-default
;;;;
;;;; Purpose
;;;;    Default emacs initialization file for site swing
;;;;
;;;; Revision Dates
;;;;    29-May-1994 (CT) Creation (of comment)
;;;;    29-May-1994 (CT) require-list moved from swing-keys to this file
;;;;     6-Jan-1995 (CT) Mail alias commands
;;;;    13-Mar-1995 (CT) Defined delete-old-versions (for Emacs 19.27)
;;;;    23-Jan-1996 (CT) Set some variables for the version-control package vc
;;;;    30-May-1996 (CT) Set `message-log-max' (for Emacs 19.29)
;;;;    30-May-1996 (CT) Took some tricks from S.u.S.E. default.el
;;;;    31-May-1996 (CT) Took some more tricks from S.u.S.E. default.el and
;;;;                     term/function-keys.el
;;;;     2-Oct-1996 (CT) Use paren.el instead of stig-paren.el
;;;;     8-Oct-1996 (CT) Set vc-follow-symlinks to nil
;;;;     8-Oct-1996 (CT) Set `interpreter-mode-alist' to avoid sh-mode
;;;;    27-Oct-1996 (CT) Set `adaptive-fill-mode' to nil
;;;;    25-May-1997 (CT) Redefined `vc-lock-from-permissions'
;;;;    13-Dec-1997 (CT) font-lock-mode enabled
;;;;     2-Jan-1998 (CT) Some ediff options set
;;;;     2-Jan-1998 (CT) `lse-keys:override-emacs-control-keys' defined
;;;;    10-Jan-1998 (CT) Define `lse-keys:override-emacs-control-keys' only
;;;;                     if not running under `window-system'
;;;;     5-Feb-1998 (CT) mail- parameters added
;;;;    17-Mar-1998 (CT) (require 'msb) added
;;;;    19-Mar-1998 (CT) msb-max-menu-items set to 50
;;;;    21-Mar-1998 (CT) Removed mail aliases (better defined in /etc/aliases)
;;;;    29-Apr-1998 (CT) Customization of emacs print commands
;;;;    19-Dec-1999 (CT) `lazy-lock-minimum-size' set to 1024
;;;;    19-Dec-1999 (CT) `enable-multibyte-characters' set to nil
;;;;     1-Jan-2000 (CT) `find-file-visit-truename' set to t
;;;;     3-Jan-2000 (CT) Execute `msb-mode' if necessary
;;;;     3-Jan-2000 (CT) `inhibit-eol-conversion' set to t
;;;;     3-Jan-2000 (CT) `server-window' set to extra frame
;;;;    11-Nov-2001 (CT) `tool-bar-mode` disabled
;;;;    11-Nov-2001 (CT) `delete-trailing-whitespace` added to
;;;;                     `write-file-hooks`
;;;;    13-Sep-2002 (MG) `boundp` guards added for `x-pointer-left-ptr` and
;;;;                     `x-display-color-p`
;;;;    13-Nov-2002 (CT) Don't make server window under windows-nt
;;;;     1-Oct-2007 (CT) `lazy-lock-mode` guarded against Emacs 22 (where it
;;;;                     doesn't exist anymore)
;;;;     1-Oct-2007 (CT) Call to `set-language-environment` added
;;;;     1-Oct-2007 (CT) Various modifications for Emacs 22
;;;;     3-Oct-2007 (CT) `lse-keys:override-emacs-control-keys` removed
;;;;     3-Oct-2007 (CT) `lse-keys:function-key-map-bindings` added
;;;;     4-Oct-2007 (CT) `lse-file:update-copyright` added to `before-save-hook`
;;;;     5-Oct-2007 (CT) `Py-Version-Update` added and bound to `[red gold ?V]`
;;;;     9-Oct-2007 (CT) `(lse-tpu:toggle-regexp)` removed
;;;;    13-Oct-2007 (CT) `lse-byte-compile:files` updated
;;;;    18-Oct-2007 (CT) `lse-config.el` factored out
;;;;     3-Dec-2007 (CT) Remove `vc-find-file-hook` from `find-file-hook`
;;;;    ««revision-date»»···
;;;;--

(load "lse-config")

(if lse-emacs19-p
    (progn
      (setq                 directory-abbrev-alist
       '(
          ("^/Node/[a-zA-Z0-9_]+/.swing"         . "/swing")
          ("^/swing/.*/generic_folien"           . "/swing/folien")
          ("^/swing/.*org/aufwandserfassung"     . "/swing/aufwand")
          ("^/swing/.*org/buchhaltung"           . "/swing/buchhaltung")
          ("^/swing/.*org/finanzplanung"         . "/swing/finanzplanung")
          ("^/swing/.*org/kassabuch"             . "/swing/kassabuch")
          ("^/swing/.*project/ek"                . "/swing/ek")
          ("^/swing/.*/seminare/sb"              . "/swing/seminar_def")
          ("^/swing/.*system/awk"                . "/swing/awk")
          ("^/swing/.*system/com"                . "/swing/com")
          ("^/swing/.*system/emacs"              . "/swing/emacs")
          ("^/swing/.*system/kartei"             . "/swing/kartei")
          ("^/swing/.*system/login"              . "/swing/login")
          ("^/swing/.*system/perl"               . "/swing/perl")
          ("^/swing/.*system/stydoc"             . "/swing/stydoc")
          ("^/swing/work/marketing"              . "/swing/marketing")
          ("^/swing/work/org"                    . "/swing/org")
          ("^/swing/work/private"                . "/swing/private")
          ("^/swing/work/project"                . "/swing/project")
          ("^/swing/work/system"                 . "/swing/system")
        )
      )
    )
)

(require 'swing-command)
(load "swing-keys")
(load "swing-kartei")

(if lse-emacsX-p; 31-May-1996
    (progn
      (if (eq system-type 'windows-nt); 13-Nov-2002
          t
        (setq server-window ;  3-Jan-2000
          (lse-frame:make "LSE-Server"
            nil nil '((visibility . icon) (width . 80) (height . 30))
          )
        )
      )
    )
)

; (byte-recompile-directory "/swing/system/emacs" t)

;;; 13-Oct-2007
(lse-add-to-list lse-byte-compile:files "swing-default")
(lse-add-to-list lse-byte-compile:files "swing-keys-v19")
(lse-add-to-list lse-byte-compile:files "swing-kartei-bib")

;;;  3-Dec-2007
(remove-hook 'find-file-hook 'vc-find-file-hook)

;;; __END__ swing-default.el
