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
;;;;    ls-emacs
;;;;
;;;; Purpose
;;;;    Language Sensitive Emacs: master file. `Requiring' this file loads
;;;;    everything necessary.
;;;;
;;;; Revision Dates
;;;;    26-May-1994 (CT) Creation (of comment)
;;;;     3-Jun-1994 (CT) Initialization of lse-load-path and friends
;;;;    23-Jul-1994 (CT) EMACSLSESRIPTS added
;;;;    24-Sep-1994 (CT) Preload language cc
;;;;    24-Sep-1994 (CT) Preload language buchhaltung
;;;;    21-Feb-1995 (CT) lse-compilation require'd
;;;;    12-Mar-1995 (CT) transient-mark-mode called (X goodie)
;;;;                     lse-emacsX-p added
;;;;    18-Mar-1995 (CT) lse-emacsX-p defined correctly
;;;;    25-May-1995 (CT) Preload language mail
;;;;     9-Jul-1995 (CT) Don't preload languages in batch (noninteractive) mode
;;;;    30-Mar-1996 (CT) Require 'lse-lingo
;;;;    12-Aug-1996 (CT) Require 'lse-frame
;;;;    29-Sep-1996 (CT) Don't load lse-lingo automatically
;;;;     3-Oct-1996 (CT) lse-emacsX-p defined as window-system instead of
;;;;                     `(boundp 'x-no-window-manager)'
;;;;    11-Oct-1996 (CT) Use `(fboundp 'resize-minibuffer-mode)' instead of
;;;;                     `(>= emacs-minor-version 31)' to decide if
;;;;                     `resize-minibuffer-mode' is available
;;;;    16-Oct-1996 (CT) Require 'lse-menu
;;;;     4-Dec-1997 (CT) `lse-emacs20-p' added
;;;;    13-Dec-1997 (CT) `lse-flat-fill-in:define-flat-keys' called
;;;;    29-Dec-1997 (CT) `lse-version' added
;;;;     7-Jan-1998 (CT) Preload python language
;;;;     8-Feb-1998 (CT) Set `server-temp-file-regexp'
;;;;    24-May-1999 (CT) `lse-emacs20.3-p' added
;;;;     3-Aug-1999 (CT) Preload latex2e language
;;;;    13-Aug-1999 (CT) Preload tpp-issue
;;;;    29-Aug-2000 (CT) Preload gnats-pr
;;;;    14-Nov-2000 (CT) Require 'gnuserv
;;;;    16-Feb-2001 (CT) Require 'gnuserv commented out
;;;;    28-Nov-2001 (CT) `lse-emacs21-p` added
;;;;    23-Jan-2002 (CT) Preload c language
;;;;    26-Aug-2002 (CT) `lse-define-cursor-movements` and
;;;;                     `lse-define-deletion-keys` added
;;;;    30-Aug-2002 (CT) `lse-define-insertion-keys` added
;;;;     1-Sep-2002 (CT) `lse-version` increased
;;;;     6-Sep-2002 (CT) Adapted to `windows-nt`
;;;;    13-Sep-2002 (MG) Set `w32-alt-is-meta` to nil added
;;;;     4-Apr-2003 (CT) `lse-cal` added
;;;;     1-Oct-2007 (CT) `lse-emacs22-p` added
;;;;     3-Oct-2007 (CT) `lse-create-lse-keymaps` removed
;;;;     3-Oct-2007 (CT) Explicit calls to `lse-keys-v19:define-fkp-key` removed
;;;;    13-Oct-2007 (CT) `lse-byte-compile` added
;;;;     4-Feb-2008 (CT) Removed latex2e language
;;;;    13-May-2009 (CT) Use `defvar` instead of `setq` to set
;;;;                     `lse-directory` and friends
;;;;     5-Aug-2009 (CT) `(vt-keypad-on)` removed from `lse-terminal-setup`
;;;;     5-Aug-2009 (CT) `lse-emacs23-p` added
;;;;     5-Aug-2009 (CT) `lse-version` increased
;;;;    17-Nov-2009 (CT) `server-start` guarded against `server-running-p`
;;;;    17-Nov-2009 (CT) Guard for `lse-global-home-mark-initialized` added
;;;;    16-May-2011 (CT) `lse-version` increased
;;;;    29-May-2011 (CT) `lse-vcs` added, `lse-version` increased
;;;;     3-Jun-2011 (CT) `lse-git` added, `lse-version` increased
;;;;     3-Jun-2011 (CT) `lse-w32` factored
;;;;    18-Feb-2012 (CT) Move `require lse-tpu` to the front
;;;;    19-Feb-2012 (CT) `lse-version` increased
;;;;    15-Mar-2012 (CT) Use `lse-frame:title-prefix`, increase `lse-version`
;;;;     4-Jul-2012 (CT) Remove `enable-multibyte-characters` to please Emacs 24
;;;;    23-Nov-2012 (CT) Add `lse-emacs24-p`
;;;;     5-Sep-2013 (CT) Add `lse-emacs24.4-p`;
;;;;                     Use `version<`, not `string-lessp`
;;;;     4-Nov-2014 (CT) Don't set frame title in `lse-terminal-setup`
;;;;     6-Nov-2014 (CT) Add `lse-emacs24.3-p`
;;;;     9-Nov-2014 (CT) Remove compiler warnings, increase `lse-version`
;;;;    12-Nov-2014 (CT) Remove support for ancient Emacs versions,
;;;;                     increase `lse-version`
;;;;    12-Nov-2014 (CT) Remove support for ancient Emacs versions
;;;;    20-Nov-2014 (CT) Increase `lse-version`
;;;;    ««revision-date»»···
;;;;--
(provide 'ls-emacs)

(defconst lse-version      "4.1.0"        "Version number of LS-Emacs.")
(defconst lse-version-date "20-Nov-2014 " "Date of last change of LS-Emacs.")

(defconst lse-emacs19-p (not (version< emacs-version "19"))
  "Non-NIL if we are running GNU Emacs version 19."
)

(defconst lse-emacs20-p (not (version< emacs-version "20"))
  "Non-NIL if we are running GNU Emacs version 20."
);  4-Dec-1997

(defconst lse-emacs20.3-p (not (version< emacs-version "20.3"))
  "Non-NIL if we are running GNU Emacs version 20.3."
); 24-May-1999

(defconst lse-emacs21-p (not (version< emacs-version "21"))
  "Non-NIL if we are running GNU Emacs version 21."
); 24-Mar-2002

(defconst lse-emacs22-p (not (version< emacs-version "22"))
  "Non-NIL if we are running GNU Emacs version 22."
);  1-Oct-2007

(defconst lse-emacs23-p (not (version< emacs-version "23"))
  "Non-NIL if we are running GNU Emacs version 23."
);  5-Aug-2009

(defconst lse-emacs24-p (not (version< emacs-version "24"))
  "Non-NIL if we are running GNU Emacs version 24."
); 23-Nov-2012

(defconst lse-emacs24.3-p (not (version< emacs-version "24.3"))
  "Non-NIL if we are running GNU Emacs version 24.3."
);  6-Nov-2014

(defconst lse-emacs24.4-p (not (version< emacs-version "24.4"))
  "Non-NIL if we are running GNU Emacs version 24.4."
);  5-Sep-2013

(defconst lse-emacsX-p window-system
  "Non-NIL if running under X"
)

(setq default-load-path load-path)

;; load-path is to be defined by environment variable EMACSLOADPATH
(let ((ldir     (getenv "EMACSLSEDIR"))
      (lsrc     (getenv "EMACSLSESRC"))
      (lscripts (getenv "EMACSLSESCRIPTS"))
     )
  (defvar lse-directory     (or ldir     "/swing/project/ls-emacs/lsc"))
  (defvar lse-src-directory (or lsrc     "/swing/project/ls-emacs/lse"))
  (defvar lse-script-dir    (or lscripts "/swing/project/ls-emacs/scripts"))
)
(setq lse-load-path
  (append (list lse-directory lse-src-directory) load-path nil)
)

;;;+
;;; lse_current_fill-in stores the fill-in_info about the currently
;;; considered (flat) fill-in.
;;; lse_dead_fill-in stores the fill-in_info about the last cleaned
;;; fill-in.
;;;-
(defvar                      lse_current_fill-in     nil)
(make-variable-buffer-local 'lse_current_fill-in)
(defvar                      lse_dead_fill-in     nil)
(make-variable-buffer-local 'lse_dead_fill-in)

(defun lse-initialize ()
  (if noninteractive       ;  9-Jul-1995
      t                    ;  9-Jul-1995 no need for pre-loading of languages
    (lse-language:pre-load "aufwandserfassung")
    (lse-language:pre-load "awk")
    (lse-language:pre-load "bash")
    (lse-language:pre-load "buchhaltung")
    (lse-language:pre-load "bib-kartei")
    (lse-language:pre-load "c"); 23-Jan-2002
    (lse-language:pre-load "cc"); 24-Sep-1994
    (lse-language:pre-load "elisp")
    (lse-language:pre-load "finanzplanung")
    (lse-language:pre-load "firma-kartei")
    (lse-language:pre-load "generic")
    (lse-language:pre-load "gnats-pr"); 27-Sep-2001
    (lse-language:pre-load "kassabuch")
    (lse-language:pre-load "latex")
    (lse-language:pre-load "lse")
    (lse-language:pre-load "mail"); 25-May-1995
    (lse-language:pre-load "perl")
    (lse-language:pre-load "person-kartei")
    (lse-language:pre-load "python");  7-Jan-1998
    (lse-language:pre-load "quot-kartei")
    (lse-language:pre-load "rest");
    (lse-language:pre-load "sed")
    (lse-language:pre-load "texinfo")
  )
)

(defun lse-terminal-setup ()
  (if noninteractive; 17-Dec-1997
      t             ; 17-Dec-1997 no need for setting up the terminal
    (lse-replace-std-emacs-bindings)
    (lse-define-std-keys)
    (unless lse-global-home-mark-initialized
      (lse-set-home-mark-global (point-marker))
    )
    (lse-window:mark-stack:initialize)
    (lse-ring-bell)
  )
)

(require 'lse-basics)
(require 'lse-list-util)
(require 'lse-tpu)

(require 'lse-buffer)
(require 'lse-buffer-list)
(require 'lse-byte-compile)
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
(require 'lse-git)
(require 'lse-indent)
(require 'lse-interactive)
(require 'lse-keys)
(require 'lse-kill)
(require 'lse-language)
(require 'lse-learn-key)
(require 'lse-mark-stack)
(require 'lse-menu); 16-Oct-1996
(require 'lse-mode-alist)
(require 'lse-range)
(require 'lse-session)
(require 'lse-vcs)
(require 'lse-window)
(require 'lse-tpu-keys)
(require 'lse-cal)

(lse-initialize)
(if noninteractive       ;  20-Jul-1995
    t                    ;  20-Jul-1995 no need for defining keys
  (unless (eq system-type 'windows-nt);  6-Sep-2002
   (require 'server)
   (unless (fboundp 'server-running-p)
     ;;; 17-Nov-2009; `server-running-p` doesn't exist in 22.x
     (defun server-running-p (&optional x) nil)
   )
   (unless (server-running-p) (server-start))
   (setq server-temp-file-regexp
     (concat "MH/draft/[0-9]+\\|" server-temp-file-regexp)
   )
  )
  (lse-define-tpu-keys)
  (lse-define-tpu-gold-keys)
  (lse-define-tpu-blue-keys)
  (lse-define-cursor-movements); 26-Aug-2002
  (lse-define-deletion-keys);    26-Aug-2002
  (lse-define-insertion-keys);   30-Aug-2002
)

(add-hook 'after-init-hook 'lse-terminal-setup)

;;; __END__ ls-emacs.el
