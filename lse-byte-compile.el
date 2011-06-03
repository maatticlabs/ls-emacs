;-*- coding: iso-8859-15; -*-

;;;; Copyright (C) 1997-2011 Mag. Christian Tanzer. All rights reserved.
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

;;;;
;;;;++
;;;; Name
;;;;    lse-byte-compile
;;;;
;;;; Purpose
;;;;    Compile ls-emacs elisp source files which are compilable without errors
;;;;
;;;; Revision Dates
;;;;    14-Dec-1997 (CT) Creation
;;;;    13-Oct-2007 (CT) Complete overhaul
;;;;    15-Oct-2007 (CT) `lse-interactive.el` removed from `black-list`
;;;;    18-Oct-2007 (CT) `lse-config` added
;;;;    10-Nov-2010 (CT) Use `mapc` instead of `mapcar` where appropriate
;;;;    31-Jan-2011 (CT) `lse-hash` added
;;;;    29-May-2011 (CT) `lse-vcs` added
;;;;    ««revision-date»»···
;;;;--

(provide 'lse-byte-compile)

(defvar   lse-byte-compile:source-dir "/swing/project/ls-emacs/")

;;; 13-Oct-2007
(defvar   lse-byte-compile:load t)

;;; 13-Oct-2007
(defvar lse-byte-compile:black-list
  (list
    ;;; compilation of lse-fill-in--search.el goes into a endless loop
    "lse-fill-in--search"
  )
  "Files that can't be compiled without breaking LS-Emacs"
)

;;; 13-Oct-2007
(defvar lse-byte-compile:files
  (list
    "ls-emacs"
    "lse-basics"
    "lse-buffer-list"
    "lse-buffer"
    "lse-byte-compile"
    "lse-cal"
    "lse-command"
    "lse-comment"
    "lse-compilation"
    "lse-completion"
    "lse-config"
    "lse-deep-fill-in"
    "lse-define"
    "lse-editing"
    "lse-face"
    "lse-file"
    "lse-fill-in--delimiters"
    "lse-fill-in-history"
    "lse-fill-in-info"
    "lse-fill-in-marks"
    "lse-fill-in"
    "lse-flat-fill-in"
    "lse-frame"
    "lse-git"
    "lse-hash"
    "lse-indent"
    "lse-interactive"
    "lse-keys-v19"
    "lse-keys"
    "lse-kill"
    "lse-language"
    "lse-learn-key"
    "lse-list-util"
    "lse-mark-stack"
    "lse-menu"
    "lse-mode-alist"
    "lse-range"
    "lse-session"
    "lse-tpu-keys-v19"
    "lse-tpu-keys"
    "lse-tpu"
    "lse-vcs"
    "lse-window"
  )
)

;;; 13-Oct-2007
(defun lse-byte-compile:all (&rest files)
  "Byte-compile all LS-Emacs files that are safe for compiling and load them
into Emacs."
  (interactive)
  (mapc 'lse-byte-compile:one (or files lse-byte-compile:files))
; lse-byte-compile:all
)

;;; 13-Oct-2007
(defun lse-byte-compile:current ()
  "Byte-compile current file and load it into Emacs."
  (interactive)
  (save-match-data
    (let ((full-name (buffer-file-name)))
      (if (string-match "/\\([-a-zA-Z0-9_]+\\)\\.el" full-name)
          (let ((file (match-string 1 full-name)))
            (if (buffer-modified-p)
                (save-buffer)
            )
            (lse-byte-compile:one file)
          )
        (message "Not a elisp buffer '%s'?" full-name)
      )
    )
  )
; lse-byte-compile:current
)

;;; 13-Oct-2007
(defun lse-byte-compile:one (file)
  (let ((full-name
          (lse-file:expanded-name
            (concat lse-byte-compile:source-dir file ".el")
          )
        )
       )
    (if (member file lse-byte-compile:black-list)
        (save-excursion
          (message
            "File %s is not safe for compiling, evaluating it instead." file
          )
          (eval-buffer (or (get-file-buffer full-name) (find-file full-name)))
        )
      (byte-compile-file full-name lse-byte-compile:load)
    )
  )
; lse-byte-compile:one
)

;;; 13-Oct-2007
(defun lse-byte-compile:is-lse-file-p ()
  (let (result)
    (save-match-data
      (setq result
        (string-match
          "\\(ls-emacs\\|lse-[-a-zA-Z0-9_]+\\|swing-[-a-zA-Z0-9_]+\\)\\.el"
          (buffer-file-name)
        )
      )
    )
    result
  )
; lse-byte-compile:is-lse-file-p
)

;;;  __END__ lse-byte-compile.el
