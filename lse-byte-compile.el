;-*- coding: utf-8 -*-

;;;; Copyright (C) 1997-2016 Mag. Christian Tanzer. All rights reserved.
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
;;;;    17-Feb-2012 (CT) Defvar `:source-dir` as `nil`
;;;;    17-Feb-2012 (CT) Add and use `:setup-source-dir`, `:extra-source-dirs`
;;;;    16-May-2013 (CT) Remove `lse-fill-in--search` from `black-list`
;;;;    12-Nov-2014 (CT) Remove support for ancient Emacs versions
;;;;     6-Mar-2015 (CT) Add `lse-byte-compile:test-dot-emacs`
;;;;    15-Jul-2015 (CT) Add `lse-python` to `lse-byte-compile:files`
;;;;    ««revision-date»»···
;;;;--

(provide 'lse-byte-compile)

(defvar   lse-byte-compile:all-source-dirs nil
  "All source directories for ls-emacs specific files, determined automagically"
)

(defvar   lse-byte-compile:extra-source-dirs nil
  "Additional source directories for ls-emacs specific files, set in ~/.emacs"
)

(defvar   lse-byte-compile:source-dir nil
  "Source directory for ls-emacs specific files, determined automagically"
)

;;; 13-Oct-2007
(defvar   lse-byte-compile:load t
  "Load the file after compiling it?"
)

;;; 13-Oct-2007
(defvar lse-byte-compile:black-list
  (list
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
    "lse-fill-in--search"
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
    "lse-keys"
    "lse-kill"
    "lse-language"
    "lse-learn-key"
    "lse-list-util"
    "lse-mark-stack"
    "lse-menu"
    "lse-mode-alist"
    "lse-python"
    "lse-range"
    "lse-session"
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
  (lse-byte-compile:setup-source-dir)
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
            (lse-byte-compile:setup-source-dir)
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
  (let (full-name)
    (let ((load-path lse-byte-compile:all-source-dirs))
      (setq full-name (locate-library file))
    )
    (when (string-ends-with full-name ".elc")
      (setq full-name (substring full-name 0 -1))
    )
    (if (member file lse-byte-compile:black-list)
        (save-mark-and-excursion
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

;;; 17-Feb-2012
(defun lse-byte-compile:setup-source-dir ()
  (unless lse-byte-compile:all-source-dirs
    (unless (stringp lse-byte-compile:source-dir)
      (setq lse-byte-compile:source-dir
        (file-name-directory (locate-library "lse-byte-compile"))
      )
    )
    (unless (string-ends-with lse-byte-compile:source-dir "/")
      (setq lse-byte-compile:source-dir
        (concat lse-byte-compile:source-dir "/")
      )
    )
    (setq lse-byte-compile:all-source-dirs
      (append
        lse-byte-compile:extra-source-dirs
        (list lse-byte-compile:source-dir)
      )
    )
  )
; lse-byte-compile:setup-source-dir
)

;;;  6-Mar-2015
(defun lse-byte-compile:test-dot-emacs ()
  ;; http://oremacs.com/2015/03/05/testing-init-sanity/
  (interactive)
  (require 'async)
  (async-start
    (lambda ()
      (shell-command-to-string
        "emacs --batch --eval \"
(condition-case e
    (progn
      (load \\\"~/.emacs\\\")
      (message \\\"-OK-\\\")
    )
  (error
   (message \\\"ERROR!\\\")
   (signal (car e) (cdr e)))
)\""
      )
    )
   `(lambda (output)
      (if (string-match "-OK-" output)
          (when ,(called-interactively-p 'any)
            (message "All is well with .emacs")
          )
        (switch-to-buffer-other-window "*startup error*")
        (delete-region (point-min) (point-max))
        (insert output)
        (search-backward "ERROR!")
      )
    )
  )
; lse-byte-compile:test-dot-emacs
)

;;;  __END__ lse-byte-compile.el
