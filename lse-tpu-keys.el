;-*- coding: iso-8859-15; -*-

;;;;unix_ms_filename_correspondency lse-tpu-keys:el lse_tpks:el
;;;; Copyright (C) 1994-2010 Mag. Christian Tanzer. All rights reserved.
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
;;;;    18-Dec-1997 (CT) `lse-tpu-keys:where-is*' and
;;;;                     `lse-tpu-keys:show-*-keys' added
;;;;    28-Dec-1997 (CT) `lse-tpu-keys:matching-commands' and
;;;;                     `lse-tpu-keys:show-keys-matching' added
;;;;     1-Sep-2002 (CT) `lse-tpu:app-keypad-p`,
;;;;                     `lse-tpu:electric-inserts-p`, and
;;;;                     `lse-tpu:use-control-keys-p` added
;;;;     3-Oct-2007 (CT) `lse-tpu:app-keypad-p` removed
;;;;    10-Nov-2010 (CT) Use `mapc` instead of `mapcar` where appropriate
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

(require 'lse-tpu-keys-v19)

;;; 29-Dec-1997
(setq dos-keypad-mode t)

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
    (save-excursion
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
                             lse-goto-last-position
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
