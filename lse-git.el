;-*- coding: utf-8 -*-

;;;; Copyright (C) 2011-2015 Mag. Christian Tanzer All rights reserved
;;;; Glasauergasse 32, A--1130 Wien, Austria. tanzer@swing.co.at
;;;; #*** <License> ************************************************************#
;;;; This library is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this library. If not, see <http://www.gnu.org/licenses/>.
;;;; #*** </License> ***********************************************************#
;;;;
;;;;++
;;;; Name
;;;;    lse-git
;;;;
;;;; Purpose
;;;;    Provide support for editing git commit message, rebase buffers, ...
;;;;
;;;; Revision Dates
;;;;     3-Jun-2011 (CT) Creation
;;;;    12-May-2012 (CT) Add `set-fill-column` to `72` to `:setup-hook`
;;;;    13-Nov-2014 (CT) Use `lse-keys/define`
;;;;    14-Sep-2015 (CT) Change `set-fill-column` to `70`
;;;;    27-Dec-2015 (CT) Add `magit`
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-git)

;;; 28-May-2011
(defun lse-git:abort ()
  "Abort editing a git buffer -- aborts the git operation, too!"
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (kill-buffer)
  )
  (if (framep server-window) (iconify-frame server-window))
; lse-git:abort
)

;;; 28-May-2011
(defun lse-git:finish ()
  "Finish editing a git buffer"
  (interactive)
  (save-buffer 0)
  (server-edit)
  (if (framep server-window) (iconify-frame server-window))
; lse-git:finish
)

;;; 28-May-2011
(defun lse-git:setup-hook ()
  "Setup buffer for git operation (commit/message, rebase, ...)"
  (when (integerp (string-match ".git/" (buffer-file-name)))
    (lse-keys/define #'local-set-key
      '(
        ([blue gold ?e] lse-git:abort)
        ([?\C-c ?\C-c]  lse-git:finish)
        ([?\C-x ?#]     lse-git:finish)
      )
    )
    (auto-fill-mode  t)
    (set-fill-column 70)
  )
; lse-git:setup-hook
)

(add-hook 'server-switch-hook 'lse-git:setup-hook)

(defvar lse-git:magit-p (require 'magit nil t))

;;;; __END__ lse-git
