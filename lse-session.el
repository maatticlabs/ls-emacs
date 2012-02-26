;-*- coding: iso-8859-15; -*-

;;;;unix_ms_filename_correspondency lse-session:el lse_sssn:el
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
;;;;    lse-session
;;;;
;;;; Purpose
;;;;    Provide functions related to the current session
;;;;
;;;; Revision Dates
;;;;    30-May-1994 (CT) Creation (of comment)
;;;;    30-May-1994 (CT) lse-shell-command added
;;;;    18-Jun-1994 (CT) lse-file-name-sans-extension added
;;;;    20-Feb-1995 (CT) lse-file-name-extension added
;;;;    10-Mar-1995 (CT) Heinz Appoyer added to known names
;;;;     3-May-1995 (CT) lse-user-abbr-name and lse-insert-user-abbr-name added
;;;;    12-Aug-1996 (CT) lse-system-name addded
;;;;    27-Aug-1996 (CT) lse-user-initials-r added
;;;;     5-Sep-1996 (CT) Eleanor Froelich added to known names
;;;;    11-Jan-1998 (CT) Site-specific definitions depedent of system-name
;;;;    12-Jan-1998 (CT) `lse-insert-user-login-name' added
;;;;    12-Jan-1998 (CT) `lse-system-domain' and `lse-insert-system-domain'
;;;;                     added
;;;;    29-Apr-1998 (CT) Glueck and friends added
;;;;    13-Jul-1998 (CT) Smaili added
;;;;    26-Aug-1998 (CT) `lse-yyyy/mm/dd' and `lse-insert-yyyy/mm/dd' added
;;;;    30-Oct-1998 (CT) Angelow and Langmaier added
;;;;    11-Jan-1999 (CT) Changed `lse-user-e-mail-address'
;;;;     7-Apr-1999 (CT) Benesch, Lettner, and Niedersüß added
;;;;     7-May-1999 (CT) Bauer and Wächter added
;;;;     1-Jul-1999 (CT) Pisecky added
;;;;    16-Jul-1999 (CT) Schwarz added
;;;;     4-Aug-1999 (CT) Doppelbauer added
;;;;    18-Oct-1999 (CT) Glawan added
;;;;     6-Feb-2002 (CT) `lse-dd-mmm-yyyy` changed to use `format` instead of
;;;;                     `concat` (the latter gives a nasty error in 21.1.1)
;;;;     3-Apr-2003 (CT) Use `lse-buffer:base-name`
;;;;     4-Apr-2003 (CT) `lse-date-day0` added and used for `lse-yyyy/mm/dd`
;;;;     3-Nov-2004 (CT) `^t[0-9]$` added to `string-match`ing of
;;;;                     `lse-session:system-name`
;;;;    24-Mar-2005 (CT) Small changes when trying to make it work on `ty`
;;;;                     (finally, it turned out to be missing byte
;;;;                     compilation, arrrrgggh)
;;;;    24-May-2007 (CT) TTTech site-specific code removed
;;;;    24-May-2007 (CT) `(defun lse-system-domain () "swing.co.at")` added
;;;;                     to site-specific code for `swing`
;;;;     6-Dec-2007 (CT) `lse-insert-buffer-name-plus-extension` added
;;;;    12-Nov-2009 (CT) `lse-new-site-info` and functions using it added
;;;;                     (and: site-specific function definitions removed)
;;;;    23-Jul-2010 (CT) `lse-insert-date-time-comment` added
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-session)

(defun lse-date-day ()
  (substring (current-time-string) 8 10)
)

;;;  4-Apr-2003
(defun lse-date-day0 ()
  (let ((day (lse-date-day)))
    (if (string= (substring day 0 1) " ")
        (setq day (format "0%s" (substring day 1 2)))
    )
    day
  )
; lse-date-day0
)

(defun lse-date-hour ()
  (substring (current-time-string) 11 13)
)

(defun lse-date-minute ()
  (substring (current-time-string) 14 16)
)

(defun lse-date-month ()
  (substring (current-time-string) 4 7)
)

(defun lse-date-time ()
  (substring (current-time-string) 11 16)
)

(defun lse-date-year ()
  (substring (current-time-string) 20 24)
)

(defun lse-month-mm (month)
  (cond ((string= month "Jan")  1)
        ((string= month "Feb")  2)
        ((string= month "Mar")  3)
        ((string= month "Apr")  4)
        ((string= month "May")  5)
        ((string= month "Jun")  6)
        ((string= month "Jul")  7)
        ((string= month "Aug")  8)
        ((string= month "Sep")  9)
        ((string= month "Oct") 10)
        ((string= month "Nov") 11)
        ((string= month "Dec") 12)
  )
)

(defun lse-dd-mmm-yyyy ()
  (concat (lse-date-day)   "-"
          (lse-date-month) "-"
          (lse-date-year)
  )
)

(defun lse-insert-dd-mmm-yyyy ()
  (interactive "*")
  (lse-tpu:insert (format "%11s" (lse-dd-mmm-yyyy)))
)

(defun lse-insert-dd-mmm-yyyy+blank ()
  (interactive "*")
  (lse-insert+blank-maybe (format "%11s" (lse-dd-mmm-yyyy)))
)

(defun lse-dd-mm-yyyy ()
  (format "%s.%s.%s"
          (lse-date-day)
          (lse-month-mm (lse-date-month))
          (lse-date-year)
  );  6-Feb-2002
)

(defun lse-insert-dd-mm-yyyy ()
  (interactive "*")
  (lse-tpu:insert (format "%10s" (lse-dd-mm-yyyy)))
)

(defun lse-insert-dd-mm-yyyy+blank ()
  (interactive "*")
  (lse-insert+blank-maybe (format "%10s" (lse-dd-mm-yyyy)))
)

;;; 26-Aug-1998
(defun lse-yyyy/mm/dd ()
  (format "%4s/%2.2d/%2s" (lse-date-year)
                          (lse-month-mm (lse-date-month))
                          (lse-date-day0)
  )
)

;;; 26-Aug-1998
(defun lse-insert-yyyy/mm/dd ()
  (interactive "*")
  (lse-tpu:insert (format "%10s" (lse-yyyy/mm/dd)))
)

;;; 26-Aug-1998
(defun lse-insert-yyyy/mm/dd+blank ()
  (interactive "*")
  (lse-insert+blank-maybe (format "%10s" (lse-yyyy/mm/dd)))
)

(defun lse-insert-time ()
  (interactive "*")
  (lse-tpu:insert (lse-date-time))
)

(defun lse-insert-time+blank ()
  (interactive "*")
  (lse-insert+blank-maybe (lse-date-time))
)

(defun lse-insert-year ()
  (interactive "*")
  (lse-tpu:insert (lse-date-year))
)

;;; 23-Jul-2010
(defun lse-insert-date-time-comment ()
  (interactive "*")
  (lse_start_replacement_if_in_fill-in)
  (dotimes (i 3) (lse-tpu:insert (or lse_comment_head_delim "#")))
  (lse-tpu:insert " ")
  (lse-insert-dd-mmm-yyyy+blank)
  (lse-insert-time+blank)
  (if lse_comment_tail_delim
      (dotimes (i 3) (lse-tpu:insert lse_comment_tail_delim))
  )
  (newline-and-indent)
; lse-insert-date-time-comment
)

(defun lse-user-name ()
  (capitalize (user-login-name))
)

(defun lse-user-initials-tex ()
  (concat "\\" (lse-user-initials) "/")
)

;;; 12-Aug-1996
(defun lse-system-name ()
  (let* ((sn   (system-name))
         (tail (string-match "\\." sn))
        )
    (substring sn 0 tail)
  )
; lse-system-name
)

;;; 12-Jan-1998
(defun lse-system-domain-auto ()
  (let* ((sn   (system-name))
         (tail (string-match "\\." sn))
        )
    (substring sn (1+ tail))
  )
; lse-system-domain-auto
)

;;; 12-Jan-1998
(defun lse-insert-system-domain ()
  "Insert the name of the system's internet domain into the current buffer."
  (interactive "*")
  (lse-tpu:insert (lse-system-domain))
; lse-insert-system-domain
)

;;; 12-Jan-1998
;;;; *********************** maybe site-specific *****************************
(defun lse-user-e-mail-address ()
  "Returns the users fully qualified internet e-mail adress.
This is correct only if the locally used domain is a valid internet domain.
"
  (concat (user-login-name) "@" (lse-system-domain)
          " (" (lse-user-full-name) ")"; 12-Jan-1999
  )
; lse-user-e-mail-address
)

;;; 12-Jan-1998
(defun lse-insert-user-e-mail-address ()
  "Insert user's e-mail addresss into the current buffer."
  (interactive "*")
  (lse-tpu:insert (lse-user-e-mail-address))
; lse-insert-user-e-mail-address
)

(defun lse-insert-user-name ()
  (interactive "*")
  (lse-tpu:insert (lse-user-name))
)

;;; 12-Jan-1998
(defun lse-insert-user-login-name ()
  "Insert the login name of the current user into the buffer."
  (interactive "*")
  (lse-tpu:insert (user-login-name))
; lse-insert-user-login-name
)

(defun lse-insert-user-full-name ()
  (interactive "*")
  (lse-tpu:insert (lse-user-full-name))
)

(defun lse-insert-user-initials ()
  (interactive "*")
  (lse-tpu:insert (lse-user-initials))
)

(defun lse-insert-user-initials-tex ()
  (interactive "*")
  (lse-tpu:insert (lse-user-initials-tex))
)

(defun lse-insert-user-name+blank ()
  (interactive "*")
  (lse-insert+blank-maybe (lse-user-name))
)

(defun lse-insert-user--name+blank ()
  (interactive "*")
  (lse-insert+blank-maybe (lse-user-full-name))
)

(defun lse-insert-user-initials+blank ()
  (interactive "*")
  (lse-insert+blank-maybe (lse-user-initials))
)

(defun lse-insert-user-initials-tex+blank ()
  (interactive "*")
  (lse-insert+blank-maybe (lse-user-initials-tex))
)

(defun lse-insert-buffer-name (&optional buf)
  (interactive "*")
  (lse-tpu:insert (lse-buffer:base-name (or buf (current-buffer))))
)

;;;  6-Dec-2007
(defun lse-insert-buffer-name-plus-extension (&optional buf)
  (interactive "*")
  (lse-insert-buffer-name buf)
  (lse-tpu:insert
    (lse-file-name-extension (buffer-name (or buf (current-buffer))))
  )
)

(defun lse-file-name-sans-extension (name)
  (let ((extension (string-match "\\..*$" (or name "")))
        ;; "\\..*$"          returns name without any  extension
        ;; "\\.[^.]*$" would return  name without last extension
       )
    (if extension
        (substring name 0 extension)
      name
    )
  )
)

(defun lse-file-name-extension (name)
  (let ((extension (string-match "\\..*$" name))
        ;; "\\..*$"          returns name without any  extension
        ;; "\\.[^.]*$" would return  name without last extension
       )
    (if extension
        (substring name extension)
      name
    )
  )
;;; lse-file-name-extension
)

(defun lse-shell-command (command &optional flag)
  "Shell around emacs shell-command"
  (interactive
      (list (read-from-minibuffer
                 "Shell command: " nil nil nil 'shell-command-history
            )
            current-prefix-arg
      )
  )
  (lse-window:make-wb-list)
  (shell-command command flag)
  (lse-window:save-temp-hidden)
; lse-shell-command
)

(defvar lse-session:system-name (lse-system-name)); 11-Jan-1998

;;; 12-Nov-2009
(defun lse-new-site-info (kw &optional parent)
  (let ((result
          (if parent
              (copy-hash-table parent)
            (make-hash-table :test 'equal)
          )
        )
        item
        key
        val
       )
    (while kw
      (setq item (car kw))
      (setq kw   (cdr kw))
      (setq key  (car item))
      (setq val  (cdr item))
      (puthash key val result)
    )
    result
  )
; lse-new-site-info
)

(load "lse-site-info")

;;; 12-Nov-2009
(defun lse-company-address ()
  (gethash 'company-address lse-site-info "Earth, Milky Way, Universe")
; lse-company-address
)

;;; 12-Nov-2009
(defun lse-company-e-mail ()
  (let ((result (gethash 'company-e-mail lse-site-info)))
    (if result
        t
      (setq result (concat "office@" (lse-system-domain)))
    )
    result
  )
; lse-company-e-mail
)

;;; 12-Nov-2009
(defun lse-company-name ()
  (gethash 'company-name lse-site-info (lse-user-full-name))
; lse-company-name
)

;;; 12-Nov-2009
(defun lse-system-domain ()
  (gethash 'system-domain lse-site-info "undefined.dontknow")
; lse-system-domain
)

;;; 12-Nov-2009
(defun lse-user-full-name (&optional login)
  (let* ((uln      (or login (user-login-name)))
         (ufn      (user-full-name))
         (user-map (gethash 'user-map:name lse-site-info))
         (result   (if user-map (gethash uln user-map ufn) ufn))
        )
    result
  )
; lse-user-full-name
)

;;; 12-Nov-2009
(defun lse-user-initials (&optional login full)
  (let* ((uln      (or login (user-login-name)))
         (ufn      (or full  (lse-user-full-name)))
         (user-map (gethash 'user-map:initials lse-site-info))
         (result   (if user-map (gethash uln user-map)))
         (case-fold-search nil)
        )
    (if result
        t
      (save-match-data
        (setq result (replace-regexp-in-string "[^A-Z]" "" ufn t))
      )
    )
    result
  )
; lse-user-initials
)

;;; 27-Aug-1996
(defun lse-user-initials-r ()
  (let ((user (user-login-name)))
    (cond ((string= user "root") "root"); 27-Aug-1996
          (t (lse-user-initials))
    )
  )
)
