;-*-unibyte: t;-*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work

;;;;unix_ms_filename_correspondency lse-session:el lse_sssn:el
;;;; (c) 1994 Swing Informationssysteme GmbH. All rights reserved.

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
(defun lse-system-domain ()
  (let* ((sn   (system-name))
         (tail (string-match "\\." sn))
        )
    (substring sn (1+ tail))
  )
; lse-system-domain
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

(defun lse-insert-user-abbr-name ();  3-May-1995
  (interactive "*")
  (lse-tpu:insert (lse-user-abbr-name))
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
  (lse-tpu:insert (lse-buffer:base-name (current-buffer)))
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

;;;; ****************************** site-specific *****************************
(cond ((or (string-match "swing"       lse-session:system-name); 11-Jan-1998
           (string-match "^t[0-9a-z]$" lse-session:system-name); 29-Dec-2004
       );  3-Nov-2004
       ;;;; ****************************** site-specific *****************************
       (defun lse-user-full-name ()
         (if (eq system-type 'vax-vms)
             (let ((user (user-login-name)))
               (cond ((string= user "appoyer")     "Heinz Appoyer")
                     ((string= user "froelich")    "Eleanor Froelich")
                     ((string= user "koenighofer") "Gerhard Koenighofer")
                     ((string= user "koenig")      "Gerhard Koenighofer")
                     ((string= user "rainer")      "Ulrich Rainer-Harbach")
                     ((string= user "tanzer")      "Christian Tanzer")
                     ((string= user "root")        "Christian R. Tanzer")
                     (t user)
               )
             )
           (let ((user (user-full-name)))
             (if (string= user "root") "Christian Tanzer" user)
           )
         )
       )

       ;;;; ****************************** site-specific *****************************
       (defun lse-user-abbr-name ();  3-May-1995
         (let ((user (user-login-name)))
           (cond ((string= user "appoyer")        "H. Appoyer")
                 ((string= user "froelich")       "E. Froelich")
                 ((string= user "koenighofer")    "G. Koenighofer")
                 ((string= user "koenig")         "G. Koenighofer")
                 ((string= user "rainer")         "U. Rainer-Harbach")
                 ((string= user "tanzer")         "C. Tanzer")
                 ((string= user "root")           "C.R. Tanzer")
                 (t "")
           )
         )
       )

       ;;;; ****************************** site-specific *****************************
       (defun lse-user-initials ()
         (let ((user (user-login-name)))
           (cond ((string= user "appoyer")        "HA")
                 ((string= user "froelich")       "EFE")
                 ((string= user "koenighofer")    "GK")
                 ((string= user "koenig")         "GK")
                 ((string= user "rainer")         "RH")
                 ((string= user "tanzer")         "CT")
                 ((string= user "root")           "CT/R")
                 (t "")
           )
         )
       )
      ); end   of swing  definitions
      ;; begin of tttech definitions
      ((or (string-match "ttt" lse-session:system-name)
           (string-match "ctt" lse-session:system-name)
       )
       (defun lse-user-full-name () (user-full-name))

       ;;;; ****************************** site-specific *****************************
       ;;;; 11-Mar-1998
       (defun lse-system-domain ()  "tttech.com")

       ;;;; ****************************** site-specific *****************************
       (defun lse-user-name ()
         (let ((user (user-login-name)))
           (cond ((string= user "gkopetz")  "Kopetz")
                 ((string= user "hkopetz")  "Kopetz"); 13-Jul-1998
                 (t (capitalize user))
           )
         )
       )

       ;;;; ****************************** site-specific *****************************
       (defun lse-user-abbr-name ();  3-May-1995
         (let ((user (user-login-name)))
           (cond
             ((string= user "angelow")       "H. Angelow"); 30-Oct-1998
             ((string= user "bauer")         "G. Bauer");    7-May-1999
             ((string= user "briant")        "Y. Briant");  15-Feb-2000
             ((string= user "eder")          "C. Eder"); 12-Nov-2002
             ((string= user "erkinger")      "E. Erkinger"); 10-Jun-2002
             ((string= user "doppelbauer")   "K. Doppelbauer"); 4-Aug-1999
             ((string= user "gkopetz")       "G. Kopetz")
             ((string= user "glueck")        "M. Glück")
             ((string= user "goller")        "A. Goller"); 12-Nov-2002
             ((string= user "hoeller")       "G. Höller"); 20-Feb-2001
             ((string= user "kober")         "K. Kober"); 20-Feb-2001
             ((string= user "koenighofer")   "G. Könighofer");  9-Oct-2002
             ((string= user "kraft")         "H. Kraft"); 10-Jun-2002
             ((string= user "lettner")       "R. Lettner");    7-Apr-1999
             ((string= user "maier")         "R. Maier");    15-Feb-2000
             ((string= user "niedersuess")   "M. Niedersüß");  7-Apr-1999
             ((string= user "novak")         "M. Novak");  15-Feb-2000
             ((string= user "pisecky")       "M. Pisecky");  1-Jul-1999
             ((string= user "priesch")       "M. Priesch"); 10-Jun-2002
             ((string= user "poledna")       "S. Poledna")
             ((string= user "prammer")       "M. Prammer")
             ((string= user "rugo")          "A. Rugo"); 20-Feb-2001
             ((string= user "schlatterbeck") "R. Schlatterbeck"); 11-May-2000
             ((string= user "schoepf")       "M. Schöpf"); 28-Feb-2001
             ((string= user "schwarz")       "M. Schwarz"); 16-Jul-1999
             ((string= user "smaili")        "I. Smaili"); 13-Jul-1998
             ((string= user "stoeger")       "G. Stöger")
             ((string= user "tanzer")        "C. Tanzer")
             ((string= user "waechter")      "M. Wächter");  7-May-1999
             (t user)
           )
         )
       )

       ;;;; ****************************** site-specific *****************************
       (defun lse-user-initials ()
         (let ((user (user-login-name)))
           (cond
             ((string= user "angelow")        "HA"); 30-Oct-1998
             ((string= user "bauer")          "GB");  7-May-1999
             ((string= user "briant")         "YB"); 15-Feb-2000
             ((string= user "doppelbauer")    "KD");  4-Aug-1999
             ((string= user "eder")           "CED"); 10-Jun-2002
             ((string= user "erkinger")       "EER"); 10-Jun-2002
             ((string= user "feuchtinger")    "MFE"); 20-Feb-2001
             ((string= user "forstinger")     "SF"); 15-Feb-2000
             ((string= user "gkopetz")        "GK")
             ((string= user "glueck")         "MG")
             ((string= user "goller")         "AGO"); 12-Nov-2002
             ((string= user "hoeller")        "GHO"); 20-Feb-2001
             ((string= user "kober")          "KKO"); 20-Feb-2001
             ((string= user "koenighofer")    "GKH");  9-Oct-2002
             ((string= user "kraft")          "HKA"); 10-Jun-2002
             ((string= user "lettner")        "RL");  7-Apr-1999
             ((string= user "maier")          "RM"); 15-Feb-2000
             ((string= user "niedersuess")    "MN");  7-Apr-1999
             ((string= user "novak")          "NV"); 15-Feb-2000
             ((string= user "pisecky")        "MPI");  5-Sep-2002 ; 1-Jul-1999
             ((string= user "poledna")        "SP")
             ((string= user "prammer")        "MP")
             ((string= user "priesch")        "MPH"); 10-Jun-2002
             ((string= user "rugo")           "ARU"); 20-Feb-2001
             ((string= user "schlatterbeck")  "RS"); 11-May-2000
             ((string= user "schoepf")        "MSF"); 28-Feb-2001
             ((string= user "schwarz")        "MS"); 16-Jul-1999
             ((string= user "smaili")         "IS"); 13-Jul-1998
             ((string= user "stoeger")        "GS")
             ((string= user "tanzer")         "CT")
             ((string= user "waechter")       "MW");  7-May-1999
             (t user)
           )
         )
       )
      )
      (t;  3-Nov-2004 ; 22-Oct-2002
       (defun lse-user-full-name ()
         (user-full-name)
       )
       (defun lse-user-name ()
         (user-login-name)
       )
       (defun lse-user-abbr-name ()
         ""
       )
       (defun lse-user-initials ()
         ""
       )
       (defun lse-system-domain ()  "undefined.dontknow")
      )
)

;;;; ****************************** site-specific *****************************
;;; 27-Aug-1996
(defun lse-user-initials-r ()
  (let ((user (user-login-name)))
    (cond ((string= user "root") "root"); 27-Aug-1996
          (t (lse-user-initials))
    )
  )
)
