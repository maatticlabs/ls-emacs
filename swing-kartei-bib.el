;-*- coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it, character ranges

;;;; Copyright (C) 1994-2007 Mag. Christian Tanzer. All rights reserved
;;;; Glasauergasse 32, A--1130 Wien, Austria. tanzer@swing.co.at
;;;;++
;;;; Name
;;;;    swing-kartei-bib
;;;;
;;;; Purpose
;;;;    Management of swing bib kartei
;;;;
;;;; Revision Dates
;;;;    26-Aug-1994 (CT) Creation (of comment)
;;;;    26-Aug-1994 (CT) swing-kartei-bib:change-directory added
;;;;    14-Nov-1994 (CT) Error corrected
;;;;                     (DefBibSubtitle instead of DefBibSubTitle)
;;;;    15-Oct-1995 (CT) swing-kartei:bib:check-isbn added
;;;;    21-May-2007 (CT) swing-kartei:bib:check-isbn changed to use
;;;;                     `/swing/python/isbn.py` instead of
;;;;                     `/swing/local/bin/isbn`
;;;;    ««revision-date»»···
;;;;--
(provide       'swing-kartei-bib)

(defconst swing-kartei:bib:directory      swing-kartei:directory)
(defconst swing-kartei:bib:file-name      "bib")
(defconst swing-kartei:bib:file-directory "/swing/bib/")
(defconst swing-kartei:bib:language       "bib-kartei")

(defvar   swing-kartei:bib-history        nil)

(defun swing-kartei:bib:make-entry-summary (limit)
  ;; completion uses entry field
  (swing-kartei:copy-field src-buf dst-buf limit "entry"           "\t" 15)
  (swing-kartei:copy-field src-buf dst-buf limit "DefBibAuthor/"   "\t" 55)
  (swing-kartei:copy-field src-buf dst-buf limit "DefBibTitle/"    ". ")
  (swing-kartei:copy-field src-buf dst-buf limit "DefBibSubtitle/" "\t"); 14-Nov-1994
  (swing-kartei:copy-field src-buf dst-buf limit "entryflag"       "\n")
; swing-kartei:bib:make-entry-summary
)

(defun swing-kartei:bib:make-summary ()
  "Generate summary file for bibliographic kartei"
  (interactive)
  (swing-kartei:make-summary
       swing-kartei:bib:directory
       swing-kartei:bib:file-name
       'swing-kartei:bib:make-entry-summary
  )
; swing-kartei:bib:make-summary
)

(defun swing-kartei:bib:change ()
  "Change entry of bibliographic kartei"
  (interactive)
  (swing-kartei:change
       swing-kartei:bib:directory
       swing-kartei:bib:file-name
       swing-kartei:bib:file-directory
       'swing-kartei:bib:make-entry-summary
       swing-kartei:bib:language
       nil
       t
  )
; swing-kartei:bib:change
)

(defun swing-kartei:bib:add ()
  "Add entry to bibliographic kartei"
  (interactive)
  (swing-kartei:add
       swing-kartei:bib:directory
       swing-kartei:bib:file-name
       swing-kartei:bib:file-directory
       'swing-kartei:bib:make-entry-summary
       swing-kartei:bib:language
       nil
       t
  )
; swing-kartei:bib:add
)

(defun swing-kartei:bib:split ()
  "Split bib kartei into one file per entry"
  (interactive)
  (swing-kartei:split
       swing-kartei:bib:directory
       swing-kartei:bib:file-name
       swing-kartei:bib:file-directory
  )
; swing-kartei:bib:split
)

(defun swing-kartei:bib:sort ()
  "Sort bib kartei alphabetically (warning: this normally takes a very long time!)"
  (interactive)
  (swing-kartei:sort swing-kartei:bib:directory swing-kartei:bib:file-name)
; swing-kartei:bib:sort
)

(defun swing-kartei:bib:change-directory (&optional val)
  "Change directory of bib kartei."
  (interactive)
  (or val
      (setq val
            (lse-read-file-name "directory of bib kartei: "
                                swing-kartei:bib:directory
            )
      )
  )
  (setq swing-kartei:bib:directory val)
; swing-kartei:bib:change-directory
)

;;; 15-Oct-1995
(defun swing-kartei:bib:check-isbn (isbn)
  (if (eq (call-process "/swing/python/isbn.py" nil
                        (get-buffer-create "*Help*") nil
                        isbn
          ) 0
      )
      (lse-message (concat "ISBN '" isbn "' is valid"))
    (lse-message (concat "ISBN '" isbn "' is invalid"))
    nil
  )
; swing-kartei:bib:check-isbn
)

