;-*- coding: utf-8 -*-
 
;;;; Copyright (C) 1994 Mag. Christian Tanzer. All rights reserved.
;;;; Glasauergasse 32, A--1130 Wien, Austria. tanzer.co.at
;;;;++
;;;; Name
;;;;    swing-kartei-person
;;;;
;;;; Purpose
;;;;    Management of swing person kartei
;;;;
;;;; Revision Dates
;;;;    26-Aug-1994 (CT) Creation (of comment)
;;;;    26-Aug-1994 (CT) swing-kartei-person:change-directory added
;;;;    ««revision-date»»···
;;;;-- 
(provide       'swing-kartei-person)

(defconst swing-kartei:person:directory      swing-kartei:directory)
(defconst swing-kartei:person:file-name      "person")
(defconst swing-kartei:person:file-directory "/swing/pkartei/")
(defconst swing-kartei:person:language       "person-kartei")

(defvar   swing-kartei:person-history        nil)

(defun swing-kartei:person:make-entry-summary (limit)
  ;; completion uses entry field
  (swing-kartei:copy-field src-buf dst-buf limit "entry"      "\t"  24)
  (swing-kartei:copy-field src-buf dst-buf limit "entrystate" "\t"  38)
  (swing-kartei:copy-field src-buf dst-buf limit "DefName"    " ")
  (swing-kartei:copy-field src-buf dst-buf limit "DefVorname" ", ")
  (swing-kartei:copy-field src-buf dst-buf limit "DefTitel"   "\t"  65)
  (swing-kartei:copy-field+tail-option src-buf dst-buf limit "DefFirma" "\t" 80)
  (swing-kartei:copy-field src-buf dst-buf limit "entryflag"  "\n")
; swing-kartei:person:make-entry-summary
)

(defun swing-kartei:person:make-summary ()
  "Make summary file for person kartei"
  (interactive)
  (swing-kartei:make-summary
       swing-kartei:person:directory 
       swing-kartei:person:file-name
       'swing-kartei:person:make-entry-summary
  )
; swing-kartei:person:make-summary
)

(defun swing-kartei:person:change ()
  "Change entry of person kartei"
  (interactive)
  (swing-kartei:change 
       swing-kartei:person:directory
       swing-kartei:person:file-name
       swing-kartei:person:file-directory
       'swing-kartei:person:make-entry-summary
       swing-kartei:person:language
  )
; swing-kartei:person:change
)

(defun swing-kartei:person:add ()
  "Add entry to person kartei"
  (interactive)
  (swing-kartei:add 
       swing-kartei:person:directory
       swing-kartei:person:file-name
       swing-kartei:person:file-directory
       'swing-kartei:person:make-entry-summary
       swing-kartei:person:language
  )
; swing-kartei:person:add
)

(defun swing-kartei:person:split ()
  "Split person kartei into one file per entry"
  (interactive)
  (swing-kartei:split 
       swing-kartei:person:directory
       swing-kartei:person:file-name
       swing-kartei:person:file-directory
  )
; swing-kartei:person:split
)

(defun swing-kartei:person:sort ()
  "Sort person kartei alphabetically (warning: this normally takes a very long time!)"
  (interactive)
  (swing-kartei:sort swing-kartei:person:directory 
                     swing-kartei:person:file-name
  )
; swing-kartei:person:sort
)

(defun swing-kartei:person:change-directory (&optional val)
  "Change directory of person kartei."
  (interactive)
  (or val
      (setq val
            (lse-read-file-name "directory of person kartei: "
                                swing-kartei:person:directory
            )
      )
  )
  (setq swing-kartei:person:directory val)
; swing-kartei:person:change-directory
) 
