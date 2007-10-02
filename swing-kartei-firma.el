;-*- unibyte: t; coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work
 
;;;;unix_ms_filename_correspondency swing-kartei-firma.el swi_kfrm.el
;;;; Copyright (C) 1994 Mag. Christian Tanzer. All rights reserved.
;;;; Glasauergasse 32, A--1130 Wien, Austria. tanzer.co.at
;;;;++
;;;; Name
;;;;    swing-kartei-firma
;;;;
;;;; Purpose
;;;;    Management of swing firma kartei
;;;;
;;;; Revision Dates
;;;;    26-Aug-1994 (CT) Creation (of comment)
;;;;    26-Aug-1994 (CT) swing-kartei-firma:change-directory added
;;;;    ««revision-date»»···
;;;;-- 
(provide       'swing-kartei-firma)

(defconst swing-kartei:firma:directory       swing-kartei:directory)
(defconst swing-kartei:firma:file-name       "firma")
(defconst swing-kartei:firma:file-directory  "/swing/fkartei/")
(defconst swing-kartei:firma:language        "firma-kartei")

(defvar   swing-kartei:firma-history         nil)

(defun swing-kartei:firma:make-entry-summary (limit)
  ;; completion uses entry field
  (swing-kartei:copy-field src-buf dst-buf limit "entry"         "\t"  24)
  (swing-kartei:copy-field src-buf dst-buf limit "entrystate"    "\t"  38)
  (swing-kartei:copy-field src-buf dst-buf limit "DefFirmaKurz"  "\t"  50)
  (swing-kartei:copy-field src-buf dst-buf limit "DefFirmenName" "\t"  78)
  (swing-kartei:copy-field src-buf dst-buf limit "entryflag"     "\n")
; swing-kartei:firma:make-entry-summary
)

(defun swing-kartei:firma:make-summary ()
  "Generate summary file for firma kartei"
  (interactive)
  (swing-kartei:make-summary
       swing-kartei:firma:directory
       swing-kartei:firma:file-name
       'swing-kartei:firma:make-entry-summary
  )
; swing-kartei:firma:make-summary
)

(defun swing-kartei:firma:change ()
  "Change entry of firma kartei"
  (interactive)
  (swing-kartei:change 
       swing-kartei:firma:directory
       swing-kartei:firma:file-name
       swing-kartei:firma:file-directory
       'swing-kartei:firma:make-entry-summary
       swing-kartei:firma:language
  )
; swing-kartei:firma:change
)

(defun swing-kartei:firma:add ()
  "Add entry to firma kartei"
  (interactive)
  (swing-kartei:add 
       swing-kartei:firma:directory
       swing-kartei:firma:file-name
       swing-kartei:firma:file-directory
       'swing-kartei:firma:make-entry-summary
       swing-kartei:firma:language
  )
; swing-kartei:firma:add
)

(defun swing-kartei:firma:split ()
  "Split firma kartei into one file per entry"
  (interactive)
  (swing-kartei:split 
       swing-kartei:firma:directory
       swing-kartei:firma:file-name
       swing-kartei:firma:file-directory
  )
; swing-kartei:firma:split
)

(defun swing-kartei:firma:sort ()
  "Sort firma kartei alphabetically (warning: this normally takes a very long time!)"
  (interactive)
  (swing-kartei:sort swing-kartei:firma:directory swing-kartei:firma:file-name)
; swing-kartei:firma:sort
)

(defun swing-kartei:firma:change-directory (&optional val)
  "Change directory of firma kartei."
  (interactive)
  (or val
      (setq val
            (lse-read-file-name "directory of firma kartei: "
                                swing-kartei:firma:directory
            )
      )
  )
  (setq swing-kartei:firma:directory val)
; swing-kartei:firma:change-directory
) 
