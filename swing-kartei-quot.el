;-*- coding: utf-8 -*-
 
;;;;unix_ms_filename_correspondency swing-kartei-quot.el swi_kqut.el
;;;; Copyright (C) 1994 Mag. Christian Tanzer. All rights reserved.
;;;; Glasauergasse 32, A--1130 Wien, Austria. tanzer.co.at
;;;;++
;;;; Name
;;;;    swing-kartei-quot
;;;;
;;;; Purpose
;;;;    Management of swing quot kartei
;;;;
;;;; Revision Dates
;;;;    26-Aug-1994 (CT) Creation (of comment)
;;;;    26-Aug-1994 (CT) swing-kartei-quot:change-directory added
;;;;     2-Oct-1996 (CT) swing-kartei:quot:ispell defined
;;;;    ««revision-date»»···
;;;;-- 
(provide       'swing-kartei-quot)

(defconst swing-kartei:quot:directory      swing-kartei:directory)
(defconst swing-kartei:quot:file-name      "quotations")
(defconst swing-kartei:quot:file-directory "/swing/quot/")
(defconst swing-kartei:quot:language       "quot-kartei")

(defvar   swing-kartei:quotations-history  nil)

(defun swing-kartei:quot:make-entry-summary (limit)
  ;; completion uses entry field
  (swing-kartei:copy-field src-buf dst-buf limit "entry"                   "\t"  23)
  (swing-kartei:copy-field src-buf dst-buf limit "def\\\\QuotSource/"      "\t"  31)
  (swing-kartei:copy-field src-buf dst-buf limit "def\\\\QuotSourcePage/"  "\t"  47)
  (swing-kartei:copy-field src-buf dst-buf limit "def\\\\QuotCreator/"     "\n")
; swing-kartei:quot:make-entry-summary
)

(defun swing-kartei:quot:make-summary ()
  "Make summary file for quotation kartei"
  (interactive)
  (swing-kartei:make-summary
       swing-kartei:quot:directory
       swing-kartei:quot:file-name
       'swing-kartei:quot:make-entry-summary
  )
; swing-kartei:quot:make-summary
)

(defvar swing-kartei:quot:check-spelling t);  2-Oct-1996

;;;  2-Oct-1996 
(defun swing-kartei:quot:ispell ()
  "Check spelling of QuotContents with ispell"
  (interactive)
  (let ((ispell-command-options "-p /swing/kartei/.ispell_words"))
    (save-excursion 
      (goto-char (point-min))
      (if (and swing-kartei:quot:check-spelling
            (re-search-forward
               (concat "\\\\" "QuotContents/" swing-kartei:pattern:arg-of-macro) 
               nil
               t
            )
          )
          (condition-case nil
              (ispell-region (match-beginning 2) (match-end 2))
            (error nil)
          )
      )
    )
  )
; swing-kartei:quot:ispell
)

(defun swing-kartei:quot:change ()
  "Change entry of quotation kartei"
  (interactive)
  (let ((ispell-command-options "-p /swing/kartei/.ispell_words")
       )
    (swing-kartei:change 
         swing-kartei:quot:directory
         swing-kartei:quot:file-name
         swing-kartei:quot:file-directory
         'swing-kartei:quot:make-entry-summary
         swing-kartei:quot:language
         'swing-kartei:quot:ispell
         t
    )
  )
; swing-kartei:quot:change
)

(defun swing-kartei:quot:add ()
  "Add entry to quotation kartei"
  (interactive)
  (swing-kartei:add 
       swing-kartei:quot:directory
       swing-kartei:quot:file-name
       swing-kartei:quot:file-directory
       'swing-kartei:quot:make-entry-summary
       swing-kartei:quot:language
       'swing-kartei:quot:ispell
       t
  )
; swing-kartei:quot:add
)

(defun swing-kartei:quot:split ()
  "Split quot kartei into one file per entry"
  (interactive)
  (swing-kartei:split 
       swing-kartei:quot:directory
       swing-kartei:quot:file-name
       swing-kartei:quot:file-directory
  )
; swing-kartei:quot:split
)

(defun swing-kartei:quot:sort ()
  "Sort quot kartei alphabetically (warning: this normally takes a very long time!)"
  (interactive)
  (swing-kartei:sort swing-kartei:quot:directory swing-kartei:quot:file-name)
; swing-kartei:quot:sort
)

(defun swing-kartei:quot:change-directory (&optional val)
  "Change directory of quot kartei."
  (interactive)
  (or val
      (setq val
            (lse-read-file-name "directory of quot kartei: "
                                swing-kartei:quot:directory
            )
      )
  )
  (setq swing-kartei:quot:directory val)
; swing-kartei:quot:change-directory
) 
