;-*-unibyte: t;-*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work
 
;;;;unix_ms_filename_correspondency swing-kartei--summary.el swi_ksmy.el
;;;; (c) 1994 Swing Informationssysteme GmbH. All rights reserved.
;;;;++
;;;; Name
;;;;    swing-kartei--summary
;;;;
;;;; Purpose
;;;;    Functions for handling kartei summaries
;;;;
;;;; Revision Dates
;;;;    16-Aug-1994 (CT) Creation (of comment)
;;;;    16-Aug-1994 (CT) case-fold-search added
;;;;    ««revision-date»»···
;;;;-- 
(provide 'swing-kartei--summary)

(defun swing-kartei:summary:entry-name-pattern (&optional name)
  (concat "^ +"
          "\\("
            (or name "[^ \t]*")
          "\\)"
          " *\t"
  )
)

(defun swing-kartei:summary:entry-line-pattern (&optional name)
  (concat (swing-kartei:summary:entry-name-pattern name)
          ".*\n?"
  )
)

(defun swing-kartei:summary:find-entry (name &optional quiet)
  (interactive "sName of entry: ")
  (if (re-search-forward (swing-kartei:summary:entry-line-pattern name) nil t)
      (goto-char (match-beginning 0))
    (if (not quiet)
        (error "Could not find entry %s in summary file" name)
    )
    nil
  )
; swing-kartei:summary:find-entry
)

(defun swing-kartei:summary:entry-value (name &optional quiet)
  (interactive "sName of entry: ")
  (if (swing-kartei:summary:find-entry name quiet)
      (buffer-substring-no-properties (match-beginning 0) (match-end 0))
  )
; swing-kartei:summary:entry-value
)

(defun swing-kartei:summary:successor (entry-name)
  (let ((successor "")
        (case-fold-search nil)
       )
    (goto-char 1)
    (while (and (string< successor entry-name)
                (re-search-forward (swing-kartei:summary:entry-name-pattern)
                                   nil t
                )
           )
      (setq successor
            (buffer-substring-no-properties (match-beginning 1) (match-end 1))
      )
      (goto-char (match-end 0))
    )
    (if (string< successor entry-name)
        (progn
          (setq successor nil); new entry larger than all existing
          (goto-char (point-max))
        )
      (beginning-of-line 1)
    )
    successor
  )
; swing-kartei:summary:successor
)


(defun swing-kartei:summary:delete
           (entry-name in-buffer kartei-case-fold-search)
  (save-excursion
    (set-buffer in-buffer)
    (setq case-fold-search kartei-case-fold-search)
    (if (looking-at (swing-kartei:summary:entry-line-pattern entry-name))
        t
      (goto-char 1)
      (swing-kartei:summary:find-entry entry-name)
    )
    (delete-region (match-beginning 0) (match-end 0))
    (if (looking-at (swing-kartei:summary:entry-name-pattern))
        (buffer-substring-no-properties (match-beginning 1) (match-end 1))
    )
  )
; swing-kartei:summary:delete
)
