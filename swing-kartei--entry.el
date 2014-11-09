;-*- coding: utf-8 -*-

;;;;unix_ms_filename_correspondency swing-kartei--entry.el swi_kntr.el
;;;; Copyright (C) 1994-2007 Mag. Christian Tanzer. All rights reserved.
;;;; Glasauergasse 32, A--1130 Wien, Austria. tanzer.co.at
;;;;++
;;;; Name
;;;;    swing-kartei--entry
;;;;
;;;; Purpose
;;;;    Functions and variables for handling kartei entry buffers
;;;;
;;;; Revision Dates
;;;;    16-Aug-1994 (CT) Creation (of comment)
;;;;    16-Aug-1994 (CT) case-fold-search added
;;;;     8-Sep-1994 (CT) case-fold parameter for
;;;;                     swing-kartei:entry:complete-name
;;;;     1-May-1999 (CT) lse-kartei-mode added to
;;;;                     swing-kartei:entry:create+goto-buffer
;;;;    23-Dec-2007 (CT) `(lse-completion:left_margin 3)` added to
;;;;                     `swing-kartei:entry:complete-name`
;;;;    ««revision-date»»···
;;;;--
(provide                    'swing-kartei--entry)

;;;;++
;;;; searching for entries
;;;;--
(defun swing-kartei:entry:head-pattern (name-pat)
  (concat "\\(^%?"                         ; \\1 entire head of entry
             "\\\\entry *{"
             "\\("                         ; \\2        name of entry
             (or name-pat ".+")
             "\\)"
             "} *\n"
          "\\)"
  )
; swing-kartei:entry:head-pattern
)

(defun swing-kartei:entry:pattern (name-pat)
  (concat (swing-kartei:entry:head-pattern name-pat)
          swing-kartei:entry:body-pattern
          swing-kartei:entry:tail-pattern
  )
; swing-kartei:entry:pattern
)

(defconst swing-kartei:entry:head-pattern
  (swing-kartei:entry:head-pattern ".+")
)

(defconst swing-kartei:entry:body-pattern
  (concat "\\("                           ; \\1 entire body
            "\\("                         ; \\2
              "^[\\% ].*\n"               ;     a single line
              "\\|^\n"                    ;     maybe empty
            "\\)+"                        ;         but at least one line
          "\\)"                           ;
  )
)

(defconst swing-kartei:entry:tail-pattern
  (concat "\\(^%?"                        ; \\1 the entire tail
            "\\\\endentry.*\n"            ;
            "\\(·+ *\n\\)"                ; \\2 filler between entries
          "\\)"
  )
)

(defconst swing-kartei:entry:pattern
  (concat swing-kartei:entry:head-pattern ; \\1, \\2
          swing-kartei:entry:body-pattern ; \\3, \\4
          swing-kartei:entry:tail-pattern ; \\5, \\6
  )
)

(defun swing-kartei:entry:find (name &optional quiet)
  (interactive "sName of entry: ")
  (if (re-search-forward (swing-kartei:entry:pattern name) nil t)
      (goto-char (match-beginning 0))
    (if (not quiet)
        (error "Could not find entry `%s'" name)
    )
    nil
  )
; swing-kartei:entry:find
)

(defun swing-kartei:entry:value (name &optional quiet)
  (interactive "sName of entry: ")
  (if (swing-kartei:entry:find name quiet)
      (buffer-substring-no-properties (match-beginning 0) (match-end 0))
  )
; swing-kartei:entry:value
)

;;;;++
;;;; entry buffer related material
;;;;--
(defvar                      swing-kartei:entry:kartei-name        nil)
(defvar                      swing-kartei:entry:kartei-dir         nil)
(defvar                      swing-kartei:entry:kartei-file-dir    nil)
(defvar                      swing-kartei:entry:initial-val        nil)
(defvar                      swing-kartei:entry:make-entry-summary nil)
(defvar                      swing-kartei:entry:commit-entry-hook  nil)
(defvar                      swing-kartei:entry:case-fold-search   nil)
(make-variable-buffer-local 'swing-kartei:entry:kartei-name)
(make-variable-buffer-local 'swing-kartei:entry:kartei-dir)
(make-variable-buffer-local 'swing-kartei:entry:kartei-file-dir)
(make-variable-buffer-local 'swing-kartei:entry:initial-val)
(make-variable-buffer-local 'swing-kartei:entry:make-entry-summary)
(make-variable-buffer-local 'swing-kartei:entry:commit-entry-hook)
(make-variable-buffer-local 'swing-kartei:entry:case-fold-search)

(defun swing-kartei:entry:complete-name (kartei-dir kartei-name case-fold)
  (let* ((lse-completion:buffer
              (swing-kartei:get-file-buffer kartei-dir kartei-name "summary")
         )
         (lse-completion:left_margin 3)
         result
         blank
        )
    (save-excursion
      (set-buffer lse-completion:buffer)
      (lse-completion:initialize_buffer kartei-name)
      (setq lse-completion:end_pos (1- (point-max)))
      (setq lse-completion:sorted  t)
      (setq tab-width 1)
      (lse-completion:widen)
      (goto-char (point-min))
    )
    (setq result (lse-complete "" nil nil t nil nil case-fold))
    (if result
    (if (setq blank  (string-match " " result))
        (setq result (substring result 0 blank))
    )
    )
    result
  )
; swing-kartei:entry:complete-name
)

(defun swing-kartei:entry:read-name (kartei-dir kartei-name)
  (if lse-emacs19-p
      (read-from-minibuffer
           (format "Name of %s-entry: " kartei-name)
           nil nil nil
           (intern (concat "swing-kartei:" kartei-name "-history"))
      )
    (read-with-history-in
         (intern (concat kartei-name "-history"))
         (format "Name of %s-entry: " kartei-name)
    )
  )
)

(defun swing-kartei:entry:create+goto-buffer
           (kartei-dir kartei-name kartei-file-dir
                       entry-name entry
                       make-entry-summary
                       &optional kartei-language
                       commit-entry-hook
                       not-case-fold-search
           )
  (lse-goto-buffer+maybe-create entry-name)
  (lse-kartei-mode);  1-May-1999
  (setq swing-kartei:entry:kartei-dir         kartei-dir)
  (setq swing-kartei:entry:kartei-name        kartei-name)
  (setq swing-kartei:entry:kartei-file-dir    kartei-file-dir)
  (setq swing-kartei:entry:initial-val        entry)
  (setq swing-kartei:entry:make-entry-summary make-entry-summary)
  (setq swing-kartei:entry:commit-entry-hook  commit-entry-hook)
  (setq swing-kartei:entry:case-fold-search   (not not-case-fold-search))
  (if (stringp entry) (insert entry))
  (if kartei-language (lse-language:use kartei-language))
  (save-excursion
    (goto-char 1)
    (insert (make-string 79 ?·) "\n")
    (goto-char (point-max))
    (insert lse_req_fill-in_head_delim
            lse_no_replacement_fill-in_marker
            "Kartei-Entry-Command-Menu"
            lse_no_replacement_fill-in_marker
            lse_req_fill-in_tail_delim
            "\n"
    )
  )
  (lse-set-buffer-nowrite (current-buffer))
  (set-buffer-modified-p nil)
  (goto-char 1)
; swing-kartei:entry:create+goto-buffer
)

(defun swing-kartei:entry:delete (entry-name in-buffer)
  ;; returns position of successor of deleted entry
  (save-excursion
    (set-buffer in-buffer)
    (let (buffer-read-only)
      (if (looking-at (swing-kartei:entry:pattern entry-name))
          t
        (goto-char 1)
        (swing-kartei:entry:find entry-name)
      )
      (delete-region (match-beginning 0) (match-end 0))
      (looking-at (swing-kartei:entry:pattern ".+" ))
    )
  )
; swing-kartei:entry:delete
)

(defun swing-kartei:entry:insert (entry successor in-buffer)
  (save-excursion
    (set-buffer in-buffer)
    (let (buffer-read-only)
      (if successor
          (if (looking-at (swing-kartei:entry:pattern successor))
                  t
                (goto-char 1)
                (if (not (swing-kartei:entry:find successor 'quiet))
                    (error
                       "Could not find entry %s. Kartei internally inconsistent"
                       successor
                    )
                )
          )
        (goto-char (point-max))
      )
      (insert new-entry)
    )
  )
; swing-kartei:entry:insert
)
