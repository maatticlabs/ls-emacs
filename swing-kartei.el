;-*- unibyte: t; coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work
 
;;;;unix_ms_filename_correspondency swing-kartei.el swi_kart.el
;;;; Copyright (C) 1994 Mag. Christian Tanzer. All rights reserved.
;;;; Glasauergasse 32, A--1130 Wien, Austria. tanzer.co.at
;;;;++
;;;; Name
;;;;    swing-kartei
;;;;
;;;; Purpose
;;;;    Generic kartei management
;;;;
;;;; Revision Dates
;;;;     1-Aug-1994 (CT) Creation (of comment)
;;;;                     swing-kartei:log added
;;;;                     Locking mechanism added
;;;;    16-Aug-1994 (CT) not-case-fold-search parameter to swing-kartei:add
;;;;                     and friends
;;;;     8-Sep-1994 (CT) case-fold parameter for completion
;;;;    30-Mar-1995 (CT) swing-kartei:quit-entry changed (changed text
;;;;                     attributes of command-fill-in resulted in stupid
;;;;                     question to user)
;;;;-- 
(let ((kdir (getenv "SWING_KARTEI")))
  (if kdir
      (defconst swing-kartei:directory (concat kdir "/"))
    (defconst swing-kartei:directory "/swing/kartei/")
  )
)

(require        'swing-kartei--entry)
(require        'swing-kartei--summary)
(require        'swing-kartei-bib)
(require        'swing-kartei-firma)
(require        'swing-kartei-person)
(require        'swing-kartei-quot)

(defun swing-kartei:sort (kartei-dir kartei-name)
  ;; Sorts the kartei-entries alphabetically.
  ;; (warning: this normally takes a very long time!)
  (let ((src-buf (swing-kartei:get-file-buffer
                      kartei-dir kartei-name "kartei" t
                 )
        )
        (gc-cons-threshold 100000); all other values take much more time!!!
       )
    (switch-to-buffer src-buf)
    (let (buffer-read-only)
      (sort-regexp-fields nil 
                          swing-kartei:entry:pattern 
                          "\\2" 
                          (point-min) 
                          (point-max)
      )
    )
  )
; swing-kartei:sort
)

(defun swing-kartei:make-entry-summary (limit make-entry-summary)
  (let (head)
    ;; indentation + \t allow the summary file to be used as completion buffer 
    (set-buffer dst-buf)
    (setq head (point))
    (indent-to lse_completion:left_margin)
    (funcall make-entry-summary limit)
    (subst-char-in-region head (1- (point)) ?\n ?  t)
    (set-buffer src-buf)
  )
)

(defun swing-kartei:make-summary (kartei-dir kartei-name make-entry-summary)
  (save-window-excursion
    (let ((swing-kartei:n 0)
          (src-buf (swing-kartei:get-file-buffer
                        kartei-dir kartei-name "kartei" t
                   )
          )
          (field   "")
          (gc-cons-threshold 100000); all other values take much more time!!!
          dst-buf 
          p
         )
      (save-window-excursion
        (lse-goto-buffer+maybe-create 
             (concat kartei-dir kartei-name ".summary") nil t
        )
        (setq tab-width 1)
        (setq dst-buf (current-buffer))
      )
      (switch-to-buffer src-buf)
      (goto-char (point-min))
      (while (swing-kartei:entry:find ".+" 'quiet)
        (setq p (match-end 0))
        (swing-kartei:make-entry-summary p make-entry-summary)
        (goto-char p)
        (setq swing-kartei:n (1+ swing-kartei:n))
        (if (equal (% swing-kartei:n 10) 0) (message "... %d" swing-kartei:n))
      )
      (switch-to-buffer dst-buf)
      (goto-char 1)
      (lse-tpu:replace-all "\\\\," " ")
      (lse-tpu:replace-all "\\\\&" "&")
      (lse-tpu:replace-all " *\\." ".")
      (lse-tpu:replace-all "% *"   " ")
      (shell-command-on-region 1 (point-max) "sort -b -d -f" t nil)
      (message "Kartei contains %d entries" swing-kartei:n)
    )
  )
; swing-kartei:make-summary
)

(defun swing-kartei:split (kartei-dir kartei-name file-dir)
  (save-window-excursion
    (let ((swing-kartei:n 0)
          (src-buf (swing-kartei:get-file-buffer
                        kartei-dir kartei-name "kartei" t
                   )
          )
          (gc-cons-threshold 100000); all other values take much more time!!!
          end-of-entry
          entry-name
         )
      (switch-to-buffer src-buf)
      (let (buffer-read-only)
        (lse-set-buffer-nowrite src-buf)
        (lse-tpu:replace-all "^\\\\entry"    "%\\\\entry")
        (lse-tpu:replace-all "^\\\\endentry" "%\\\\endentry")
        (goto-char (point-min))
        (while (swing-kartei:entry:find ".+" 'quiet)
          (if (match-beginning 6)
              (setq end-of-entry (match-beginning 6))
            (setq end-of-entry (match-end 0))
          )
          (setq entry-name
            (buffer-substring-no-properties (match-beginning 2) (match-end 2))
          )
          (write-region (match-beginning 0) end-of-entry
                        (concat file-dir entry-name ".tex") nil 'quiet
          )
          (goto-char (match-end 0))
          (setq swing-kartei:n (1+ swing-kartei:n))
          (if (equal (% swing-kartei:n 10) 0) (message "... %d" swing-kartei:n))
        )
      )
    )
  )
; swing-kartei:split
)

(defun swing-kartei:map-entries (map-fct)
  (let (ce)
    (save-excursion
      (goto-char (point-min))
      (while (setq ce (swing-kartei:entry:value ".+"))
        (funcall map-fct ce)
      )
    )
  )
; swing-kartei:map-entries
)

(defun swing-kartei:show-entry (ce)
  (message (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
  (setq swing-kartei:n (1+ swing-kartei:n))
; swing-kartei:show-entry
)

(defun swing-kartei:field-value (limit field-name)
  (if (re-search-forward
         (concat "\\\\" field-name swing-kartei:pattern:arg-of-macro) 
         limit
         t
      )
      (buffer-substring-no-properties (match-beginning 2) (match-end 2))
  )
)

(defun swing-kartei:copy-field 
           (src-buf dst-buf limit field-name &optional trailer indent)
  (let (field)
    (set-buffer src-buf)
    (save-excursion
      (setq field (or (swing-kartei:field-value limit field-name) ""))
    )
    (set-buffer dst-buf)
    (insert     field)
    (if indent  (indent-to  indent))
    (if trailer (insert     trailer))
  )
; swing-kartei:copy-field
)

(defconst swing-kartei:pattern:arg-of-macro  
  (concat " *%? *\n? *"            ; 
          "\\(\\[[^]]*\\]\\)?"     ; \\1 optional leading  parameter (including [])
          "{\\([^}]*\\)}"          ; \\2 required          parameter (excluding {})
          "\\(\\[[^]]*\\]\\)?"     ; \\3 optional trailing parameter (including [])
  )
)

(defun swing-kartei:field+option-values (limit field-name)
  (if (re-search-forward
         (concat "\\\\" field-name swing-kartei:pattern:arg-of-macro) 
         limit
         t
      )
      (list 
         (buffer-substring-no-properties (match-beginning 2) (match-end 2))
         (if (match-beginning 1) 
             (buffer-substring-no-properties (match-beginning 1) (match-end 1))
         )
         (if (match-beginning 3) 
             (buffer-substring-no-properties (match-beginning 3) (match-end 3))
         )
      )
    nil
  )
)

(defun swing-kartei:copy-field+tail-option 
           (src-buf dst-buf limit field-name &optional trailer indent)
  (set-buffer src-buf)
  (save-excursion
    (if (re-search-forward
           (concat "\\\\" field-name swing-kartei:pattern:arg-of-macro) 
           limit
           t
        )
        (progn
          (setq field
             (buffer-substring-no-properties (match-beginning 2) (match-end 2))
          )
          (if (match-beginning 3)
              (setq field 
                    (concat field " " 
                       (buffer-substring-no-properties (match-beginning 3) (match-end 3))
                    )
              )
          )
        )
      (setq field "")
    )
  )
  (set-buffer dst-buf)
  (insert     field)
  (if trailer (insert     trailer))
  (if indent  (indent-to  indent))
; swing-kartei:copy-field+tail-option
)

(defun swing-kartei:get-file-buffer (dir name ext &optional read-only no-lock)
  (if (or no-lock (file-readable-p (concat dir "." name ".open")))
      (let* ((full-name (concat dir name "." ext))
             (rbuf      (get-file-buffer full-name))  
            )
        (if rbuf
            (let ((uptodate (verify-visited-file-modtime rbuf))
                  (modified (buffer-modified-p           rbuf))
                 )
              (if uptodate
                  t                         ; relax
                (if modified
                    (error "Buffer and file for kartei %s do not match"
                           full-name
                    )
                  (save-excursion
                    (lse-kill-buffer rbuf)
                    (setq rbuf (find-file-noselect full-name))
                  )
                )
              )
            )
          (setq rbuf (find-file-noselect full-name))
          (if (not rbuf)
              (error "Kartei %s not found" full-name)
          )
          (if read-only
              (save-excursion
                (set-buffer rbuf)
                (setq buffer-read-only t)
              )
          )
        )
        rbuf
      )
    (error "Kartei `%s' currently isn't available. Try again later." name)
  )
; swing-kartei:get-file-buffer
)

(defun swing-kartei:add
         (kartei-dir kartei-name kartei-file-dir make-entry-summary 
          &optional kartei-language commit-entry-hook not-case-fold-search
         )
  (let* ((entry-name (swing-kartei:entry:read-name kartei-dir kartei-name))
         (case-fold-search         (not not-case-fold-search))
         (default-case-fold-search case-fold-search)
         entry
        )
    (if (not entry-name)
        t                               ; relax
      (save-excursion
        (set-buffer (swing-kartei:get-file-buffer
                          kartei-dir kartei-name "summary"
                    )
        )
        (setq case-fold-search (not not-case-fold-search))
        (goto-char 1)
        (setq entry (swing-kartei:summary:entry-value entry-name 'quiet))
        (if entry
            (error "Entry %s does already exist: %s" entry-name entry)
        )
      )
      (swing-kartei:entry:create+goto-buffer
           kartei-dir kartei-name kartei-file-dir entry-name entry
           make-entry-summary kartei-language commit-entry-hook
           not-case-fold-search
      )
      (goto-char 1)
      (while (lse_search_fill-in:forward "entry-name")
        (lse-expand)
      )
      (set-buffer-modified-p nil)
      (goto-char 1)
      (lse-goto-next-fill-in)
      (recenter (/ (window-height) 2))
    )
  )
; swing-kartei:add
)

(defun swing-kartei:change
         (kartei-dir kartei-name kartei-file-dir make-entry-summary 
          &optional   kartei-language commit-entry-hook not-case-fold-search
         )
  (let* ((case-fold-search         (not not-case-fold-search))
         (default-case-fold-search case-fold-search)
         (entry-name
             (swing-kartei:entry:complete-name kartei-dir kartei-name
                                               case-fold-search
             )
         )
         entry
        )
    (if (not entry-name)
        t                               ; relax
      (save-excursion
        (set-buffer (swing-kartei:get-file-buffer
                          kartei-dir kartei-name "kartei" t
                    )
        )
        (setq case-fold-search (not not-case-fold-search))
        (goto-char 1)
        (setq entry (swing-kartei:entry:value entry-name 'quiet))
        (if (not entry)
            (error "Entry %s does not exist" entry-name)
        )
      )
      (swing-kartei:entry:create+goto-buffer
           kartei-dir kartei-name kartei-file-dir entry-name entry
           make-entry-summary kartei-language commit-entry-hook
           not-case-fold-search
      )
    )
  )
; swing-kartei:change
)

(defun swing-kartei:assert-consistency 
           (entry-name initial-value with-buffer kartei-case-fold-search)
  (save-excursion
    (let (old-entry)
      (set-buffer with-buffer)
      (setq case-fold-search kartei-case-fold-search)
      (goto-char 1)
      (setq old-entry (swing-kartei:entry:value entry-name 'quiet))
      (if (not (equal old-entry initial-value))
          (error "Value of %s has changed in kartei" entry-name)
      )
    )
  )
; swing-kartei:assert-consistency 
)

(defun swing-kartei:commit-entry ()
  "Commit kartei entry in current buffer."
  (interactive)
  (if (not swing-kartei:entry:kartei-name)
      (error "Current buffer does not contain a kartei entry")
  )
  (save-excursion
    (if (fboundp swing-kartei:entry:commit-entry-hook)
        (funcall swing-kartei:entry:commit-entry-hook)
    )
    (goto-char 1)
    (let* ((kartei-buf       (swing-kartei:get-file-buffer
                                swing-kartei:entry:kartei-dir 
                                swing-kartei:entry:kartei-name 
                                "kartei"
                                t
                             )
           )
           (summary-buf      (swing-kartei:get-file-buffer
                                swing-kartei:entry:kartei-dir 
                                swing-kartei:entry:kartei-name 
                                "summary"
                             )
           )
           (case-fold-search swing-kartei:entry:case-fold-search)
           (default-case-fold-search case-fold-search)
           (src-buf          (current-buffer))
           (entry-name       (lse-file-name-sans-extension (buffer-name)))
           (new-entry        (swing-kartei:entry:value entry-name 'quiet))
           (limit            (match-end 0))
           (inhibit-quit     t)
           successor 
           end-of-entry
          )
      (if (not new-entry) (error "Did not find entry %s" entry-name))
      (if (equal new-entry swing-kartei:entry:initial-val) 
          (error "Entry `%s' did not change" entry-name)
      )
      (swing-kartei:assert-consistency 
          entry-name swing-kartei:entry:initial-val kartei-buf
          swing-kartei:entry:case-fold-search
      )
      (setq successor (swing-kartei:commit-entry@summary
                           swing-kartei:entry:kartei-dir 
                           swing-kartei:entry:kartei-name 
                           entry-name
                           swing-kartei:entry:initial-val 
                           swing-kartei:entry:make-entry-summary limit
                           src-buf
                           swing-kartei:entry:case-fold-search
                      )
      )
      (if swing-kartei:entry:initial-val 
          (swing-kartei:entry:delete entry-name kartei-buf)
      )
      (swing-kartei:entry:insert new-entry successor kartei-buf)
      (swing-kartei:save_buffers kartei-buf summary-buf)
      (set-buffer  src-buf)
      (goto-char (point-min))
      (lse-tpu:replace-all "^\\\\entry"    "%\\\\entry")
      (lse-tpu:replace-all "^\\\\endentry" "%\\\\endentry")
      (setq new-entry (swing-kartei:entry:value entry-name 'quiet))
      (if (match-beginning 6)
          (setq end-of-entry (match-beginning 6))
        (setq end-of-entry (match-end 0))
      )
      (write-region (match-beginning 0) end-of-entry
          (concat swing-kartei:entry:kartei-file-dir entry-name ".tex") 
          nil 'quiet
      )
      (swing-kartei:log
          (if swing-kartei:entry:initial-val "changed" "added")
          entry-name
          swing-kartei:entry:kartei-dir 
          swing-kartei:entry:kartei-name 
      )
    ); let
  ); save-excursion
  (set-buffer-modified-p nil)
  (lse-kill-buffer (current-buffer))
; swing-kartei:commit-entry
)

(defun swing-kartei:commit-entry@summary
           (kartei-dir kartei-name entry-name initial-entry
            make-entry-summary limit src-buf kartei-case-fold-search
           )
  (let ((successor ""))
    (save-excursion
      (set-buffer (swing-kartei:get-file-buffer
                       kartei-dir kartei-name "summary"
                  )
      )
      (setq case-fold-search kartei-case-fold-search)
      (if (not initial-entry)
          (setq successor (swing-kartei:summary:successor entry-name))
        (setq successor 
              (swing-kartei:summary:delete entry-name (current-buffer)
                                           kartei-case-fold-search
              )
        )
      )
      (let ((dst-buf (current-buffer)))
        (swing-kartei:make-entry-summary limit make-entry-summary)
      )
    )
    successor
  )
; swing-kartei:commit-entry@summary
)

(defun swing-kartei:delete-entry ()
  "Delete kartei entry in current buffer from kartei."
  (interactive)
  (if (not swing-kartei:entry:kartei-name)
      (error "Current buffer does not contain a kartei entry")
  )
  (save-excursion
    (goto-char 1)
    (let* ((kartei-buf    (swing-kartei:get-file-buffer
                               swing-kartei:entry:kartei-dir 
                               swing-kartei:entry:kartei-name 
                               "kartei"
                               t
                          )
           )
           (summary-buf   (swing-kartei:get-file-buffer
                               swing-kartei:entry:kartei-dir 
                               swing-kartei:entry:kartei-name 
                               "summary"
                          )
           )
           (case-fold-search swing-kartei:entry:case-fold-search)
           (default-case-fold-search case-fold-search)
           (entry-name    (lse-file-name-sans-extension (buffer-name)))
           (new-entry     (swing-kartei:entry:value entry-name 'quiet))
          )
      (if (not new-entry) (error "Did not find entry %s" entry-name))
      (swing-kartei:assert-consistency 
          entry-name swing-kartei:entry:initial-val kartei-buf
          swing-kartei:entry:case-fold-search
      )
      (swing-kartei:summary:delete entry-name summary-buf
                                   swing-kartei:entry:case-fold-search
      )
      (swing-kartei:entry:delete   entry-name kartei-buf)
      (swing-kartei:save_buffers   kartei-buf summary-buf)
      (swing-kartei:log
          "deleted" 
          entry-name
          swing-kartei:entry:kartei-dir 
          swing-kartei:entry:kartei-name 
      )
    ); let
  ); save-excursion
  (set-buffer-modified-p nil)
  (lse-kill-buffer (current-buffer))
; swing-kartei:delete-entry
)

(defun swing-kartei:rename-entry ()
  "Rename kartei entry in current buffer."
  (interactive)
  (if (not swing-kartei:entry:kartei-name)
      (error "Current buffer does not contain a kartei entry")
  )
  (save-excursion
    (goto-char 1)
    (let* ((kartei-buf       (swing-kartei:get-file-buffer
                                swing-kartei:entry:kartei-dir 
                                swing-kartei:entry:kartei-name 
                                "kartei"
                                t
                             )
           )
           (summary-buf      (swing-kartei:get-file-buffer
                                swing-kartei:entry:kartei-dir 
                                swing-kartei:entry:kartei-name 
                                "summary"
                             )
           )
           (case-fold-search swing-kartei:entry:case-fold-search)
           (default-case-fold-search case-fold-search)
           (entry-name       (lse-file-name-sans-extension (buffer-name)))
           (new-entry        (swing-kartei:entry:value entry-name 'quiet))
           new-name
          )
      (if (not new-entry) (error "Did not find entry %s" entry-name))
      (if (not swing-kartei:entry:initial-val)
          t                             ; relax
        (swing-kartei:assert-consistency 
            entry-name swing-kartei:entry:initial-val kartei-buf
            swing-kartei:entry:case-fold-search
        )
        (swing-kartei:summary:delete entry-name summary-buf
                                     swing-kartei:entry:case-fold-search
        )
        (swing-kartei:entry:delete   entry-name kartei-buf)
        (setq swing-kartei:entry:initial-val nil)
        (swing-kartei:save_buffers kartei-buf summary-buf)
      )
      (setq new-name 
            (swing-kartei:entry:read-name swing-kartei:entry:kartei-dir 
                                          swing-kartei:entry:kartei-name
            )
      )
      (lse-tpu:replace-all entry-name new-name)
      (rename-buffer new-name)
      (swing-kartei:log
          "deleted" 
          entry-name
          swing-kartei:entry:kartei-dir 
          swing-kartei:entry:kartei-name 
      )
    ); let
  ); save-excursion
; swing-kartei:rename-entry
)

(defun swing-kartei:quit-entry ()
  "Quit kartei entry in current buffer from kartei."
  (interactive)
  (if (not swing-kartei:entry:kartei-name)
      (error "Current buffer does not contain a kartei entry")
  )
  (save-excursion
    (goto-char 1)
    (if (equal (swing-kartei:entry:value
                   (lse-file-name-sans-extension (buffer-name)) 'quiet
               )
               swing-kartei:entry:initial-val
        ); 30-Mar-1995 ; instead of buffer-modified-p 
        t                                 ; relax
      (if (not (y-or-n-p 
                  (format "Buffer %s modified! kill anyway? " (buffer-name))
               )
          )
          (error "Entry not quit")
      )
      (set-buffer-modified-p nil)
    )
  )
  (lse-kill-buffer (current-buffer))
; swing-kartei:quit-entry
)

(defun swing-kartei:save_buffers (kartei_buf summary_buf)
  (save-excursion
    (set-buffer  kartei-buf)
    (save-buffer nil)
    (set-buffer  summary-buf)
    (save-buffer nil)
  )
; swing-kartei:save_buffers
)

(defun swing-kartei:log (mode entry dir name)
  (save-excursion
    (set-buffer 
        (swing-kartei:get-file-buffer dir (concat "." name) mode nil t)
    )
    (goto-char (point-max))
    (insert entry "\n")
    (save-buffer nil)
  )
; swing-kartei:log
)


(defun swing-kartei:remove-tex-markup ()
  (let ((case-replace nil)); do not change case of replacements 
    (lse-tpu:replace-all "\\\\ "     " ")
    (lse-tpu:replace-all "\\\\,"     " ")
    (lse-tpu:replace-all "\\\\&"     "&")
    (lse-tpu:replace-all "% *"       " ")
    (lse-tpu:replace-all "~"         " ")
    (lse-tpu:replace-all "\\\\'n"    "n")
    (lse-tpu:replace-all "\\\\v s"   "s")
    (let ((case-fold-search t)); ignore case when searching 
      (lse-tpu:replace-all "\\\\swingphone/"       "877 66 92--0")
      (lse-tpu:replace-all "\\\\viennaphonecode/"  "")
      (lse-tpu:replace-all "\\\\austriaphonecode/" "0")
      (lse-tpu:replace-all "\\\\germanphonecode/"  "00 49")
      (lse-tpu:replace-all "\\\\swissphonecode/"   "00 41")
      (lse-tpu:replace-all "\\\\francephonecode/"  "00 33")
      (lse-tpu:replace-all "\\\\denmarkphonecode/" "00 45")
      (lse-tpu:replace-all "\\\\KPenalty/"         "")
      (lse-tpu:replace-all "\\\\MayBreak/"         "")
      (lse-tpu:replace-all "\\\\MayBreak"          "")
      (lse-tpu:replace-all "\\\\smaller"           "")
      (lse-tpu:replace-all "\\\\abbr"              "")
      (lse-tpu:replace-all "\\\\sf"                "")
      (lse-tpu:replace-all "\\\\sidiom"            "")
      (lse-tpu:replace-all "\\\\idiom"             "")
      (lse-tpu:replace-all "\\\\Hr/"               "Herr")
      (lse-tpu:replace-all "\\\\Fr/"               "Frau")
      (lse-tpu:replace-all "\\\\Di/"               "Dipl.-Ing.")
      (lse-tpu:replace-all "\\\\Dinf/"             "Dipl.-Inf.")
      (lse-tpu:replace-all "\\\\Ing/"              "Ing.")
      (lse-tpu:replace-all "\\\\Dr/"               "Dr.")
      (lse-tpu:replace-all "\\\\Mag/"              "Mag.")
      (lse-tpu:replace-all "\\\\Doz/"              "Doz.")
      (lse-tpu:replace-all "\\\\Dkfm/"             "Dkfm.")
      (lse-tpu:replace-all "\\\\Prof/"             "Prof.")
      (lse-tpu:replace-all "\\\\GmbH/"             "GmbH")
      (lse-tpu:replace-all "\\\\GmbHCo/"           "GmbH & Co.")
      (lse-tpu:replace-all "\\\\GmbHCoKG/"         "GmbH & Co.KG")
      (lse-tpu:replace-all "\\\\CoKG/"             "Co.KG")
    )
  )
; swing-kartei:remove-tex-markup
)

