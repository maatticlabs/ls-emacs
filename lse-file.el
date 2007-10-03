;-*- unibyte: t; coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work

;;;;unix_ms_filename_correspondency lse-file:el lse_file:el
;;;; Copyright (C) 1994-2007 Mag. Christian Tanzer. All rights reserved.
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
;;;;    lse-file
;;;;
;;;; Purpose
;;;;    Functions for handling of files in emacs
;;;;
;;;; Revision Dates
;;;;    27-May-1994 (CT) Creation
;;;;    18-Jun-1994 (CT) lse-file-name-sans-extension moved to lse-session
;;;;    17-Sep-1994 (CT) Use lse-script-dir instead of literal path
;;;;    16-Oct-1994 (CT) lse-make-directory defined
;;;;    28-Oct-1996 (CT) lse-toggle-read-only defined
;;;;     3-Oct-2007 (CT) lse-visit-file-new defined
;;;;     3-Oct-2007 (CT) `lse-file:copyright-update` added and added to
;;;;                     `before-save-hook`
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-file)

(defun lse-read-file-name (prompt &optional dir def must-exist)
  (or def (setq def (lse-tpu:selection)))
  (if lse-emacs19-p
      (read-file-name prompt dir nil must-exist def)
    ;; (read-file-name-with-history-in 'file-history prompt dir def must-exist)
  )
)

(defun lse-find-file-noselect (filename &optional nowarn)
  ;; 22-Nov-1993 CT: do not return buffer already existing
  ;; Read file FILENAME into a buffer and return the buffer.
  ;; The buffer is not selected, just returned to the caller.
  (setq filename (expand-file-name filename))
  ;; Get rid of the prefixes added by the automounter.
  (if (and (string-match automount-dir-prefix filename)
           (file-exists-p (file-name-directory
                           (substring filename (1- (match-end 0)))
                          )
           )
      )
      (setq filename (substring filename (1- (match-end 0))))
  )
  (if (file-directory-p filename)
      (if find-file-run-dired
          (dired-noselect filename)
        (error "%s is a directory." filename)
      )
    (let (buf
          error
         )
      (save-excursion
        (setq buf (create-file-buffer filename))
        (set-buffer buf)
        (erase-buffer)
        (condition-case ()
            (insert-file-contents filename t)
          (file-error
           (setq error t)
           ;; Run find-file-not-found-hooks until one returns non-nil.
           (let ((hooks find-file-not-found-hooks))
             (while (and hooks (not (funcall (car hooks))))
               (setq hooks (cdr hooks))
             )
           )
          )
        )
        (setq default-directory (file-name-directory filename))
        (after-find-file error (not nowarn))
      )
      buf
    )
  )
)

(defun lse@visit@file (switch-cmd &optional file must-exist)
  (or file
      (setq file
            (lse-read-file-name
                "Visit file : "     ; prompt
                 nil                ; default-directory
                 nil                ; default-filename
                 must-exist         ; file must not exist
            )
      )
  )
  (lse-set-last-mark-all)
  (if (string-match "\\*\\|`" file)
      (let ((files (lse-file:expand-wildcard file))
            (first nil)
            (n     0)
            f
            current
           )
        (while (consp files)
          (setq f       (car files))
          (setq files   (cdr files))
          (setq current (lse-find-file-noselect f))
          (or   first   (setq first current))
          (setq n       (1+ n))
        )
        (if first
            (progn
              (funcall switch-cmd first)
              (lse-message (format "%d files matching %s read" n file))
            )
          (lse-message "No file read (nothing matched %s)" file)
        )
      )
    (if must-exist
        (if (not (file-exists-p file))
            (error "File %s does not exist" file)
        )
    )
    (funcall switch-cmd (lse-find-file-noselect file))
  )
; lse@visit@file
)

(defun lse-visit-file (&optional must-not-exist file)
  "Reads file into a new buffer."
  (interactive "P")
  (lse@visit@file 'switch-to-buffer file (not must-not-exist))
)
;;;  3-Oct-2007
(defun lse-visit-file-new (&optional file)
  "Create a new file and visit it in a new buffer."
  (interactive "P")
  (lse-visit-file t file)
; lse-visit-file-new
)

(defun lse-visit-file-other-window (&optional must-not-exist file)
  "Reads file into a new buffer and displays it in another window."
  (interactive "P")
  (let ((w (selected-window)))
    (save-excursion
      (lse@visit@file
           'switch-to-buffer-other-window file (not must-not-exist)
      )
    )
    (select-window w)
  )
)

(defun lse-visit-alternate-file (&optional file)
  "Replaces contents of current buffer by file."
  (interactive)
  (and (buffer-modified-p)
       (not (yes-or-no-p
             (format
              "Buffer %s is modified. Do you want to replace it nevertheless? "
              (buffer-name)
             )
            )
       )
       (error "Aborted")
  )
  (setq file
        (or file
            (lse-read-file-name
                "Visit alternate file : "     ; prompt
                 nil                          ; default-directory
                 nil                          ; default-filename
                 t                            ; file must exist
            )
        )
  )
  (widen)
  (erase-buffer)
  (insert-file-contents file t)
  ; (rename-buffer (lse@unique@buffer@name file))
)

(defun lse-insert-file (&optional file)
  (interactive)
  (setq file
        (or file
            (lse-read-file-name
                "Insert file : "    ; prompt
                 nil                ; default-directory
                 nil                ; default-filename
                 t                  ; file must exist
            )
        )
  )
  (insert-file file)
  (lse-tpu:unselect)
; lse-insert-file
)

(defun lse-change-output-file-current ()
  "Change output file of current buffer"
  (interactive)
  (lse-change-output-file (current-buffer))
; lse-change-output-file-current
)

(defun lse-change-output-file (buf &optional new-name silent)
  "Change output file of buffer."
  (interactive "bChange output file of buffer: ")
  (let ((old-name (buffer-file-name (get-buffer buf)))
       )
    (if old-name
        (setq old-name (file-name-nondirectory old-name))
    )
    (if (not new-name)
        (setq new-name (lse-read-file-name "File name: " nil old-name nil))
    )
    (or new-name (setq new-name ""))
    ;; here should go a completion of new-name with fields taken from old-name
    (save-excursion
      (set-buffer (get-buffer buf))
      (set-visited-file-name new-name)
      (if silent
          nil
        (message "File name of buffer %s is now %s"
                 (buffer-name) (buffer-file-name)
        )
      )
    )
  )
)

(defun lse-file:expand-wildcard (wild-card)
  (let (expander-result
        bp
       )
    (save-excursion
      (set-buffer (get-buffer-create " $swing expander filter$"))
      (erase-buffer)
      (if (call-process (concat lse-script-dir "/expand_wildcard")
                        nil              ; infile
                        (current-buffer) ; output-buffer = current buffer
                        nil              ; display
                        wild-card
          )
          (progn
            (goto-char 1)
            (while (not (eobp))
               (setq bp (point))
               (skip-chars-forward "^ \t\n")
               (if (> (point) bp)
                   (setq expander-result
                         (cons (buffer-substring bp (point)) expander-result)
                   )
               )
               (skip-chars-forward " \t\n")
            )
          )
      )
    )
    (reverse expander-result)
  )
; lse-file:expand-wildcard
)

;;; 16-Oct-1994
(defun lse-make-directory (&optional name)
  "Make directory 'name' (default: directory of filename of current buffer)"
  (interactive)
  (or (stringp name)
      (setq name (file-name-directory (buffer-file-name)))
  )
  (make-directory name t)
  (message (concat "Directory " name " created"))
; lse-make-directory
)

;;; 28-Oct-1996
(defun lse-toggle-read-only (&optional buf)
  "Change whether this buffer is visiting its file read-only.
Doesn't overrule file protection."
  (interactive)
  (or buf (setq buf (current-buffer)))
  (let ((fn (buffer-file-name (get-buffer buf)))
       )
    (if fn
        (progn
          (if (file-writable-p fn)
              (toggle-read-only)
            (lse-ring-bell)
            (error "File `%s' is not writable" fn)
          )
        )
      (error "Buffer `%s' has no output file" (buffer-name (get-buffer buf)))
    )
  )
; lse-toggle-read-only
)

;;;  3-Oct-2007
(defconst lse-file:copyright-pattern
  (concat
    "\\([Cc]opyright \\)?"                              ; \\1 Copyright
    "([Cc]) "
    "\\([0-9]\\{4\\}\\)"                                ; \\2 Start year
    "\\(-\\([0-9]\\{4\\}\\)\\)?"                        ; \\4 End   year
  )
)

;;;  3-Oct-2007
(defun lse-file:copyright-update ()
  "Update copyright in current buffer"
  (interactive)
  (save-excursion
    (save-match-data
      (lse-tpu:move-to-beginning)
      (if (re-search-forward lse-file:copyright-pattern (buffer-size) t)
          (progn
            (let* ((y   (lse-date-year))
                   (low (match-string 2))
                   (up  (or (match-string 4) y))
                   (upp (if (string-lessp up y) y up))
                   (range
                     (if (string-lessp low upp)
                         (concat low "-" upp)
                       low
                     )
                   )
                  )
              (replace-match (concat "Copyright (C) " range))
            )
          )
      )
    )
  )
; lse-file:copyright-update
)

(add-hook 'before-save-hook 'lse-file:copyright-update)
