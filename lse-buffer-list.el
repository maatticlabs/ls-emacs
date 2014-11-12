;-*- coding: utf-8 -*-

;;;; Copyright (C) 1994-2014 Mag. Christian Tanzer. All rights reserved.
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
;;;;    lse-buffer-list
;;;;
;;;; Purpose
;;;;    Functions for displaying a list of all buffers
;;;;
;;;; Revision Dates
;;;;    29-May-1994 (CT) Creation (of comment)
;;;;    30-May-1994 (CT) lse-buffer-list::name-terminator added (to handle
;;;;                     buffer names with blanks correctly)
;;;;    23-Jan-1995 (CT) Key "q" added
;;;;    19-Feb-1995 (CT) lse-buffer-list::show: identify read-only buffers
;;;;                     Display version if file is version-controlled
;;;;    20-Feb-1995 (CT) Update number of fields (from 5 to 6) and
;;;;                     lse-buffer-list::toggle-field-cmds
;;;;    20-Feb-1995 (CT) lse-buffer-list-toggle-vc added
;;;;    19-Mar-1995 (CT) lse-buffer-list::overlay added
;;;;    14-Jun-1996 (CT) Features renamed
;;;;    14-Jun-1996 (CT) Optional parameter added to `lse-show-buffers'
;;;;                     trying (unsuccessfully) to accomodate change of
;;;;                     `list-buffers' in Emacs 19.30
;;;;     3-Oct-1996 (CT) Use post-command-hook instead of
;;;;                     `lse-buffer-list::key-handler'
;;;;     3-Oct-1996 (CT) Keydef's for Emacs 18.x removed (no
;;;;                     post-command-hook there)
;;;;    16-Oct-1996 (CT) Set mouse-face
;;;;    16-Oct-1996 (CT) lse-buffer-list::name-length-max added
;;;;    17-Dec-1997 (CT) `lse-buffer-list-mouse-goto-buffer' added
;;;;    10-Jan-1998 (CT) Moved most Control-Keys to Alt-Keys
;;;;    10-Nov-2010 (CT) Use `mapc` instead of `mapcar` where appropriate
;;;;    27-Feb-2012 (CT) Add `eval-when-compile` for `fset` (compile warning)
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-buffer-list)

(defvar   lse-buffer-list::buffer        nil)
(defvar   lse-buffer-list::cb            nil)
(defvar   lse-buffer-list::buffer-keymap nil)
(defconst lse-buffer-list::toggle-field-cmds
          (vector
               'lse-rename-buffer                  ; field: buffer-name
               'ignore                             ; field: number of lines
               'lse-write-buffer                   ; field: modified
               'lse-buffer-list-toggle-write       ; field: write
               'lse-buffer-list-toggle-save        ; field: autosave
               'lse-buffer-list-toggle-vc          ; field: version control
               'lse-change-output-file             ; field: file name
          )
)
(defconst lse-buffer-list::buffer-left-margin   6)
(defvar   lse-buffer-list::buffer-top-line      3)
(defvar   lse-buffer-list::name-length         15)
(defvar   lse-buffer-list::name-length-max     15)
(defconst lse-buffer-list::name-terminator   "\t")

(defvar lse-buffer-list::overlay nil); 14-Dec-1997

(defun lse-buffer-list::define-keys ()
  (let ((lmap (current-local-map)))
    (define-key lmap [left]           'lse-tpu:goto-prev-bs-word-head)
    (define-key lmap [right]          'lse-tpu:goto-next-bs-word-head)
    (define-key lmap [kp-1]           'lse-tpu:goto-next-bs-word-head)
    (define-key lmap [find]           'lse-buffer-list-goto-buffer)
    (define-key lmap [select]         'lse-buffer-list-goto-buffer)
    (define-key lmap [mouse-2]        'lse-buffer-list-mouse-goto-buffer)

    (define-key lmap [?\C-i]          'lse-tpu:goto-next-bs-word-head)
    (define-key lmap [tab]            'lse-tpu:goto-next-bs-word-head)
    (define-key lmap [?\A-i]          'lse-tpu:goto-next-bs-word-head)
    (define-key lmap [?\C-m]          'lse-buffer-list-goto-buffer)
    (define-key lmap [?\A-m]          'lse-buffer-list-goto-buffer)
    (define-key lmap [return]         'lse-buffer-list-goto-buffer); 5-Jan-1998
    (define-key lmap [?\C-z]          'lse-goto-last-mark-global)
    (define-key lmap [?\A-z]          'lse-goto-last-mark-global)

    (define-key lmap ">"              'lse-tpu:pan-right)
    (define-key lmap "<"              'lse-tpu:pan-left)

    (lse-define-alpha-key lmap [] "e" 'lse-buffer-list-delete-buffer)
    (lse-define-alpha-key lmap [] "f" 'lse-buffer-list-goto-buffer)
    (lse-define-alpha-key lmap [] "q" 'lse-goto-last-mark-global); 23-Jan-1995
    (lse-define-alpha-key lmap [] "s" 'lse-buffer-list-sort)
    (lse-define-alpha-key lmap [] "t" 'lse-buffer-list-toggle-field)
    (lse-define-alpha-key lmap [] "w" 'lse-buffer-list-write-buffer)
  )
; lse-buffer-list::define-keys
)

(defun lse-buffer-list::goto-buffer ()
  (if (not (and (bufferp     lse-buffer-list::buffer)
                (buffer-name lse-buffer-list::buffer)
           )
      )
      (progn
        (setq lse-buffer-list::buffer
              (get-buffer-create " $Lse-Buffer-List$")
        )
        (lse-goto-buffer lse-buffer-list::buffer nil nil t)
        (setq            lse-buffer-list::buffer-keymap (make-sparse-keymap))
        (use-local-map   lse-buffer-list::buffer-keymap)
        (lse-buffer-list::define-keys)
        (setq tab-width 1)
        ;; 19-Mar-1995
        (setq lse-buffer-list::overlay (make-overlay 1 1))
        (overlay-put lse-buffer-list::overlay 'face 'lse-face:completion)
      )
    (lse-goto-buffer lse-buffer-list::buffer nil nil t)
  )
; lse-buffer-list::goto-buffer
)

(defun lse-buffer-list::selected-buffer ()
  (let (result)
    (save-excursion
      (lse-buffer-list-goto-line-begin)
      (let ((b (point)))
        (skip-chars-forward (concat "^" lse-buffer-list::name-terminator))
        (setq result (buffer-substring-no-properties b (point)))
      )
    )
    result
  )
; lse-buffer-list::selected-buffer
)

(defun lse-buffer-list::selected-field ()
  (let ((cp (point))
        (n  -1)
       )
    (save-excursion
      (lse-buffer-list-goto-line-begin)
      (save-restriction
        (narrow-to-region (point) (save-excursion (end-of-line 1)(1+ (point))))
        (while (not (> (point) cp))
          (lse-tpu:goto-next-bs-word-tail 1)
          (lse-tpu:goto-next-bs-word-head 1)
          (setq n (1+ n))
        )
      )
    )
    (max 0 (min 6 n))
  )
; lse-buffer-list::selected-field
)

(defun lse-buffer-list-delete-buffer ()
  (interactive)
  (save-excursion (lse-kill-buffer (lse-buffer-list::selected-buffer) t))
  (beginning-of-line)
  (lse-tpu:delete-next-line 1)
  (set-buffer-modified-p nil)
; lse-buffer-list-delete-buffer
)

;;; 17-Dec-1997
(defun lse-buffer-list-mouse-goto-buffer ()
  (interactive)
  (call-interactively 'mouse-set-point)
  (lse-buffer-list-goto-buffer)
; lse-buffer-list-mouse-goto-buffer
)
(defun lse-buffer-list-goto-buffer ()
  (interactive)
  (save-current-buffer
     (set-buffer lse-buffer-list::cb)
     (lse-set-last-mark-all)
  )
  (lse-goto-buffer (lse-buffer-list::selected-buffer) nil nil t)
; lse-buffer-list-goto-buffer
)

(defun lse-buffer-list-toggle-field ()
  (interactive)
  (let* ((field (lse-buffer-list::selected-field))
         (cmd   (aref lse-buffer-list::toggle-field-cmds field))
         (buf   (lse-buffer-list::selected-buffer))
         result
        )
    (if (commandp cmd)
        (progn
          (setq result (funcall cmd buf))
          (and result
               (if (stringp result)
                   (setq buf result)
                 (if (bufferp result)
                     (setq buf (buffer-name buf))
                 )
               )
          )
          (lse-buffer-list-update buf)
        )
    )
  )
; lse-buffer-list-toggle-field
)

(defun lse-buffer-list-toggle-write (&optional buf)
  (interactive)
  (or buf (setq buf (lse-buffer-list::selected-buffer)))
  (if (buffer-file-name (get-buffer buf))
      (lse-set-buffer-nowrite buf)
    (lse-set-buffer-write buf)
  )
  (lse-buffer-list-update buf)
; lse-buffer-list-toggle-write
)

(defun lse-buffer-list-toggle-save (&optional buf)
  (interactive)
  (or buf (setq buf (lse-buffer-list::selected-buffer)))
  (save-current-buffer
    (set-buffer buf)
    (auto-save-mode nil)
  )
  (lse-buffer-list-update buf)
; lse-buffer-list-toggle-save
)

(defun lse-buffer-list-toggle-vc (&optional buf)
  (interactive)
  (if t
      t
    (or buf (setq buf (lse-buffer-list::selected-buffer)))
    (save-window-excursion
      (set-buffer buf)
      (vc-toggle-read-only)
    )
    (lse-buffer-list-update buf)
  )
; lse-buffer-list-toggle-vc
)

(defun lse-buffer-list-write-buffer ()
  (interactive)
  (let ((buf (lse-buffer-list::selected-buffer)))
    (lse-write-buffer buf)
    (lse-buffer-list-update buf)
  )
; lse-buffer-list-write-buffer
)


(defun lse-buffer-list-update (buf)
  (let ((cp (point)))
    (beginning-of-line)
    (lse-tpu:delete-next-line 1)
    (lse-buffer-list::show (get-buffer buf))
    (forward-line -1)
    (set-buffer-modified-p nil)
    (goto-char cp)
  )
)

(defun lse-reverse-video-current-line ()
  (save-excursion
    (beginning-of-line)
    (setq overlay-arrow-string   "###>")
    (setq overlay-arrow-position (point-marker))
    ;; 19-Mar-1995
    (move-overlay lse-buffer-list::overlay
                  (point-marker) (lse-tpu:line-tail-pos)
                  (current-buffer)
    )
  )
; lse-reverse-video-current-line
)

(defun lse-buffer-list-goto-begin ()
  (interactive)
  (goto-char (point-min))
  (forward-line (1- lse-buffer-list::buffer-top-line))
; lse-buffer-list-goto-begin
)

(defun lse-buffer-list-goto-end ()
  (interactive)
  (let ((n (- (1- (lse-lines-in-buffer)) (lse-line-number))))
    (lse-tpu:next-line n)
  )
; lse-buffer-list-goto-end
)

(defun lse-buffer-list-sort-lines (); not used(sort-regexp-fields is better)
  (interactive)
  (sort-lines nil
    (save-excursion (lse-buffer-list-goto-begin) (beginning-of-line) (point))
    (save-excursion (lse-buffer-list-goto-end)   (end-of-line)       (point))
  )
  (set-buffer-modified-p nil)
)

(defun lse-buffer-list-sort ()
  (interactive)
  (sort-regexp-fields nil
    "^......\\([^\\t]*\\).+$" "\\1"
    (save-excursion (lse-buffer-list-goto-begin) (beginning-of-line) (point))
    (save-excursion (lse-buffer-list-goto-end)   (end-of-line)       (point))
  )
  (set-buffer-modified-p nil)
)

(defun lse-buffer-list-goto-line-begin ()
  (interactive)
  (beginning-of-line)
  (lse-tpu:forward-char lse-buffer-list::buffer-left-margin)
; lse-buffer-list-goto-line-begin
)

(defun lse-buffer-list::snap-cursor ()
  (if (eobp)
      (lse-buffer-list-goto-end)
  )
  (if (< (lse-line-number) lse-buffer-list::buffer-top-line)
      (lse-buffer-list-goto-begin)
  )
  (if (< (current-column) lse-buffer-list::buffer-left-margin)
      (lse-buffer-list-goto-line-begin)
  )
; lse-buffer-list::snap-cursor
)

(defun lse-buffer-list::key-handler ()
  (let (key
        binding
        cp
       )
    (while t
      (unwind-protect
          (progn
            (lse-buffer-list::snap-cursor)
            (lse-reverse-video-current-line)
            (setq key (read-key-sequence nil))
            (setq binding (lookup-key (current-local-map) key))
            (if (commandp binding)
                (command-execute binding)
              (setq binding (lookup-key (current-global-map) key))
              (if (commandp binding)
                  (command-execute binding)
                (error "Key %s is undefined" (lse-key-name key))
              )
            )
            (if (not (eq (current-buffer) lse-buffer-list::buffer))
                (throw 'lse-buffer-list::exit-signal t)
            )
            (if (buffer-modified-p lse-buffer-list::buffer)
                (progn
                  (setq cp (point))
                  (lse-buffer-list::show-function)
                  (if (< cp (point-max)) (goto-char cp))
                )
            )
          )
        nil
      )
    )
  )
; lse-buffer-list::key-handler
)

(defun lse-buffer-list::format-buffer
           (leader buf-nam n-lines mod write jou vc-mode file)
  (let ((opoint (point))); 11-Oct-1996
    (insert (or leader "     "))
            (indent-to lse-buffer-list::buffer-left-margin)
    (insert buf-nam lse-buffer-list::name-terminator)
            (indent-to (+ lse-buffer-list::name-length-max
                          lse-buffer-list::buffer-left-margin
                          2
                       )
            )
            ;; (indent-to (/ (* (screen-width) 3) 8)); w * 3 / 8
    (add-text-properties opoint (point) '(mouse-face lse-face:completion-m))
    (insert (format "%5s "   n-lines))
    (insert (format "%3s "   mod))
    (insert (format "%5s "   write))
    (insert (format "%3s "   jou))
    (insert (format "%-9s "  vc-mode))
    (if (string= file "") (setq file "no file"))
    (insert file)
    (insert "\n")
  )
; lse-buffer-list::format-buffer
)

(defun lse-buffer-list::show (b)
  (lse-buffer-list::format-buffer
       (if (eq b lse-buffer-list::cb)
           "Curr*"
         (if (eq b (lse-buffer:main)) "Main=" nil)
       )
       (buffer-name b)
       (format "%d" (lse-lines-in-buffer (get-buffer b)))
       (cond ((save-current-buffer (set-buffer b) buffer-read-only)
              "r/o"
             )
             (t
              (if (buffer-modified-p (get-buffer b)) "YES"  "no" )
             )
       )
       (if (buffer-file-name  (get-buffer b)) "YES " "no ")
       (if (save-current-buffer (set-buffer b) buffer-auto-save-file-name)
           "YES"
         "no"
       )
       (save-current-buffer
         (set-buffer b)
         (if (stringp vc-mode) vc-mode "   ---")
       )
       (or (buffer-file-name (get-buffer b))
           lse-buffer:file-name
           "no file"
       )
  )
; lse-buffer-list::show
)

(defun lse-buffer-list::initialize ()
  (lse-buffer-list::goto-buffer)
  (erase-buffer)

  (lse-buffer-list::format-buffer nil "Buffer-Name" "lines" "Mod" "Write" "Jou" " Version" "File")
  (lse-buffer-list::format-buffer nil
   (substring
    "************************************************************************"
    1 (min lse-buffer-list::name-length-max 60)
   )
                                        "=====" "***" "=====" "***" " ========"
   "**************************************************************************"
  )
  (setq lse-buffer-list::buffer-top-line (lse-line-number))
; lse-buffer-list::initialize
)

(defun lse-buffer-list::show-all ()
  (let ((n 0))
    (mapc
      (function
        (lambda (b)
          (setq n (1+ n))
          (setq lse-buffer-list::name-length-max
            (max lse-buffer-list::name-length-max (length (buffer-name b)))
          )
        )
      )
      (buffer-list)
    )
    (lse-buffer-list::initialize)
    (mapc
      (function (lambda (b) (if b (lse-buffer-list::show b))))
      (buffer-list)
    )
    (newline)
    (narrow-to-region 1 (1- (point)))
    (message "Number of buffers: %d" n)
    (set-buffer-modified-p nil)
  )
; lse-buffer-list::show-all
)

(defun lse-buffer-list::show-user ()
  (let* ((main (lse-buffer:main))
         (b    main)
         (n    0)
         done
       )
    (while (not done)
      (setq n (1+ n))
      (setq b (lse-buffer:prev b))
      (if b
          (setq lse-buffer-list::name-length-max
            (max lse-buffer-list::name-length-max (length (buffer-name b)))
          )
      )
      (setq done (or (not b) (eq b main)))
    )
    (setq done nil)
    (setq b main)
    (lse-buffer-list::initialize)
    (while (not done)
      (setq b (lse-buffer:prev b))
      (if b   (lse-buffer-list::show b))
      (setq done (or (not b) (eq b main)))
    )
    (newline)
    (narrow-to-region 1 (1- (point)))
    (message "Number of buffers: %d" n)
    (set-buffer-modified-p nil)
  )
; lse-buffer-list::show-user
)

(if (fboundp 'make-local-hook);  4-Oct-1996
    ;;  3-Oct-1996
    (defun lse-buffer-list::driver ()
      (lse-set-last-mark-global)
      (setq lse-buffer-list::cb (current-buffer))
      (setq lse-buffer-list::name-length-max lse-buffer-list::name-length)
      (lse-buffer-list::show-function)
      (lse-buffer-list-goto-begin)
      (make-local-hook 'post-command-hook)
      (add-hook 'post-command-hook 'lse-buffer-list:post-command-hook nil t)
    ; lse-buffer-list::driver
    )
  (defun lse-buffer-list::driver ()
    (lse-set-last-mark-global)
    (setq lse-buffer-list::name-length-max lse-buffer-list::name-length)
    (let ((lse-buffer-list::cb (current-buffer))
         )
      (unwind-protect
          (progn
            (catch 'lse-buffer-list::exit-signal
                (progn
                  (lse-buffer-list::show-function)
                  (lse-buffer-list-goto-begin)
                  (lse-buffer-list::key-handler)
                )
            )
          )
        (progn
          (setq overlay-arrow-position nil)
          (if (eq (current-buffer) lse-buffer-list::buffer)
              (lse-goto-buffer lse-buffer-list::cb nil nil t)
          )
          (message "Leaving Buffer-List-Mode")
        )
      )
    )
  ; lse-buffer-list::driver
  )
)

(eval-when-compile
  (fset 'lse-buffer-list::show-function 'lse-buffer-list::show-user)
)

(defun lse-show-buffers (&optional files-only)
  (interactive "P")
  (fset 'lse-buffer-list::show-function 'lse-buffer-list::show-user)
  (lse-buffer-list::driver)
  (fmakunbound 'lse-buffer-list::show-function)
; lse-show-buffers
)

(defun lse-show-all-buffers ()
  (interactive)
  (fset 'lse-buffer-list::show-function 'lse-buffer-list::show-all)
  (lse-buffer-list::driver)
  (fmakunbound 'lse-buffer-list::show-function)
; lse-show-all-buffers
)

;;;  3-Oct-1996
(defun lse-buffer-list:post-command-hook ()
  "Snap cursor to buffer list and highlight current line"
  (if (not (eq (current-buffer) lse-buffer-list::buffer))
      (progn
        (setq overlay-arrow-position nil)
        (message "Leaving Buffer-List-Mode")
      )
    (lse-buffer-list::snap-cursor)
    (lse-reverse-video-current-line)
    (if (buffer-modified-p lse-buffer-list::buffer)
        (let (cp); 14-Dec-1997
          (setq lse-buffer-list::name-length-max lse-buffer-list::name-length)
          (setq cp (point))
          (lse-buffer-list::show-function)
          (if (< cp (point-max)) (goto-char cp))
        )
    )
  )
; lse-buffer-list:post-command-hook
)
