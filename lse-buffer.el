;-*-unibyte: t;-*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work

;;;;unix_ms_filename_correspondency lse-buffer:el lse_buff:el
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
;;;;    lse-buffer
;;;;
;;;; Purpose
;;;;    Functions for file and buffer handling
;;;;
;;;; Revision Dates
;;;;    27-May-1994 (CT) Creation (of comment)
;;;;    27-May-1994 (CT) File handling functions factored to swing-file
;;;;    29-May-1994 (CT) Mechanism for buffer chaining improved
;;;;    30-May-1994 (CT) lse-window:temp-hidden and friends added
;;;;    18-Jun-1994 (CT) lse-window:temp-hidden and friends moved to
;;;;                     lse-window
;;;;    19-Jun-1994 (CT) Renamed to lse-buffer (previously swing-buffer)
;;;;    28-Jan-1995 (CT) lse-buffer:rebuild-chain made interactive
;;;;                     lse-buffer:initialize: don't overwrite existing
;;;;                     buffer chain
;;;;     2-Oct-1996 (CT) Hack around different behavior of `find-file-hook'
;;;;                     starting with Emacs version 19.30
;;;;    10-Oct-1996 (CT) lse-buffer:new_n added to accomodate multiple buffer
;;;;                     creations by a single command (extension of change
;;;;                     of 2-Oct-1996)
;;;;    10-Oct-1996 (CT) Added optional parameter `silent' to
;;;;                     `lse-buffer:rebuild-chain'
;;;;     5-Apr-1998 (CT) `lse-save-some-buffers' added
;;;;     5-May-1998 (CT) `lse-goto-last-mark-window' added to `lse-kill-buffer'
;;;;     5-Aug-2002 (CT) Two `t` arguments added to call of `revert-buffer`
;;;;     2-Apr-2003 (CT) `lse-buffer:unique_anchored_name` added and used
;;;;     3-Apr-2003 (CT) `lse-buffer:base-name` added
;;;;    10-Apr-2003 (CT) `lse-buffer:unique_anchored_name` fixed
;;;;    18-May-2003 (CT) `lse-save-some-buffers` and `lse-write-buffer`
;;;;                     changed to let `inhibit-point-motion-hooks` and
;;;;                     `inhibit-read-only`
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-buffer)

(defvar buf nil) ; for some reason debugger does not work without this???

(defun lse-buffer:alist ()
  (mapcar (function (lambda (x)
                      (cons (buffer-name x) x)
                    )
          )
          (buffer-list)
  )
)

(defvar buffer-history nil)

(defun lse-buffer:read-lse-completion (&optional def)
  (lse-complete (if (stringp def) def "") (lse-buffer:alist) nil nil t)
)

(defun lse-buffer:read-name (prompt &optional def must-exist)
  (let (result)
    (setq result
          (if lse-emacs19-p
              (completing-read
                   prompt (lse-buffer:alist) nil must-exist
                   (if (stringp def)
                       def
                     (if (bufferp def) (buffer-name def) "")
                   )
                   'buffer-history
              )
            ; (read-buffer-with-history-in 'buffer-history prompt def must-exist)
          )
    )
;    (if result (get-buffer result))
    result
  )
)

;;;  2-Apr-2003
(defvar lse-buffer:relative-directory-anchors nil
  "List of directories used to anchor buffernames."
)

;;;  2-Apr-2003
(defun lse-buffer:unique_anchored_name (&optional buffer)
  (let* ((buffer  (or buffer              (current-buffer)))
         (fname   (file-truename          (buffer-file-name buffer)))
         (bname   (file-name-nondirectory fname))
         (result  (buffer-name            buffer))
         (anchors lse-buffer:relative-directory-anchors)
         a
         rn
        )
    (while (consp anchors)
      (setq a       (car anchors))
      (setq anchors (cdr anchors))
      (setq rn      (file-relative-name fname a))
      (if (or (string= ".." (substring rn 0 2)) (string= rn bname))
          t
        (while (string-match "/" rn)
          ;;  3-Apr-2003
          ;; `switch-to-buffer` gets confused by buffer-names containing `/`
          (setq rn (replace-match ":" t t rn))
        )
        (setq result (generate-new-buffer-name rn))
        (setq anchors nil)
      )
    )
    result
  )
; lse-buffer:unique_anchored_name
)

;;;  2-Apr-2003
(defun lse-buffer:rename-unique-anchored-name ()
  "Rename current buffer to uniquely anchored name"
  (interactive "")
  (let ((old-name (buffer-name))
        (new-name (lse-buffer:unique_anchored_name))
       )
    (if (string= old-name new-name)
        t
      (rename-buffer new-name)
    )
  )
; lse-buffer:rename-unique-anchored-name
)

;;;;++
;;; Buffer chaining
;;;;--
(defvar lse-buffer:main       nil) ; Main buffer (containing file read first)
(defvar lse-buffer:new        nil) ; Hack for lse-buffer:initialize-hack-19.30+
(defvar lse-buffer:new_n      0)   ; Hack for lse-buffer:initialize-hack-19.30+

(defvar lse-buffer:file-name  nil) ; Original file name of buffer
(defvar lse-buffer:base-name  nil) ; Base name of buffer
(defvar lse-buffer:next       nil) ; Pointer to next buffer
(defvar lse-buffer:prev       nil) ; Pointer to prev buffer
(defvar lse-buffer:n          nil) ; Number of buffer
(make-variable-buffer-local 'lse-buffer:file-name)
(make-variable-buffer-local 'lse-buffer:base-name);  3-Apr-2003
(make-variable-buffer-local 'lse-buffer:next)
(make-variable-buffer-local 'lse-buffer:prev)
(make-variable-buffer-local 'lse-buffer:n)
(put 'lse-buffer:file-name  'permanent-local t)
(put 'lse-buffer:base-name  'permanent-local t);  3-Apr-2003
(put 'lse-buffer:next       'permanent-local t)
(put 'lse-buffer:prev       'permanent-local t)
(put 'lse-buffer:n          'permanent-local t)

(defun lse-buffer:next (&optional buf)
  (or buf (setq buf (current-buffer)))
  (let (result)
    (save-excursion
      (set-buffer buf)
      (setq result lse-buffer:next)
    )
    (if (buffer-name result)
        result
    )
  )
)

(defun lse-buffer:prev (&optional buf)
  (or buf (setq buf (current-buffer)))
  (let (result)
    (save-excursion
      (set-buffer buf)
      (setq result lse-buffer:prev)
    )
    (if (buffer-name result)
        result
    )
  )
)

(defun lse-buffer:main ()
  (if (and (bufferp lse-buffer:main) (buffer-name lse-buffer:main))
      lse-buffer:main
    (setq lse-buffer:main (lse-buffer:rebuild-chain))
  )
; lse-buffer:main
)

(defun lse-buffer:is-lse-buffer (b)
  (if (buffer-name b) (save-excursion (set-buffer b) (if lse-buffer:n b)))
; lse-buffer:is-lse-buffer
)

;;;  3-Apr-2003
(defun lse-buffer:base-name (b)
  (if (buffer-name b)
    (save-excursion
      (set-buffer b)
      (or
        lse-buffer:base-name
        (lse-file-name-sans-extension (buffer-name (current-buffer)))
      )
    )
  )
; lse-buffer:base-name
)

(defun lse-buffer:< (a b)
  (cond ((and a b)
         (< (save-excursion (set-buffer a) lse-buffer:n)
            (save-excursion (set-buffer b) lse-buffer:n)
         )
        )
        (a)
        (b nil)
  )
; lse-buffer:<
)

(defun lse-buffer:rebuild-chain (&optional silent)
  (interactive); 28-Jan-1995
  (let* ((blist (sort (mapcar 'lse-buffer:is-lse-buffer (buffer-list))
                      'lse-buffer:<
                )
         )
         (start (car blist))
         (prev  start)
         next
       )
    (if start
        (save-excursion
          (or silent (message "Rebuilding buffer chain..."))
          (set-buffer start)
          (setq lse-buffer:next start)
          (setq lse-buffer:prev start)
          (setq blist (cdr blist))
          (setq next  (car blist))
          (while (and (consp blist) next)
            (set-buffer next)
            (setq lse-buffer:prev prev)
            (set-buffer           prev)
            (setq lse-buffer:next next)
            (setq prev            next)
            (setq blist (cdr blist))
            (setq next  (car blist))
          ); while
          (set-buffer start)
          (setq lse-buffer:prev prev)
          (or silent (message "Rebuilding buffer chain: done"))
        ); save-excursion
      (setq start nil)
    ); if
    start
  ); let
; lse-buffer:rebuild-chain
)

(defun lse-buffer:initialize-main ()
  (setq lse-buffer:file-name  buffer-file-name)
  (setq lse-buffer:n          1);  2-Oct-1996 ; s/0/1/
  (setq lse-buffer:main       (current-buffer))
  (setq lse-buffer:next       (current-buffer))
  (setq lse-buffer:prev       (current-buffer))
  (remove-hook 'find-file-hooks 'lse-buffer:initialize-main)
  (add-hook    'find-file-hooks 'lse-buffer:initialize)
)

(defun lse-buffer:initialize ()
  ;; store original file name
  (setq lse-buffer:file-name buffer-file-name)
  (setq lse-buffer:base-name (lse-buffer:base-name (current-buffer)))
  (lse-buffer:rename-unique-anchored-name)
  ;; create buffer-local mark-stack
  (or lse-buffer-mark@stack
      (setq lse-buffer-mark@stack (lse-new-mark-stack))
  )
  ;; put the buffer into buffer-chain
  ;; the code of lse-buffer:initialize-hack-19.30+ was in here originally
  ;; a bug starting with Emacs 19.30 forced me to put it into another
  ;; function which is called via the post-command-hook
  (setq lse-buffer:new   (current-buffer))
  (setq lse-buffer:n     0);  7-Oct-1996
  (setq lse-buffer:new_n (1+ lse-buffer:new_n)); 10-Oct-1996
  (add-hook 'post-command-hook 'lse-buffer:initialize-hack-19.30+)
)

;;;  2-Oct-1996
(defun lse-buffer:initialize-hack-19.30+ ()
  (remove-hook 'post-command-hook 'lse-buffer:initialize-hack-19.30+)
  (if (= lse-buffer:new_n 1); 10-Oct-1996
      (let* ((self  lse-buffer:new)
             (main  (lse-buffer:main))
             prev
             n
           )
        (setq lse-buffer:new_n 0)
        (if (and main self)
            (save-excursion
              (set-buffer self)
              (or lse-buffer:next; 28-Jan-1995
                  (progn
                    (setq lse-buffer:next main)
                    (save-excursion
                      (set-buffer           main)
                      (setq prev lse-buffer:prev)
                      (setq lse-buffer:prev self)
                      (set-buffer           prev)
                      (setq lse-buffer:next self)
                      (setq n               lse-buffer:n)
                    )
                    (setq lse-buffer:prev prev)
                    (setq lse-buffer:n    (1+ n))
                  )
              )
            )
          (error "Serious inconsistency in buffer chain")
        )
      )
    (setq lse-buffer:new_n 0)
    (lse-buffer:rebuild-chain t); 10-Oct-1996
  )
  (setq lse-buffer:new nil)
; lse-buffer:initialize-hack-19.30+
)

(add-hook 'find-file-hooks 'lse-buffer:initialize-main)

(defun lse-buffer:remove-from-chain ()
  ;; removes current buffer from buffer chain
  (let ((buf  (current-buffer))
        (next (lse-buffer:next buf))
        (prev (lse-buffer:prev buf))
       )
    (cond ((and next prev)
           (save-excursion
             (set-buffer                 next)
             (setq       lse-buffer:prev prev)
             (set-buffer                 prev)
             (setq       lse-buffer:next next)
           )
          )
          ((or next prev) ; buffer chain is inconsistent
           (setq lse-buffer:n nil); current buffer must not remain in chain
           (setq next (lse-buffer:rebuild-chain))
          )
    )
    (if (eq buf lse-buffer:main)
        (setq lse-buffer:main next)
    )
  )
)

(defun lse-revert-buffer ()
  (interactive "*")
  (let ((next (lse-buffer:next (current-buffer)))
        (prev (lse-buffer:prev (current-buffer)))
        self
       )
    (revert-buffer t t t)
    (if (and (buffer-name next) (buffer-name prev))
        t
      (setq next (current-buffer))
      (setq prev (current-buffer))
    )
    (setq lse-buffer:next next)
    (setq lse-buffer:prev prev)
    (save-excursion
      (setq       self            (current-buffer))
      (set-buffer                 next)
      (setq       lse-buffer:prev self)
      (set-buffer                 prev)
      (setq       lse-buffer:next self)
    )
  )
; lse-revert-buffer
)

;;;;++
;;; Commands for buffer manipulation
;;;;--
(defun lse-kill-buffer (&optional buf dont-move)
  "Delete buffer."
  (interactive)
  (setq buf
        (get-buffer (or buf (lse-buffer:read-name "Delete buffer: "
                                                    (buffer-name)
                                                    t
                             )
                    )
        )
  )
  (if (not dont-move) (lse-goto-last-mark-window));  5-May-1998
  (save-excursion
    (if lse-emacs19-p
        t;  kill-buffer-hook does the work automatically
      (set-buffer buf)
      (if lse-buffer:n
          (lse-buffer:remove-from-chain)
      )
    )
    (kill-buffer buf)
  )
; lse-kill-buffer
)

(add-hook 'kill-buffer-hook 'lse-buffer:remove-from-chain)

(defun lse-insert-buffer (&optional buffer)
  (interactive "bBuffer to paste: ")
  (insert-buffer buffer)
  (lse-tpu:unselect t)
; lse-insert-buffer
)

(defun lse-write-buffer (buf)
  "Write buffer to output file."
  (interactive "bBuffer to write: ")
  (save-excursion
    (set-buffer (get-buffer buf))
    (set-buffer-modified-p t)
    (let ((inhibit-point-motion-hooks t); 18-May-2003
          (inhibit-read-only          t); 18-May-2003
         )
      (save-buffer nil)
    )
  )
)

(defun lse-write-current-buffer ()
  "Write current buffer to output file."
  (interactive)
  (lse-write-buffer (current-buffer))
)

;;;  5-Apr-1998
(defun lse-save-some-buffers (&optional dont-ask)
  "Save some modified file-visiting buffers.  Asks user about each one.
Optional argument (the prefix) non-nil means save all with no questions."
  (interactive "P")
  (save-window-excursion
    (save-excursion
      (let ((inhibit-point-motion-hooks t); 18-May-2003
            (inhibit-read-only          t); 18-May-2003
           )
        (save-some-buffers dont-ask)
      )
    )
  )
; lse-save-some-buffers
)

(defun lse-set-buffer-nowrite (buf)
  "Set buffer nowrite (remove output file)."
  (interactive "bBuffer to set nowrite: ")
  (lse-change-output-file buf "")
  (save-excursion
    (set-buffer buf)
    (auto-save-mode -1)
  )
)

(defun lse-set-buffer-write (buf)
  "Set buffer write (define output file)."
  (interactive "bBuffer to set write: ")
  (save-excursion
    (set-buffer (get-buffer buf))
    (lse-change-output-file buf lse-buffer:file-name)
    (auto-save-mode 300)
  )
)

(defun lse-rename-buffer (buf &optional new-name)
  "Rename buffer."
  (interactive "bRename buffer: ")
  (if (not new-name)
      (setq new-name (lse-buffer:read-name "New name: " buf))
  )
  (save-excursion
    (set-buffer (get-buffer buf))
    (rename-buffer new-name)
  )
  new-name
)

(defun lse-goto-next-buffer ()
  "Goto next (younger) buffer"
  (interactive)
  (lse-goto-buffer (lse-buffer:next) nil nil t)
)

(defun lse-goto-prev-buffer ()
  "Goto to previous (next older) buffer"
  (interactive)
  (lse-goto-buffer (lse-buffer:prev) nil nil t)
)

(defun lse-goto-buffer+maybe-create (&optional buf temporary no-mark)
  "Goto to specified buffer. Creates the buffer if it does not exist."
  (interactive)
  (lse@goto@buffer 'switch-to-buffer buf t temporary no-mark)
)

(defun lse@goto@buffer
           (switch-cmd &optional buf may-create temporary no-mark default)
  ;; If 'buf' does not exist, it is created when 'may-create' is non-nil.
  ;; 'temporary' disables output file for newly created buffer
  ;; The last position is marked unless no-mark is non-nil.
  (setq buf
        (or buf (lse-buffer:read-name "Goto buffer: "
                    (or default (lse-tpu:selection) (other-buffer))
                    (not may-create)
                )
        )
  )
  (if buf
      (let (buf-is-new)
        (if (not (get-buffer buf))
            (if (not (get-buffer (file-name-nondirectory buf)))
                (setq buf-is-new t)
            )
        )
        (if (equal (current-buffer) (get-buffer buf))
            nil
          (if no-mark
              nil
            (lse-set-last-mark-all)
          )
          (funcall switch-cmd
                   (if (bufferp buf) buf (file-name-nondirectory buf))
          )
          ;; here the name of the output file of the created buffer should be set
          ;; more appropriately
          (if buf-is-new
              (progn
                (if (not temporary)
                    (progn
                      (setq buffer-file-name buf)
                      (set-buffer-modified-p nil)
                      (after-find-file); calls also (lse-buffer:initialize)
                    )
                )
              )
          )
        )
      )
  )
)

(defun lse-goto-buffer-other-window
           (&optional buf may-create temporary no-mark default)
  "Goto a buffer in another window"
  (interactive)
  (lse@goto@buffer
       'switch-to-buffer-other-window buf may-create temporary no-mark
       (or default "")
  )
)

(defun lse-goto-buffer
           (&optional buf may-create temporary no-mark default)
  "Goto to another buffer"
  (interactive)
  (lse@goto@buffer
        'switch-to-buffer buf may-create temporary no-mark
        (or default "")
  )
)

;;;;++
;;; Buffer specific mark-stack
;;;;--
(defvar lse-buffer-mark@stack nil
  ;; List of buffer-local marks defined by user.
)

(make-variable-buffer-local 'lse-buffer-mark@stack)

(defun lse-goto-last-mark-buffer ()
  "Goto buffer last mark."
  (interactive)
  (lse-goto-last-mark lse-buffer-mark@stack)
)

(defun lse-goto-home-mark-buffer ()
  "Goto buffer home mark."
  (interactive)
  (lse-goto-home-mark lse-buffer-mark@stack)
)

(defun lse-set-last-mark-buffer (&optional to-mark)
  "Set buffer last mark to specified mark."
  (interactive "d")
  (lse-set-last-mark
       lse-buffer-mark@stack
       (if (integerp to-mark) (copy-marker to-mark) to-mark)
  )
)

(defun lse-set-home-mark-buffer (to-mark)
  "Set buffer home mark to specified mark."
  (interactive "d")
  (lse-set-home-mark lse-buffer-mark@stack
                       (if (integerp to-mark) (copy-marker to-mark) to-mark)
  )
  (message (format
              "Home mark of current buffer set to position line %d, column %d"
              (lse-line-number) (1+ (current-column))
           )
  );  3-Nov-1994
)

(defun lse-push-mark-buffer ()
  "Push mark onto buffer mark stack."
  (interactive)
  (lse-push-mark lse-buffer-mark@stack)
)

(defun lse-goto-mark-and-pop-buffer ()
  "Goto top mark of buffer mark stack and remove it."
  (interactive)
  (lse-goto-mark-and-pop lse-buffer-mark@stack)
)

(defun lse-toggle-mark-buffer ()
  "Goto top mark of buffer mark-stack and replace it by previous position."
  (interactive)
  (lse-toggle-mark lse-buffer-mark@stack)
)
