;-*-unibyte: t;-*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work
 
;;;;unix_ms_filename_correspondency lse-fill-in-history:el lse_fihi:el
;;;; (c) 1995 Swing Informationssysteme GmbH. All rights reserved.

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
;;;;    lse-fill-in-history
;;;;
;;;; Purpose
;;;;    Manages history of fill-in expansion and un-expansion
;;;;
;;;; Revision Dates
;;;;    20-Mar-1995 (CT) Creation (of comment)
;;;;    20-Mar-1995 (CT) Provision for management of fill-in's in state
;;;;                     'lsde@dead
;;;;    10-Mar-1996 (CT) `lse_fill-in_history:instances' added
;;;;    17-Oct-1996 (CT) lse_fill-in_history:show_expansion changed to use
;;;;                     princ instead of insert 
;;;;-- 
(provide 'lse-fill-in-history)

;;;+
;;; lse_fill-in_history/expansion   stores the fill-ins deep so far
;;; lse_fill-in_history/unexpansion stores the fill-ins flat so far
;;;-
; (setq lse_fill-in_history/expansion nil)
; (setq lse_fill-in_history/unexpansion nil)
(defvar                      lse_fill-in_history/expansion   nil)
(defvar                      lse_fill-in_history/unexpansion nil)
(make-variable-buffer-local 'lse_fill-in_history/expansion)
(make-variable-buffer-local 'lse_fill-in_history/unexpansion)

(defmacro lse_fill-in_history:add_expansion (fill-in_info)
  (` (progn
       (lse-add-to-list lse_fill-in_history/expansion (, fill-in_info))
       (lse_fill-in_history:purge_collapsed)
     )
  )
)

(defmacro lse_fill-in_history:add_unexpansion (fill-in_info &optional history)
  (` (lse-add-to-list lse_fill-in_history/unexpansion (, fill-in_info)))
)

(defmacro lse_fill-in_history:last_expansion ()
  (` (progn
       (lse_fill-in_history:purge_collapsed)
       (car lse_fill-in_history/expansion)
     )
  )
)

;;; 20-Mar-1995 
(defun lse-fill-in-history:last_expansion (&optional state)
  (or state (setq state (list 'lse@deep)))
  (let* ((pred lse_fill-in_history/expansion)
         (result (car pred))
       )
    (while (and (consp pred)
                (not (memq (lse-fill-in:state result) state))
           )
      (setq pred   (cdr pred))
      (setq result (car pred))
    )
    result
  )
; lse-fill-in-history:last_expansion
)

(defmacro lse_fill-in_history:last_unexpansion ()
  (` (car lse_fill-in_history/unexpansion))
)

;;; 20-Mar-1995 
(defmacro lse_fill-in_history:purge_last_expansion
              (&optional replace-by state)
  (or state (setq state (list 'lse@deep)))
  (if lse_fill-in_history/expansion
      (if (car lse_fill-in_history/expansion); sometimes it's (nil)! why????
          (` (let (pred
                  )
               (progn
                 (if (memq (lse-fill-in:state
                                (car lse_fill-in_history/expansion) state
                           )
                     )
                     (if (, replace-by)
                         (setcar lse_fill-in_history/expansion (, replace-by)
                         )
                       (setq lse_fill-in_history/expansion
                             (cdr lse_fill-in_history/expansion)
                       )
                     )
                   (let ((pred lse_fill-in_history/expansion))
                     (while (and (consp pred)
                                 (not (memq (lse-fill-in:state (car(cdr pred)))
                                            state
                                      )
                                 )
                            )
                       (setq pred (cdr pred))
                     )
                     (if (consp pred)
                         (if (, replace-by)
                             (setcdr pred
                                     (cons (, replace-by) (cdr (cdr pred)))
                             )
                           (setcdr pred (cdr (cdr pred)))
                         )
                     )
                   )
                 )
               )
             )
          )
      )
  )
; lse_fill-in_history:purge_last_expansion
)

(defmacro lse_fill-in_history:change_last_expansion (fill-in_info)
  (` (setcar lse_fill-in_history/expansion (, fill-in_info)))
)

(defmacro lse_fill-in_history:remove_last_expansion ()
  (` (setq lse_fill-in_history/expansion (cdr lse_fill-in_history/expansion)))
)

(defun lse-fill-in-history:change_last_expansion (fill-in_info &optional state)
  (lse_fill-in_history:purge_last_expansion fill-in_info state)
; lse-fill-in-history:change_last_expansion
)

(defun lse-fill-in-history:remove_last_expansion (&optional state)
  (lse_fill-in_history:purge_last_expansion nil state)
; lse-fill-in-history:remove_last_expansion
)

(defmacro lse_fill-in_history:remove_last_unexpansion ()
  (` (setq lse_fill-in_history/unexpansion
           (cdr lse_fill-in_history/unexpansion)
     )
  )
)

(defmacro lse_fill-in_history:purge_unexpansion
              (the-psym the-range &optional replace-by)
  (if lse_fill-in_history/unexpansion
      (if (car lse_fill-in_history/unexpansion); sometimes it's (nil)! why????
          (` (let (pred
                   (psym (, the-psym))
                   (head (lse-range:head-pos (, the-range)))
                   (tail (lse-range:tail-pos (, the-range)))
                  )
               (progn
                 (if (lse-fill-in:equal (car lse_fill-in_history/unexpansion)
                                        psym head tail
                     )
                     (if (, replace-by)
                         (setcar lse_fill-in_history/unexpansion
                                 (, replace-by)
                         )
                       (setq lse_fill-in_history/unexpansion
                             (cdr lse_fill-in_history/unexpansion)
                       )
                     )
                   (let ((pred lse_fill-in_history/unexpansion))
                     (while (and (consp pred)
                                 (not (lse-fill-in:equal (car (cdr pred))
                                                         psym head tail
                                      )
                                 )
                            )
                       (setq pred (cdr pred))
                     )
                     (if (consp pred)
                         (if (, replace-by)
                             (setcdr pred
                                     (cons (, replace-by) (cdr (cdr pred)))
                             )
                           (setcdr pred (cdr (cdr pred)))
                         )
                     )
                   )
                 )
               )
             )
          )
      )
  )
)

(defun lse_fill-in_history:purge_collapsed ()
  (let ((pred lse_fill-in_history/expansion)
       )
    (while (and (consp pred)
                (lse-range:is-collapsed (lse-fill-in:range (car pred)))
                ;; 20-Mar-1995 retain entries in state 'lse@dead 
                (not (equal (lse-fill-in:state (car pred)) 'lse@dead))
           )
      (lse_fill-in_history:remove_last_expansion)
    )
;;; 20-Mar-1995 
    (while (consp pred)
      (if (and (lse-range:is-collapsed (lse-fill-in:range (car (cdr pred))))
               (not (equal (lse-fill-in:state (car (cdr pred))) 'lse@dead))
          )
          (setcdr pred (cdr (cdr pred)))
      )
      (setq   pred (cdr pred))
    )
;;; before 20-Mar-1995 this was used ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;    (while (and (consp pred)
;;;                (lse-range:is-collapsed (lse-fill-in:range (car (cdr pred))))
;;;           )
;;;      (setcdr pred (cdr (cdr pred)))
;;;      (setq   pred (cdr pred))
;;;    )
  )
)

(defun lse_fill-in_history:> (l r)
  (> (lse-range:head-pos (lse-fill-in:range l))
     (lse-range:head-pos (lse-fill-in:range r))
  )
)

(defun lse_fill-in_history:assq (key history-alist &optional before state)
  (or before (setq before (point-marker)))
  (or state  (setq state  (list 'lse@deep))); 20-Mar-1995 
  (lse_fill-in_history:purge_collapsed)     ; 21-Mar-1995 
  (let* ((pred (sort (copy-sequence history-alist) 'lse_fill-in_history:>))
         (result (car pred))
       )
    (while (and (consp pred)
                (not (and (eq   (lse-fill-in:symbol result) key)
                          (memq (lse-fill-in:state  result) state); 20-Mar-1995
                          (< (lse-range:head-pos (lse-fill-in:range result))
                             before
                          )
                     )
                )
           )
      (setq pred   (cdr pred))
      (setq result (car pred))
    )
    result
  )
)

;;;; 10-Mar-1996 
(defun lse_fill-in_history:instances (key history-alist &optional state)
  (or state  (setq state  (list 'lse@deep))); 20-Mar-1995 
  (lse_fill-in_history:purge_collapsed)     ; 21-Mar-1995 
  (let* ((pred (sort (copy-sequence history-alist) 'lse_fill-in_history:>))
         (instance (car pred))
         result
       )
    (while (consp pred)
      (if (and (eq   (lse-fill-in:symbol instance) key)
               (memq (lse-fill-in:state  instance) state)
          )
          (lse-add-to-list result instance)
      )
      (setq pred     (cdr pred))
      (setq instance (car pred))
    )
    result
  )
)

(defun lse_fill-in_history@show_title (leading)
  (princ (format "%-25s" (concat leading " fill-in")))
  (princ (format " %4s %5s %5s" "  id" " head" " tail"))
  (princ (format " %-10s" "State"))
  (princ (format " Complement"))
  (princ "\n")
  (princ (make-string 79 ?*))
)

(defun lse_fill-in_history:show_expansion ()
  (interactive)
  (let ((history  lse_fill-in_history/expansion)
        (uhistory lse_fill-in_history/unexpansion)
        (current  lse_current_fill-in)
        (dead     lse_dead_fill-in)
        (replaced lse_replaced_fill-in)
       )
    (with-output-to-temp-buffer " *Fill-In History*"
      (lse_fill-in_history@show_title "Deep")
      (mapcar 'lse-fill-in:show history)
      (princ "\n\n\n")
      (lse_fill-in_history@show_title "Flat")
      (mapcar 'lse-fill-in:show uhistory)

      (princ "\n\n\n")
      (lse_fill-in_history@show_title "Current")
      (lse-fill-in:show current)
      (princ "\n\n")
      (lse_fill-in_history@show_title "Cleaned")
      (lse-fill-in:show dead)
      (princ "\n\n")
      (lse_fill-in_history@show_title "Replaced")
      (lse-fill-in:show replaced)
    )
  )
)
