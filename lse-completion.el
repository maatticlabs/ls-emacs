;-*-unibyte: t;-*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work
 
;;;;unix_ms_filename_correspondency lse-completion:el lse_cmpl:el
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
;;;;    lse-completion
;;;;
;;;; Purpose
;;;;    Provide completion by menu
;;;;
;;;; Revision Dates
;;;;    18-Jun-1994 (CT) Creation (of comment)
;;;;    29-Jun-1994 (CT) lse_completion:help added
;;;;     1-Aug-1994 (CT) Optional parameter start-position added to
;;;;                     lse-complete and lse_completion:key_handler
;;;;     8-Sep-1994 (CT) Optional parameter case-fold added to lse-complete
;;;;    19-Mar-1995 (CT) lse_completion:overlay added
;;;;     1-Apr-1996 (CT) Override `completion-ignore-case' and
;;;;                     `case-fold-search' with value of
;;;;                     `lse-completion:case-fold' 
;;;;    11-Oct-1996 (CT) Bindings for mouse buttons added and mouse-face set
;;;;    16-Oct-1996 (CT) lse-face:completion-m used instead of
;;;;                     lse-face:completion for mouse-face
;;;;    17-Dec-1997 (CT) `lse_completion:mouse_exit' added
;;;;    10-Jan-1998 (CT) Moved most Control-Keys to Alt-Keys
;;;;    ««revision-date»»···
;;;;-- 
(provide 'lse-completion)

(defvar lse_completion_buffer         nil)
(defvar lse_completion_keymap         (make-keymap))

(defvar                      lse_completion:end_pos        nil)
(make-variable-buffer-local 'lse_completion:end_pos)

(defvar                      lse_completion:sorted         nil)
(defvar                      lse_completion:current        nil)
(defvar                      lse_completion:so_far         nil)
(defvar                      lse_completion:starter        nil)
(defvar                      lse_completion:last           nil)
(defvar                      lse_completion:list           nil)
(defvar                      lse_completion:last_key       nil)
(defvar                      lse_completion:saved_wdw_conf nil)
(defvar                      lse_completion:helped         nil); 29-Jun-1994 
(defvar                      lse_completion:case-fold      nil);  8-Sep-1994 
(defconst                    lse_completion:left_margin    3)
(defvar                      lse_completion:overlay        nil); 19-Mar-1995 

(defun lse_completion:widen ()
  (widen)
  (narrow-to-region 1 (max lse_completion:end_pos 1))
)

(defun lse-completion:narrow ()
  (save-excursion
    (let (head 
          tail
         )
      (setq head (lse_completion:goto_match))
      (setq tail (lse_completion:goto_last_match))
      (lse_completion:widen)
      (narrow-to-region head (lse-tpu:line-tail-pos))
    )
  )
)

(defun lse_completion:current_item ()
  (if (equal (current-buffer) lse_completion_buffer)
      (let (bp result)
        (save-excursion
          (beginning-of-line)
          (lse-tpu:forward-char lse_completion:left_margin)
          (setq bp (point))
          ;; allow blanks inside entries
          (skip-chars-forward "^\C-i")
          (setq result (buffer-substring-no-properties bp (point)))
        )
        result
      )
  )
)

(defun lse_completion:goto_match (&optional pat)
  (lse_completion:widen)
  (goto-char 1)
  (re-search-forward 
       (concat "^"
               (make-string  lse_completion:left_margin ? ) 
               (regexp-quote (or pat lse_completion:so_far))
       ) 
       nil t
  )
  (beginning-of-line)
  (point)
)

(defun lse_completion:goto_last_match (&optional pat)
  (goto-char (point-max))
  (re-search-backward 
       (concat "^"
               (make-string  lse_completion:left_margin ? ) 
               (regexp-quote (or pat lse_completion:so_far))
       ) 
       nil t
  )
  (beginning-of-line)
  (point)
)

;;;;;; 16-Oct-1996 
;;;(defun lse_completion:goto_last_match (&optional pat)
;;;  (let ((p (concat "^"
;;;               (make-string  lse_completion:left_margin ? ) 
;;;               (regexp-quote (or pat lse_completion:so_far))
;;;           )
;;;        )
;;;        result
;;;        last
;;;       )
;;;    (setq last (lse_completion:goto_match pat))
;;;    (while (not result)
;;;      (lse-tpu:next-line-internal 1)
;;;      (if (not (looking-at p))
;;;          (setq result (point))
;;;        (setq last (point))
;;;      )
;;;    )
;;;    (goto-char last)
;;;    last
;;;  )
;;;)

(defun lse_completion:highlight ()
  (beginning-of-line)
  (setq overlay-arrow-string   "#>")
  (setq overlay-arrow-position (point-marker))
  (move-overlay lse_completion:overlay (point-marker) (lse-tpu:line-tail-pos)
                (current-buffer)
  )
  (lse-tpu:forward-char lse_completion:left_margin)
  (lse-tpu:update-mode-line)
)

(defun lse_completion:abort ()
  (interactive)
  (setq lse_completion:so_far nil)
  (throw 'exit@lse_completion nil)
)

(defun lse_completion:exit ()
  (interactive)
  (setq lse_completion:so_far (lse_completion:current_item))
  (throw 'exit@lse_completion nil)
)

;;; 17-Dec-1997 
(defun lse_completion:mouse_exit ()
  (interactive)
  (call-interactively 'mouse-set-point)
  (lse_completion:exit)
; lse_completion:mouse_exit
)

(defun lse_completion:try (so-far)
  (if lse_completion:list
      (try-completion so-far lse_completion:list)
    (let (result
          head-match
          tail-match
          matches
         )
      (save-excursion
        (lse_completion:goto_match      so-far)
        (setq head-match                (lse_completion:current_item))
        (lse_completion:goto_last_match so-far)
        (setq tail-match                (lse_completion:current_item))
        (setq matches (list (cons head-match nil) (cons tail-match nil)))
        (setq result  (try-completion so-far matches))
      )
      result
    )
  )
)

(defun lse_completion:complete (&optional add-chars quiet)
  (interactive)
  (let* ((so-far (concat lse_completion:so_far 
                         (or add-chars "")
                 )
         )
         (new-compl (lse_completion:try so-far)) 
         (case-fold-search       lse_completion:case-fold);  1-Apr-1996 
         (completion-ignore-case lse_completion:case-fold);  1-Apr-1996 
         is-complete
        )
    (if (not new-compl)
        (if lse_completion:case-fold
            (let (lse_completion:case-fold
                 )
              (setq is-complete
                    (lse_completion:complete (downcase add-chars) quiet)
              )
            )
          (if (not quiet) (message "No match"))
        )
      (if (equal new-compl t) (setq new-compl so-far))
      (if lse_completion:so_far
          (lse-add-to-list lse_completion:last lse_completion:so_far)
      )
      (setq lse_completion:so_far new-compl)
      (lse_completion:goto_match)
      (setq is-complete (equal so-far (lse_completion:current_item)))
    )
    is-complete
  )
)

(defun lse_completion:delete_prev_char ()
  (interactive)
  (if (> (length lse_completion:so_far) (length lse_completion:starter))
      (setq lse_completion:so_far (substring lse_completion:so_far 0 -1))
  )
  (lse_completion:goto_match)
)

(defun lse_completion:delete_last_completion ()
  (interactive)
  (setq lse_completion:so_far (car lse_completion:last))
  (if (> (length lse_completion:last) 1) 
      (lse-remove-car-from-list lse_completion:last)
  )
  (lse_completion:goto_match)
)

(defun lse_completion:delete_so_far ()
  (interactive)
  (setq lse_completion:so_far lse_completion:starter)
  (lse_completion:goto_match)
)

(defun lse_completion:select_next ()
  (interactive)
  (lse-tpu:next-line-internal 1)
  (setq lse_completion:so_far lse_completion:starter)
)

(defun lse_completion:select_prev ()
  (interactive)
  (lse-tpu:next-line-internal -1)
  (setq lse_completion:so_far lse_completion:starter)
)

(defun lse_completion:self_insert ()
  (interactive)
  (lse_completion:sort)
  (lse_completion:complete (substring lse_completion:last_key -1))
)

(defun lse_completion:sort ()
  (interactive)
  (if lse_completion:sorted
      t
    (sort-lines nil (point-min) (point-max))
    (setq lse_completion:sorted t)
  )
)

;;; 29-Jun-1994 
(defun lse_completion:entry_desc (tail &optional full)
  (let (result
        help
       )
    (cond ((symbolp tail)
           (setq result
                 (or (get tail 'description)
                     (if (fboundp tail) 
                         (documentation tail)
                     )
                 )
           )
           (if full
               (progn
                 (setq help (get tail 'help))
                 (if help
                     (setq result (list result "\n" help))
                 )
               )
           )
          )
          ((stringp tail) (setq result tail))
          ((and (consp tail) (stringp (car tail)))
           (if full
               (setq result tail)
             (setq result (car tail))
           )
          )
          (t nil)
    )
    result
  )
; lse_completion:entry_desc
)

;;; 29-Jun-1994 
(defun lse_completion:help ()
  (interactive)
  (let* ((item (lse_completion:current_item))
         (desc (lse_completion:entry_desc
                    (cdr (assoc item lse_completion:list)) t
               )
         )
        )
    (if (not desc)
        (message (format "No help available for %s" item))
      (if lse_completion:helped
          (progn
            (lse-window:restore-temp-hidden)
            (setq lse_completion:helped nil)
          )
      )
      (with-output-to-temp-buffer " $lse help$"
        (princ "Menu entry : ")
        (princ item)
        (princ "\n\n")
        (if (consp desc)
            (princ (mapconcat 'identity desc "\n"))
          (princ desc)
        )
        (princ "\n")
      )
      (save-excursion 
        (set-buffer " $lse help$")
        (fill-individual-paragraphs (point-min) (point-max))
      )
      (setq lse_completion:helped t)
    )
  )
; lse_completion:help
)

(defun lse_completion:define_keys ()
  (local-set-key [?\C-e]               'lse_completion:exit)
  (local-set-key [?\A-e]               'lse_completion:exit)
  (local-set-key [?\C-g]               'lse_completion:abort)
  (local-set-key [?\A-g]               'lse_completion:abort)
  (local-set-key [tab]                 'lse_completion:exit)
  (local-set-key [?\A-i]               'lse_completion:exit)
  (local-set-key [?\C-j]               'lse_completion:delete_last_completion)
  (local-set-key [?\A-j]               'lse_completion:delete_last_completion)
  (local-set-key [?\C-k]               'lse_completion:abort)
  (local-set-key [?\A-k]               'lse_completion:abort)
  (local-set-key [?\C-m]               'lse_completion:exit)
  (local-set-key [?\A-m]               'lse_completion:exit)
  (local-set-key [return]              'lse_completion:exit)
  (local-set-key [?\C-o]               'lse_completion:help); 29-Jun-1994 
  (local-set-key [?\A-o]               'lse_completion:help); 29-Jun-1994 
  (local-set-key [?\C-u]               'lse_completion:delete_so_far)
  (local-set-key [?\A-u]               'lse_completion:delete_so_far)

  (local-set-key [gold ?\C-e]     'lse_completion:abort)
  (local-set-key [gold ?\A-e]     'lse_completion:abort)
  (local-set-key [left]           'lse-tpu:pan-right)
  (local-set-key [right]          'lse-tpu:pan-left)
  (local-set-key [down]           'lse_completion:select_next)
  (local-set-key [up]             'lse_completion:select_prev)
  (local-set-key [help]           'lse_completion:help); 29-Jun-1994 
  (local-set-key [f1]             'lse_completion:help);  2-Jan-1998 
  (local-set-key [blue gold help] 'help-command); 29-Jun-1994 
  (local-set-key [find]           'lse-tpu:search)
  (local-set-key [prior]          (lse-key-cmd (lse-previous-screen 2)))
  (local-set-key [next]           (lse-key-cmd (lse-next-screen 2)))
  (local-set-key [pf3]            'lse-tpu:search-again)
  (local-set-key [gold pf3]       'lse-tpu:search)
  (local-set-key [gold kp-5]      'lse-tpu:move-to-beginning)
  (local-set-key [gold kp-4]      'lse-tpu:move-to-end)
  (local-set-key [gold ?>]        'lse-tpu:pan-right)
  (local-set-key [gold ?<]        'lse-tpu:pan-left)
  (local-set-key [gold ?^]        (lambda nil (interactive) (lse-frame:set-width 132)))
  (local-set-key [blue ?^]        (lambda nil (interactive) (lse-frame:set-width 0)))
  (local-set-key [gold ??]        'lse_completion:help); 29-Jun-1994 
  (local-set-key [gold ?s]        'lse_completion:sort)
  (local-set-key "\177"           'lse_completion:delete_prev_char)
  ;; (local-set-key [del]         'lse_completion:delete_prev_char)

  (local-set-key [mouse-1]        'mouse-set-point)      ; 11-Oct-1996
  (local-set-key [mouse-2]        'lse_completion:mouse_exit); 17-Dec-1997 
  (local-set-key [mouse-3]        'mouse-set-point)      ; 11-Oct-1996
  (local-set-key [double-mouse-1] 'lse_completion:exit)  ; 11-Oct-1996 
  ;; (local-set-key [double-mouse-2] 'lse_completion:exit)  ; 11-Oct-1996 
  (local-set-key [double-mouse-3] 'lse_completion:exit)  ; 11-Oct-1996 

  (let ((i ? ))
    (while (< i ?~)
      (local-set-key (char-to-string i) 'lse_completion:self_insert)
      (setq i (1+ i))
    )
  )
)

(defun lse_completion:goto_buffer_window (buf)
  ; (split-window-vertically (/ (screen-height) 4))
  (pop-to-buffer buf)
)

(defun lse_completion:initialize_buffer (buf-nam)
  (save-excursion
    (set-buffer lse_completion_buffer)
    (use-local-map lse_completion_keymap)
    (lse_completion:define_keys)
    (setq indent-tabs-mode nil)
    (setq tab-width        2)
    (setq mode-line-format
          (list (purecopy (concat "%%%% " buf-nam ": `"))
                'lse_completion:so_far
                (purecopy "'                  `")
                'lse_completion:current
                (purecopy "'")
          )
    )
  ) 
)

(defun lse_completion:buffer (buf-nam)
  (if (not (bufferp lse_completion_buffer))
      (save-excursion
        (set-buffer (setq lse_completion_buffer 
                          (get-buffer-create (concat " $" buf-nam " buffer$"))
                    )
        )
        (lse_completion:initialize_buffer buf-nam)
        ;;  19-Mar-1995 
        (setq lse_completion:overlay (make-overlay 1 1))
        (overlay-put lse_completion:overlay 'face 'lse-face:completion)
      )
  )
  lse_completion_buffer 
)

(defun lse_completion:show_entry (entry description)
  (let ((opoint (point))); 11-Oct-1996 
    (lse-indent)
    (lse-fill-in-insert entry "\C-i")
    (indent-to 25)
    (add-text-properties opoint (point) '(mouse-face lse-face:completion-m))
    (if (stringp description)
        (lse-fill-in-insert (lse-tpu:remove-char-from-string ?\n description))
      (if (and (consp description) (stringp (car description)))
          (lse-fill-in-insert
               (lse-tpu:remove-char-from-string ?\n (car description))
          )
      )
    )
    ;; 11-Oct-1996 
  )
  (lse-fill-in-insert "\n")
)

(defun lse_completion:show_obarray (starter oba dont-sort)
  (let ((completions (all-completions starter oba))
       )
    (lse_completion:show_alist 
         starter
         (mapcar
              (function (lambda (x) 
                          (cons x
                                (or (get (intern-soft x oba) 'description)
                                    ""
                                )
                          )
                        )
              )
              completions
         )
         dont-sort
    )
  )
)

(defun lse_completion:< (l r)
  (string< (car l) (car r))
)

(defun lse_completion:show_alist (starter the-completions dont-sort)
  (let (head
        (completions 
             (if dont-sort 
                 the-completions
               (setq lse_completion:sorted t)
               (sort (copy-sequence the-completions) 'lse_completion:<)
             )
        )
        tail
       )
    (setq lse_completion:list completions); 29-Jun-1994
    (while (consp completions)
      (setq head        (car completions))
      (setq tail        (cdr head))
      (setq completions (cdr completions))
      (lse_completion:show_entry
           (car head)
           (lse_completion:entry_desc tail)
      )
    )
  )
)

(defun lse_completion:show (starter completions dont-sort)
  (widen)
  (erase-buffer)
  (let ((lse@current-expansion-indent lse_completion:left_margin)
       )
    (if (vectorp completions)
        (lse_completion:show_obarray starter completions dont-sort)
      (if (consp completions)
          (lse_completion:show_alist starter completions dont-sort)
        (error "Argument to lse-complete not an obarray or alist: %s" 
               completions
        )
      )
    )
  )
  (setq lse_completion:end_pos (1- (point)))
  (lse_completion:widen)
  (goto-char 1)
; lse_completion:show
)

(defun lse_completion:key_handler (&optional start-position)
  (let (binding helped)
    (lse_completion:goto_match start-position)
    (while t
      (unwind-protect
          (progn
            (setq lse_completion:current (lse_completion:current_item))
            (lse-completion:narrow)
            (lse_completion:highlight)
            (setq lse_completion:last_key (read-key-sequence nil))
            (setq binding 
                  (lookup-key (current-local-map) lse_completion:last_key)
            )
            (if (commandp binding)
                (command-execute binding)
              (lse-message "Key %s is undefined" 
                       (lse-key-name lse_completion:last_key)
              )
            )
            (if helped                             ; 29-Jun-1994 
                (progn
                  (lse-window:restore-temp-hidden)
                  (setq lse_completion:helped nil)
                  (setq helped                nil)
                )
              (setq helped lse_completion:helped)
            )
          )
        nil
      )
    )
  )
)

(defun lse-complete 
         (starter completions
          &optional dont-sort dont-initialize-buffer force start-position
                    case-fold
         )
  (setq lse_completion:saved_wdw_conf (current-window-configuration))
  (setq lse_completion:so_far         starter)
  (setq lse_completion:case-fold      case-fold)
  (unwind-protect
      (progn 
        (catch 'exit@lse_completion
          (progn
            (lse_completion:goto_buffer_window 
                 (lse_completion:buffer "LSE Completion")
            )
            (let* ((lse_completion:list    completions)
                   (lse_completion:starter (or starter ""))
                   (lse_completion:last    (list lse_completion:starter))
                   (lse_completion:current nil)
                   (echo-keystrokes        0)
                  )
              (if dont-initialize-buffer
                  t
                (lse_completion:show starter completions dont-sort)
              )
              (if (or (not (lse_completion:complete nil t)) force)
                  (lse_completion:key_handler start-position)
              )
            )
          )
        )
      )
    (progn
      (lse_completion:widen)
      (set-window-configuration lse_completion:saved_wdw_conf)
    )
  )  
  lse_completion:so_far
; lse-complete
)
