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
;;;;    lse-completion
;;;;
;;;; Purpose
;;;;    Provide completion by menu
;;;;
;;;; Revision Dates
;;;;    18-Jun-1994 (CT) Creation (of comment)
;;;;    29-Jun-1994 (CT) lse-completion:help added
;;;;     1-Aug-1994 (CT) Optional parameter start-position added to
;;;;                     lse-complete and lse-completion:key_handler
;;;;     8-Sep-1994 (CT) Optional parameter case-fold added to lse-complete
;;;;    19-Mar-1995 (CT) lse-completion:overlay added
;;;;     1-Apr-1996 (CT) Override `completion-ignore-case' and
;;;;                     `case-fold-search' with value of
;;;;                     `lse-completion:case-fold'
;;;;    11-Oct-1996 (CT) Bindings for mouse buttons added and mouse-face set
;;;;    16-Oct-1996 (CT) lse-face:completion-m used instead of
;;;;                     lse-face:completion for mouse-face
;;;;    17-Dec-1997 (CT) `lse-completion:mouse_exit' added
;;;;    10-Jan-1998 (CT) Moved most Control-Keys to Alt-Keys
;;;;     5-Oct-2007 (CT) lse-completion:define_keys modernized
;;;;     5-Oct-2007 (CT) Replace `search` by `search-forward` and
;;;;                     `search-reverse`
;;;;     8-Oct-2007 (CT) `lse-completion:left_margin` inscreased from `3` to `6`
;;;;     8-Oct-2007 (CT) `lse-completion:show_entry` changed to show an
;;;;                     optional `head`
;;;;     8-Oct-2007 (CT) `lse-completion:show_alist` changed to handle plain
;;;;                     lists and to put an index into left margin
;;;;     9-Oct-2007 (CT) `lse-completion:entry_desc` changed to consider
;;;;                     'variable-documentation, too
;;;;     9-Oct-2007 (CT) `lse-completion:show_alist` changed to handle
;;;;                     lists of symbols properly
;;;;    10-Oct-2007 (CT) `lse-completion:entry_desc` changed to use
;;;;                     `documentation-property` instead of home-grown code
;;;;    12-Oct-2007 (CT) Guard `(equal (current-buffer) lse-completion:buffer)`
;;;;                     added to `lse-completion:widen`,
;;;;                     `lse-completion:narrow`, `lse-completion:key_handler`
;;;;    12-Oct-2007 (CT) Bindings for `[mouse-4]` and `[mouse-5]` added
;;;;    27-Feb-2012 (CT) Use `replace-regexp-in-string` instead of
;;;;                     `lse-tpu:remove-char-from-string`
;;;;                     (that macro gave a very obscure compilation warning)
;;;;    13-Nov-2014 (CT) Use `lse-keys/define`
;;;;    19-Nov-2014 (CT) Add `lse-completion:hide-leader`,
;;;;                     `lse-completion:desc-indent`
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-completion)

(defvar   lse-completion:buffer         nil)
(defvar   lse-completion:keymap         (make-keymap))

(defvar   lse-completion:case-fold      nil)                       ;  8-Sep-1994
(defvar   lse-completion:current        nil)
(defvar   lse-completion:desc-indent    18)                        ; 19-Nov-2014
(defvar   lse-completion:end_pos        nil)
(defvar   lse-completion:helped         nil)                       ; 29-Jun-1994
(defvar   lse-completion:hide-leader    nil)                       ; 19-Nov-2014
(defvar   lse-completion:index-start    1)                         ; 8-Oct-2007
(defvar   lse-completion:last           nil)
(defvar   lse-completion:last_key       nil)
(defconst lse-completion:left_margin    6)
(defvar   lse-completion:list           nil)
(defvar   lse-completion:overlay        nil)                       ; 19-Mar-1995
(defvar   lse-completion:saved_wdw_conf nil)
(defvar   lse-completion:so_far         nil)
(defvar   lse-completion:sorted         nil)
(defvar   lse-completion:starter        nil)

(make-variable-buffer-local             'lse-completion:end_pos)

(defun lse-completion:widen ()
  (if (equal (current-buffer) lse-completion:buffer)
      (progn
        (widen)
        (narrow-to-region 1 (max lse-completion:end_pos 1))
      )
  )
)

(defun lse-completion:narrow ()
  (if (equal (current-buffer) lse-completion:buffer)
      (save-excursion
        (let (head
              tail
             )
          (setq head (lse-completion:goto_match))
          (setq tail (lse-completion:goto_last_match))
          (lse-completion:widen)
          (narrow-to-region head (lse-tpu:line-tail-pos))
        )
      )
  )
)

(defun lse-completion:current_item ()
  (if (equal (current-buffer) lse-completion:buffer)
      (let (bp result)
        (save-excursion
          (beginning-of-line)
          (lse-tpu:forward-char lse-completion:left_margin)
          (setq bp (point))
          ;; allow blanks inside entries
          (skip-chars-forward "^\C-i")
          (setq result (buffer-substring-no-properties bp (point)))
        )
        result
      )
  )
)

(defun lse-completion:goto_match (&optional pat)
  (lse-completion:widen)
  (goto-char 1)
  (re-search-forward
       (concat "^"
               (make-string  lse-completion:left_margin ?.)
               (regexp-quote (or pat lse-completion:so_far))
       )
       nil t
  )
  (beginning-of-line)
  (point)
)

(defun lse-completion:goto_last_match (&optional pat)
  (goto-char (point-max))
  (re-search-backward
       (concat "^"
               (make-string  lse-completion:left_margin ?.)
               (regexp-quote (or pat lse-completion:so_far))
       )
       nil t
  )
  (beginning-of-line)
  (point)
)

(defun lse-completion:highlight ()
  (beginning-of-line)
  (setq overlay-arrow-string   "#>")
  (setq overlay-arrow-position (point-marker))
  (move-overlay lse-completion:overlay
    (point-marker) (lse-tpu:line-tail-pos) (current-buffer)
  )
  (lse-tpu:forward-char lse-completion:left_margin)
  (lse-tpu:update-mode-line)
)

(defun lse-completion:abort ()
  (interactive)
  (setq lse-completion:so_far nil)
  (throw 'lse-completion:exit nil)
)

(defun lse-completion:exit ()
  (interactive)
  (setq lse-completion:so_far (lse-completion:current_item))
  (throw 'lse-completion:exit nil)
)

;;; 17-Dec-1997
(defun lse-completion:mouse_exit ()
  (interactive)
  (call-interactively 'mouse-set-point)
  (lse-completion:exit)
; lse-completion:mouse_exit
)

(defun lse-completion:try (so-far)
  (if lse-completion:list
      (try-completion so-far lse-completion:list)
    (let (result
          head-match
          tail-match
          matches
         )
      (save-excursion
        (lse-completion:goto_match      so-far)
        (setq head-match                (lse-completion:current_item))
        (lse-completion:goto_last_match so-far)
        (setq tail-match                (lse-completion:current_item))
        (setq matches (list (cons head-match nil) (cons tail-match nil)))
        (setq result  (try-completion so-far matches))
      )
      result
    )
  )
)

(defun lse-completion:complete (&optional add-chars quiet)
  (interactive)
  (let* ((so-far (concat lse-completion:so_far (or add-chars "")))
         (new-compl (lse-completion:try so-far))
         (case-fold-search       lse-completion:case-fold);  1-Apr-1996
         (completion-ignore-case lse-completion:case-fold);  1-Apr-1996
         is-complete
        )
    (if (not new-compl)
        (if lse-completion:case-fold
            (let (lse-completion:case-fold
                 )
              (setq is-complete
                    (lse-completion:complete (downcase add-chars) quiet)
              )
            )
          (if (not quiet) (message "No match"))
        )
      (if (equal new-compl t) (setq new-compl so-far))
      (if lse-completion:so_far
          (lse-add-to-list lse-completion:last lse-completion:so_far)
      )
      (setq lse-completion:so_far new-compl)
      (lse-completion:goto_match)
      (setq is-complete (equal so-far (lse-completion:current_item)))
    )
    is-complete
  )
)

(defun lse-completion:delete_prev_char ()
  (interactive)
  (if (> (length lse-completion:so_far) (length lse-completion:starter))
      (setq lse-completion:so_far (substring lse-completion:so_far 0 -1))
  )
  (lse-completion:goto_match)
)

(defun lse-completion:delete_last_completion ()
  (interactive)
  (setq lse-completion:so_far (car lse-completion:last))
  (if (> (length lse-completion:last) 1)
      (lse-remove-car-from-list lse-completion:last)
  )
  (lse-completion:goto_match)
)

(defun lse-completion:delete_so_far ()
  (interactive)
  (setq lse-completion:so_far lse-completion:starter)
  (lse-completion:goto_match)
)

(defun lse-completion:select_next ()
  (interactive)
  (lse-tpu:next-line-internal 1)
  (setq lse-completion:so_far lse-completion:starter)
)

(defun lse-completion:select_prev ()
  (interactive)
  (lse-tpu:next-line-internal -1)
  (setq lse-completion:so_far lse-completion:starter)
)

(defun lse-completion:self_insert ()
  (interactive)
  (lse-completion:sort)
  (lse-completion:complete (substring lse-completion:last_key -1))
)

(defun lse-completion:sort ()
  (interactive)
  (if lse-completion:sorted
      t
    (sort-lines nil (point-min) (point-max))
    (setq lse-completion:sorted t)
  )
)

;;; 29-Jun-1994
(defun lse-completion:entry_desc (tail &optional full)
  (let (result
        help
       )
    (cond ((symbolp tail)
           (setq result
                 (or (get tail 'description)
                     (if (boundp tail)
                         (documentation-property tail 'variable-documentation)
                     )
                     (if (fboundp tail) (documentation tail))
                     (if tail (symbol-name tail))
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
          (t "")
    )
    (or result "")
  )
; lse-completion:entry_desc
)

;;; 29-Jun-1994
(defun lse-completion:help ()
  (interactive)
  (let* ((item (lse-completion:current_item))
         (desc (lse-completion:entry_desc
                    (cdr (assoc item lse-completion:list)) t
               )
         )
        )
    (if (not desc)
        (message (format "No help available for %s" item))
      (if lse-completion:helped
          (progn
            (lse-window:restore-temp-hidden)
            (setq lse-completion:helped nil)
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
      (save-current-buffer
        (set-buffer " $lse help$")
        (fill-individual-paragraphs (point-min) (point-max))
      )
      (setq lse-completion:helped t)
    )
  )
; lse-completion:help
)

(defun lse-completion:define_keys ()
  (lse-keys/define #'local-set-key
    '(
      ([?\C-e]          lse-completion:exit)
      ([?\A-e]          lse-completion:exit)
      ([?\C-f]          lse-tpu:search-forward);  5-Oct-2007
      ([?\s-f]          lse-tpu:search-reverse);  5-Oct-2007
      ([?\C-g]          lse-completion:abort)
      ([?\A-g]          lse-completion:abort)
      ([tab]            lse-completion:exit)
      ([?\A-i]          lse-completion:exit)
      ([?\C-j]          lse-completion:delete_last_completion)
      ([?\A-j]          lse-completion:delete_last_completion)
      ([?\C-k]          lse-completion:abort)
      ([?\A-k]          lse-completion:abort)
      ([?\C-m]          lse-completion:exit)
      ([?\A-m]          lse-completion:exit)
      ([return]         lse-completion:exit)
      ([?\C-n]          lse-tpu:search-again-forward); 5-Oct-2007
      ([?\C-o]          lse-completion:help); 29-Jun-1994
      ([?\A-o]          lse-completion:help); 29-Jun-1994
      ([?\C-p]          lse-tpu:search-again-reverse); 5-Oct-2007
      ([?\C-u]          lse-completion:delete_so_far)
      ([?\A-u]          lse-completion:delete_so_far)

      ([gold ?\C-e]     lse-completion:abort)
      ([gold ?\A-e]     lse-completion:abort)
      ([left]           lse-tpu:pan-right)
      ([right]          lse-tpu:pan-left)
      ([down]           lse-completion:select_next)
      ([up]             lse-completion:select_prev)
      ([help]           lse-completion:help); 29-Jun-1994
      ([f1]             help-command)
      ([C-home]         lse-tpu:move-to-beginning)
      ([C-end]          lse-tpu:move-to-end)
      ([gold ?>]        lse-tpu:pan-right)
      ([gold ?<]        lse-tpu:pan-left)
      ([gold ??]        lse-completion:help); 29-Jun-1994
      ([gold ?s]        lse-completion:sort)
      ("\177"           lse-completion:delete_prev_char)

      ([mouse-1]        mouse-set-point)      ;     11-Oct-1996
      ([mouse-2]        lse-completion:mouse_exit); 17-Dec-1997
      ([mouse-3]        mouse-set-point)      ;     11-Oct-1996
      ([double-mouse-1] lse-completion:exit)  ;     11-Oct-1996
      ([double-mouse-3] lse-completion:exit)  ;     11-Oct-1996
    )
  )
  (local-set-key [next]           (lse-key-cmd (lse-next-screen 2)))
  (local-set-key [prior]          (lse-key-cmd (lse-previous-screen 2)))
  (local-set-key [gold ?^]        (lse-key-cmd (lse-frame:set-width 132)))
  (local-set-key [blue ?^]        (lse-key-cmd (lse-frame:set-width 0)))
  (when (fboundp 'mwheel-scroll); 12-Oct-2007
    (lse-keys/define #'local-set-key
      '(
        ([mouse-4]  mwheel-scroll)
        ([mouse-5]  mwheel-scroll)
       )
    )
  )

  (let ((i ? ))
    (while (< i ?~)
      (local-set-key (char-to-string i) 'lse-completion:self_insert)
      (setq i (1+ i))
    )
  )
)

(defun lse-completion:goto_buffer_window (buf)
  ; (split-window-vertically (/ (screen-height) 4))
  (pop-to-buffer buf)
)

(defun lse-completion:initialize_buffer (buf-nam)
  (save-current-buffer
    (set-buffer lse-completion:buffer)
    (use-local-map lse-completion:keymap)
    (lse-completion:define_keys)
    (setq indent-tabs-mode nil)
    (setq tab-width        2)
    (setq mode-line-format
      (list
        (purecopy (concat "%%%% " buf-nam ": `"))
        'lse-completion:so_far
        (purecopy "'                  `")
        'lse-completion:current
        (purecopy "'")
      )
    )
  )
)

(defun lse-completion:buffer (buf-nam)
  (if (not (bufferp lse-completion:buffer))
      (save-current-buffer
        (set-buffer
          (setq lse-completion:buffer
            (get-buffer-create (concat " $" buf-nam " buffer$"))
          )
        )
        (lse-completion:initialize_buffer buf-nam)
        ;;  19-Mar-1995
        (setq lse-completion:overlay (make-overlay 1 1))
        (overlay-put lse-completion:overlay 'face 'lse-face:completion)
      )
  )
  lse-completion:buffer
)

(defun lse-completion:show_entry (entry description &optional head)
  (let ((opoint (point))
        desc
       )
    (if (or (not head) lse-completion:hide-leader)
        (lse-indent)
      (lse-fill-in-insert (propertize head 'face 'fringe))
      (indent-to lse-completion:left_margin)
    )
    (lse-fill-in-insert entry "\C-i")
    (indent-to (+ lse-completion:left_margin lse-completion:desc-indent))
    (add-text-properties opoint (point) '(mouse-face lse-face:completion-m))
    (setq desc
      (if (stringp description)
          description
        (if (and (consp description) (stringp (car description)))
            (car description)
          nil
        )
      )
    )
    (when desc
      (lse-fill-in-insert (replace-regexp-in-string "\n *" " " desc))
    )
    ;; 11-Oct-1996
  )
  (lse-fill-in-insert "\n")
)

(defun lse-completion:show_obarray (starter oba dont-sort)
  (let ((completions (all-completions starter oba))
       )
    (lse-completion:show_alist
      starter
      (mapcar
        (function
          (lambda (x)
            (cons x (or (get (intern-soft x oba) 'description) ""))
          )
        )
        completions
      )
      dont-sort
    )
  )
)

(defun lse-completion:< (l r)
  (string< (car l) (car r))
)

(defun lse-completion:show_alist (starter the-completions dont-sort)
  (let ((completions
         (if dont-sort
             the-completions
           (setq lse-completion:sorted t)
           (sort (copy-sequence the-completions) 'lse-completion:<)
         )
        )
        (n lse-completion:index-start)
        head lead next tail
       )
    (setq lse-completion:list '())
    (while (consp completions)
      (setq next        (car completions))
      (setq completions (cdr completions))
      (cond
        ((consp next)
         (setq head (car next))
         (setq tail (cdr next))
        )
        ((symbolp next)
         (setq head
           (format "%s"
             (cond
               ((boundp  next) (symbol-value next))
               ((fboundp next) (symbol-name  next))
             )
           )
         )
         (setq tail next)
        )
        ((stringp next)
         (setq head next)
         (setq tail nil)
        )
      )
      (setq lead (format "%4d" n))
      (lse-completion:show_entry head (lse-completion:entry_desc tail) lead)
      (push head lse-completion:list)
      (setq n (1+ n))
    )
    (setq lse-completion:list (nreverse lse-completion:list))
  )
)

(defun lse-completion:show (starter completions dont-sort)
  (widen)
  (erase-buffer)
  (let ((lse::current-expansion-indent lse-completion:left_margin)
       )
    (if (vectorp completions)
        (lse-completion:show_obarray starter completions dont-sort)
      (if (consp completions)
          (lse-completion:show_alist starter completions dont-sort)
        (error "Argument to lse-complete not an obarray or alist: %s"
               completions
        )
      )
    )
  )
  (setq lse-completion:end_pos (1- (point)))
  (lse-completion:widen)
  (goto-char 1)
; lse-completion:show
)

(defun lse-completion:key_handler (&optional start-position)
  (let (binding helped)
    (lse-completion:goto_match start-position)
    (while t
      (if (equal (current-buffer) lse-completion:buffer)
          (progn
            (setq lse-completion:current (lse-completion:current_item))
            (lse-completion:narrow)
            (lse-completion:highlight)
            (setq lse-completion:last_key (read-key-sequence nil))
            (setq binding
                  (lookup-key (current-local-map) lse-completion:last_key)
            )
            (if (commandp binding)
                (command-execute binding)
              (lse-message "Key %s is undefined"
                       (lse-key-name lse-completion:last_key)
              )
            )
            (if helped                             ; 29-Jun-1994
                (progn
                  (lse-window:restore-temp-hidden)
                  (setq lse-completion:helped nil)
                  (setq helped                nil)
                )
              (setq helped lse-completion:helped)
            )
          )
        (setq lse-completion:so_far nil)
        (throw 'lse-completion:exit nil)
      )
    )
  )
)

(defun lse-complete
         (starter completions
          &optional dont-sort dont-initialize-buffer force start-position
                    case-fold
         )
  (setq lse-completion:saved_wdw_conf (current-window-configuration))
  (setq lse-completion:so_far         starter)
  (setq lse-completion:case-fold      case-fold)
  (unwind-protect
      (progn
        (catch 'lse-completion:exit
          (progn
            (lse-completion:goto_buffer_window
                 (lse-completion:buffer "LSE Completion")
            )
            (let* ((lse-completion:list    completions)
                   (lse-completion:starter (or starter ""))
                   (lse-completion:last    (list lse-completion:starter))
                   (lse-completion:current nil)
                   (echo-keystrokes        0)
                  )
              (if dont-initialize-buffer
                  t
                (lse-completion:show starter completions dont-sort)
              )
              (if (or (not (lse-completion:complete nil t)) force)
                  (lse-completion:key_handler start-position)
              )
            )
          )
        )
      )
    (progn
      (lse-completion:widen)
      (set-window-configuration lse-completion:saved_wdw_conf)
    )
  )
  lse-completion:so_far
; lse-complete
)
