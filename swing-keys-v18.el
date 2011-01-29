;-*- coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work
 
;;;;unix_ms_filename_correspondency swing-keys-v18.el swi_kv18.el
;;;; Copyright (C) 1994 Mag. Christian Tanzer. All rights reserved.
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
;;;;    swing-keys-v18
;;;;
;;;; Purpose
;;;;    Key definitions for swing emacs lse.
;;;;
;;;; Revision Dates
;;;;    26-May-1994 (CT) Creation (of comment)
;;;;    26-May-1994 (CT) Bindings for lse-expand-or-tabulator and lse-open-line added
;;;;    29-May-1994 (CT) require-list moved to swing-default
;;;;-- 

(defun swing-create-swing-keymaps ()
  "Define additional global key maps"
  (swing-create-map        'BLUE-map          "BLUE"      (key-seq BLUE))
  (swing-create-map        'RED-map           "RED"       (key-seq RED))
  (swing-create-map        'GREEN-map         "GREEN"     (key-seq GREEN))
  (swing-create-map        'PINK-map          "PINK"      (key-seq PINK))
  (swing-create-map        'GRAY-map          "GRAY"      (key-seq GRAY))

  (swing-create-map        'BLUE-GOLD-map     "BLUE-GOLD" (key-seq BLUE GOLD))
  (swing-create-sparse-map 'BLUE-TAB-map      "BLUE-TAB"  (key-seq BLUE-TAB))

  ;;(suppress-keymap GOLD-map); would undefine any printable key but for digits

  ;; define F17 as synonym for ESC
  ;;(global-set-key   (key-seq "F17")    'ESC-prefix)
; swing-create-swing-keymaps
) 

;;;++
;;; swing-key-cmd allows short definitions of keys via direct lisp-code (thus
;;; avoiding the need for many tiny functions)
;;; 
;;; Example:
;;;  (global-set-key (key-seq BLUE "LEFT") (key-cmd (lse-indent-rigidly -2)))
;;;--
(defmacro swing-key-cmd (&rest args)
  (`'(lambda () (interactive) (,@ args)))
)
(fset 'key-cmd 'swing-key-cmd)

(defun swing-define-alpha-key (map alpha command)
  ;; Defines a key bound to a alphabetic key.
  ;; Both lower and upper case are defined.
  (let* ((is-composite (or (stringp alpha) (> (length alpha) 1)))
         (leader (if is-composite (substring alpha 0 (1- (length alpha))) ""))
         (chr (if is-composite
                  (substring alpha -1)
                (if (stringp alpha)
                    alpha
                  (char-to-string alpha)
                )
              )
         )
         (upkey   (concat leader (upcase   chr)))
         (downkey (concat leader (downcase chr)))
       )
    (if (lookup-key map upkey)
        ;; minimize number of definitions!
        ;; originally this was unconditionally: (define-key map upkey command)
        (define-key map upkey nil)
      )
    (define-key map downkey command)
  )
; swing-define-alpha-key
) 

(defun gset-alpha-key (alpha command)
  (swing-define-alpha-key global-map alpha command)
; gset-alpha-key
) 

(defun swing-toggle-key-with-gold (key &optional keymap)
  "Interchanges definition of KEY wigh that of GOLD KEY."
  (interactive "kPress key to be toggled ")
  (let* ((map          (or keymap global-map))
         (binding      (lookup-key map key))
         (gold-binding (lookup-key map (key-seq GOLD key)))
        )
    (define-key map key                gold-binding)
    (define-key map (key-seq GOLD key) binding)
    (message "Toggled `%s'" key)
  )
; swing-toggle-key-with-gold
) 

(defun swing-iterate-keys-bound-to-function (binding do)
  "Calls `do' for every key of current keymap bound to `binding'."
  (mapcar do (where-is-internal binding))
; swing-iterate-keys-bound-to-function
) 
;;; example call:
;;;   (swing-iterate-keys-bound-to-function 'self-insert-command 'test)
;;;   (defun test (k)
;;;      (message "Key %s" k)
;;;      (sit-for 2)
;;;   )
;;; or even better
;;;  (swing-iterate-keys-bound-to-function
;;;     'self-insert-command
;;;     (function (lambda (k)
;;;                 (message "Key %s" k)
;;;                 (sit-for 2)
;;;               )
;;;     )
;;;  )
(defun swing-replace-binding-for-all-keys (old-binding new-binding)
  (swing-iterate-keys-bound-to-function
     old-binding (function (lambda (k) (global-set-key k new-binding)))
  )
; swing-replace-binding-for-all-keys
) 

(defun swing-define-key-in-minibuffer-maps       (key binding)
  (define-key minibuffer-local-map                key binding)
  (define-key minibuffer-local-ns-map             key binding)
  (define-key minibuffer-local-completion-map     key binding)
  (define-key minibuffer-local-must-match-map     key binding)
  (define-key read-expression-map                 key binding)
  (if lse-emacs19-p
      t
    (define-key repeat-complex-command-map          key binding)
    (swing-define-key-in-gmhist-maps                key binding)
  )
; swing-define-key-in-minibuffer-maps
) 

(if lse-emacs19-p
    t
  (defun swing-define-key-in-gmhist-maps           (key binding)
    (if (not (featurep 'gmhist))
        t
      (define-key gmhist-map                          key binding)
      (define-key gmhist-completion-map               key binding)
      (define-key gmhist-must-match-map               key binding)
      (define-key gmhist-execute-extended-command-map key binding)
      (define-key gmhist-filename-must-match-map      key binding)
      (define-key gmhist-filename-completion-map      key binding)
    )
  )
)

(defun swing-define-key-in-all-maps (key binding)
  (global-set-key                             key binding)
  (swing-define-key-in-minibuffer-maps        key binding)
; swing-define-key-in-all-maps
) 

(defun swing-redefine-std-emacs-keys ()
  "Redefine keys defined by standard EMACS modes"
  (swing-define-key-in-all-maps        "\C-j"                'lse-tpu:delete-prev-word)
  (swing-define-key-in-minibuffer-maps "\C-e"                'lse-tpu:current-end-of-line)
  (swing-define-key-in-minibuffer-maps "\C-z"                'abort-recursive-edit)

  (if lse-emacs19-p
      (progn
        (swing-define-key-in-minibuffer-maps (key-seq "UP")        'lse-tpu:previous-history-element)
        (swing-define-key-in-minibuffer-maps (key-seq "DOWN")      'lse-tpu:next-history-element)
        (swing-define-key-in-minibuffer-maps (key-seq "CSI-UP")    'lse-tpu:previous-history-element)
        (swing-define-key-in-minibuffer-maps (key-seq "CSI-DOWN")  'lse-tpu:next-history-element)
        (swing-define-key-in-minibuffer-maps (key-seq "PF3")       'exit-minibuffer)
        (swing-define-key-in-minibuffer-maps (key-seq "DO")        'lse-tpu:previous-history-element)
        (swing-define-key-in-minibuffer-maps (key-seq "E1")        'previous-matching-history-element)
        (swing-define-key-in-minibuffer-maps (key-seq GOLD "E1")   'next-matching-history-element)
        (swing-define-key-in-minibuffer-maps (key-seq GOLD "?")    'minibuffer-completion-help)
      )
  )

  (global-set-key  "("                   'lse-insert-parentheses)
  (global-set-key  "["                   'lse-insert-brackets)
  (global-set-key  "{"                   'lse-insert-braces)
  (global-set-key  ","                   'lse-insert-comma)
  (global-set-key  ";"                   'lse-insert-semicolon)
  (global-set-key  "\""                  'lse-insert-dquotes)
  
  (global-set-key  "\C-a"                'overwrite-mode)
  (global-set-key  "\C-d"                'dabbrev-expand)
  (global-set-key  "\C-e"                'lse-expand)
  (global-set-key  "\C-i"                'lse-expand-or-tabulator)
  (global-set-key  "\C-k"                'lse-kill-fill-in)
  (global-set-key  "\C-n"                'lse-goto-next-fill-in)
  (global-set-key  "\C-o"                'lse-describe-fill-in)
  (global-set-key  "\C-p"                'lse-goto-prev-fill-in)
  (global-set-key  "\C-r"                'lse-replace-fill-in)
  (global-set-key  "\C-s"                'lse-replicate-fill-in)
  (global-set-key  "\C-u"                'lse-tpu:delete-head-of-line)
  (global-set-key  "\C-z"                'keyboard-quit)
  (global-set-key  "\177"                'lse-tpu:delete-prev-char)
  (global-set-key  "\C-@"                'universal-argument)
  (local-unset-key "\C-i")
  (local-unset-key "\177")

  (global-set-key  (key-seq "DO")        'repeat-complex-command)
  (global-set-key  (key-seq "KP1")       'lse-tpu:goto-word-head)
  (global-set-key  (key-seq "KP2")       'lse-tpu:goto-bs-word-tail)
  (global-set-key  (key-seq "KP3")       'lse-tpu:end-of-line)
  (global-set-key  (key-seq "KP6")       'lse-tpu:cut-region)
  (global-set-key  (key-seq "KP7")       'lse-tpu:page)
  (global-set-key  (key-seq "KP9")       'lse-tpu:cut-append-region)
  (global-set-key  (key-seq "KP-")       'lse-tpu:delete-next-word)
  (global-set-key  (key-seq "KP,")       'lse-tpu:delete-next-char)
  (global-set-key  (key-seq "ENTER")     'lse-tpu:forward-char)
  (global-set-key  (key-seq "PF4")       'lse-tpu:delete-next-line)
  (global-set-key  (key-seq "E1")        'tags-loop-continue)
  (global-set-key  (key-seq "E2")        'lse-tpu:next-beginning-of-line)
  (global-set-key  (key-seq "E3")        'lse-tpu:goto-prev-bs-word-head); swing-backward-word-begin
  (global-set-key  (key-seq "E5")        (key-cmd (lse-previous-screen 2)))
  (global-set-key  (key-seq "E6")        (key-cmd (lse-next-screen 2)))
  
  (if (not (featurep 'gmhist))
      t
    (define-key gmhist-map (key-seq "PF3") 'exit-minibuffer)
  )
; swing-redefine-std-emacs-keys
) 

(defun swing-replace-std-emacs-bindings ()
  (fset 'Replace-Binding 'swing-replace-binding-for-all-keys)

  (Replace-Binding 'switch-to-buffer          'lse-goto-buffer)
  (Replace-Binding 'switch-to-buffer-other-window
                                              'lse-goto-buffer-other-window)
  (Replace-Binding 'kill-buffer               'lse-kill-buffer)
  (Replace-Binding 'revert-buffer             'lse-revert-buffer)
  (Replace-Binding 'find-file                 'lse-visit-file)
  (Replace-Binding 'find-file-other-window    'lse-visit-file-other-window)
  (Replace-Binding 'find-alternate-file       'lse-visit-alternate-file)
  (Replace-Binding 'set-visited-file-name     'lse-change-output-file)
  (Replace-Binding 'toggle-read-only          'lse-set-buffer-nowrite)
  (Replace-Binding 'list-buffers              'lse-show-buffers)

  (Replace-Binding 'delete-char               'lse-tpu:delete-next-char)

  (Replace-Binding 'delete-window             'lse-delete-window)
  (Replace-Binding 'delete-other-windows      'lse-delete-other-windows)
  (Replace-Binding 'split-window              'lse-split-window)
  (Replace-Binding 'split-window-vertically   'lse-split-window)
  (Replace-Binding 'split-window-horizontally 'lse-split-window-horizontally)

  (fmakunbound 'Replace-Binding)
; swing-replace-std-emacs-bindings
) 

(defun swing-define-gold-keys ()
  "Define Gold-Keys and related keys"

  (global-set-key (key-seq GOLD "(")    'self-insert-command)
  (global-set-key (key-seq GOLD ")")    'backward-sexp)
  (global-set-key (key-seq GOLD "[")    'self-insert-command)
  (global-set-key (key-seq GOLD "]")    'backward-sexp)
  (global-set-key (key-seq GOLD "{")    'self-insert-command)
  (global-set-key (key-seq GOLD "}")    'backward-sexp)
  (global-set-key (key-seq GOLD ",")    'self-insert-command)
  (global-set-key (key-seq GOLD ".")    'self-insert-command)
  (global-set-key (key-seq GOLD ";")    'self-insert-command)
  (global-set-key (key-seq GOLD "\"")   'self-insert-command)
  (global-set-key (key-seq GOLD "'")    'lse-insert-squotes)
  (global-set-key (key-seq GOLD "/")    'lse-align-and-up)
  (global-set-key (key-seq GOLD "=")    'lse-split-window)
  (global-set-key (key-seq GOLD ">")    'lse-tpu:pan-right)
  (global-set-key (key-seq GOLD "<")    'lse-tpu:pan-left)
  (global-set-key (key-seq GOLD "?")    'describe-key-briefly)
  (global-set-key (key-seq GOLD "*")    'lse-show-buffers)
  (global-set-key (key-seq GOLD " ")    'lse-align-to-next-word)
  (global-set-key (key-seq GOLD "~")    'vt-narrow)
  (global-set-key (key-seq GOLD "`")    'vt-wide)

  (gset-alpha-key (key-seq GOLD "B")    'lse-goto-last-mark-global)
  (gset-alpha-key (key-seq GOLD "C")    'lse-tpu:capitalize-weakly)
  (gset-alpha-key (key-seq GOLD "D")    'lse-insert-dd-mmm-yyyy+blank)
  (gset-alpha-key (key-seq GOLD "E")    'undefined)
  (gset-alpha-key (key-seq GOLD "F")    'lse-goto-buffer)
  (gset-alpha-key (key-seq GOLD "G")    'lse-goto-mark-and-pop-global)
  (gset-alpha-key (key-seq GOLD "H")    'lse-goto-home-mark-global)
  (gset-alpha-key (key-seq GOLD "I")    'lse-visit-file)
  (gset-alpha-key (key-seq GOLD "K")    'undefined)
  (gset-alpha-key (key-seq GOLD "L")    'lse-tpu:change-case-lower)
  (gset-alpha-key (key-seq GOLD "M")    'lse-push-mark-global)
  (gset-alpha-key (key-seq GOLD "N")    'lse-goto-next-buffer)
  (gset-alpha-key (key-seq GOLD "O")    'lse-change-output-file)
  (gset-alpha-key (key-seq GOLD "P")    'lse-insert-buffer)
  (gset-alpha-key (key-seq GOLD "Q")    'lse-insert-buffer-name)
  (gset-alpha-key (key-seq GOLD "T")    'lse-toggle-mark-global)
  (gset-alpha-key (key-seq GOLD "U")    'lse-tpu:change-case-upper)
  (gset-alpha-key (key-seq GOLD "V")    'lse-align-to-next-word-and-up)
  (gset-alpha-key (key-seq GOLD "W")    'lse-write-buffer)
  (gset-alpha-key (key-seq GOLD "X")    'suspend-emacs)
  (gset-alpha-key (key-seq GOLD "Y")    'eval-last-sexp)
  (gset-alpha-key (key-seq GOLD "Z")    'lse-insert-user-initials)

  (global-set-key (key-seq GOLD "\C-e") 'lse-unexpand-fill-in)
  (global-set-key (key-seq GOLD "\C-i") 'lse-indent-line)
  (global-set-key (key-seq GOLD "\C-k") 'lse-unkill-fill-in)
  (global-set-key (key-seq GOLD "\C-m") 'newline)
  (global-set-key (key-seq GOLD "\C-n") 'lse-goto-last-position)
  (global-set-key (key-seq GOLD "\C-o") 'lse-help-fill-in)
  (global-set-key (key-seq GOLD "\C-p") 'lse-goto-last-position)
  (global-set-key (key-seq GOLD "\C-r") 'lse-unreplace-fill-in)
  (global-set-key (key-seq GOLD "\C-s") 'lse-replicate-fill-in-by-older)

  (global-set-key (key-seq GOLD "KP0")  'lse-open-line)
  (global-set-key (key-seq GOLD "KP2")  'lse-tpu:delete-tail-of-line)
  (global-set-key (key-seq GOLD "KP6")  'lse-tpu:paste-region)
  (global-set-key (key-seq GOLD "KP7")  'lse-command:do)
  (global-set-key (key-seq GOLD "KP8")  'lse-tpu:replace)
  (global-set-key (key-seq GOLD "KP9")   nil)
  (global-set-key (key-seq GOLD "KP-")  'lse-tpu:undelete-word)
  (global-set-key (key-seq GOLD "KP,")  'lse-tpu:undelete-char)
  (global-set-key (key-seq GOLD "ENTER")'lse-tpu:replace-all)
  (global-set-key (key-seq GOLD "PF1")  'lse-ring-bell)
  (global-set-key (key-seq GOLD "PF4")  'lse-tpu:undelete-line)

  (global-set-key (key-seq GOLD "RIGHT")(key-cmd (lse-indent-rigidly  2)))
  (global-set-key (key-seq GOLD "LEFT") (key-cmd (lse-indent-rigidly -2)))
  (global-set-key (key-seq GOLD "UP")   'lse-previous-window)
  (global-set-key (key-seq GOLD "DOWN") 'lse-next-window)
  (global-set-key (key-seq GOLD "CSI-RIGHT")(key-cmd (lse-indent-rigidly  2)))
  (global-set-key (key-seq GOLD "CSI-LEFT") (key-cmd (lse-indent-rigidly -2)))
  (global-set-key (key-seq GOLD "CSI-UP")   'lse-previous-window)
  (global-set-key (key-seq GOLD "CSI-DOWN") 'lse-next-window)

  (global-set-key (key-seq GOLD "E1")   'tags-search)
  (global-set-key (key-seq GOLD "E5")   'lse-previous-screen)
  (global-set-key (key-seq GOLD "E6")   'lse-next-screen)

  (global-set-key (key-seq GOLD "DO")   'undo)
; swing-define-gold-keys
) 

(defun swing-define-blue-keys ()
  "Define BLUE-keys"
  (global-set-key (key-seq BLUE "PF3")      'isearch-forward)
  (global-set-key (key-seq BLUE "PF4")      'lse-tpu:duplicate-previous-line)
  (global-set-key (key-seq BLUE "KP0")      'goto-line)
  (global-set-key (key-seq BLUE "KP6")      'lse-tpu:copy-region)
  (global-set-key (key-seq BLUE "KP7")      'lse-show-position)
  (global-set-key (key-seq BLUE "KP8")      'tags-query-replace)
  (global-set-key (key-seq BLUE "KP9")      'lse-tpu:copy-append-region)
  (global-set-key (key-seq BLUE "KP-")      'lse-tpu:duplicate-previous-word)
  (global-set-key (key-seq BLUE "KP,")      'lse-tpu:duplicate-previous-char)
  (global-set-key (key-seq BLUE "KP.")      'exchange-point-and-mark)
  (global-set-key (key-seq BLUE GOLD "PF4") 'lse-tpu:duplicate-word-in-previous-line)
  (global-set-key (key-seq BLUE GOLD "KP7") 'line-to-top-of-window)
  (global-set-key (key-seq BLUE GOLD "KP8") 'lse-tpu:replace-all)
  (global-set-key (key-seq BLUE GOLD "KP-") 'lse-tpu:delete-next-word-append)
  (global-set-key (key-seq BLUE GOLD "KP,") 'lse-tpu:delete-next-char-append)
  (global-set-key (key-seq BLUE GOLD "KP.") 'lse-blink-select-mark)

  (global-set-key (key-seq BLUE "E1")       'find-tag)
  (global-set-key (key-seq BLUE "E3")       'lse-tpu:duplicate-previous-bs-word)
  (global-set-key (key-seq BLUE "E4")       'mark-section-wisely)
  (global-set-key (key-seq BLUE "E5")       'lse-scroll-other-window-forw)
  (global-set-key (key-seq BLUE "E6")       'lse-scroll-other-window-back)

  (global-set-key (key-seq BLUE "UP")             'lse-previous-window)
  (global-set-key (key-seq BLUE "CSI-UP")         'lse-previous-window)
  (global-set-key (key-seq BLUE "DOWN")           'lse-next-window)
  (global-set-key (key-seq BLUE "CSI-DOWN")       'lse-next-window)
  (global-set-key (key-seq BLUE "RIGHT")          (key-cmd (lse-indent-rigidly 6)))
  (global-set-key (key-seq BLUE "LEFT")           (key-cmd (lse-indent-rigidly -6)))
  (global-set-key (key-seq BLUE "CSI-RIGHT")      (key-cmd (lse-indent-rigidly 6)))
  (global-set-key (key-seq BLUE "CSI-LEFT")       (key-cmd (lse-indent-rigidly -6)))
  (global-set-key (key-seq BLUE GOLD "RIGHT")     'lse-tpu:pan-right)
  (global-set-key (key-seq BLUE GOLD "CSI-RIGHT") 'lse-tpu:pan-right)
  (global-set-key (key-seq BLUE GOLD "LEFT")      'lse-tpu:pan-left)
  (global-set-key (key-seq BLUE GOLD "CSI-LEFT")  'lse-tpu:pan-left)
  (global-set-key (key-seq BLUE GOLD "UP")        'enlarge-window)
  (global-set-key (key-seq BLUE GOLD "CSI-UP")    'enlarge-window)
  (global-set-key (key-seq BLUE GOLD "DOWN")      'shrink-window)
  (global-set-key (key-seq BLUE GOLD "CSI-DOWN")  'shrink-window)
  (global-set-key (key-seq BLUE "HELP")       'describe-key-briefly)
  (global-set-key (key-seq BLUE GOLD "HELP")  'help-command)
  (if lse-emacs19-p
      (global-set-key (key-seq BLUE "DO")     'execute-extended-command)
    (global-set-key (key-seq BLUE "DO")       'gmhist-execute-extended-command)
  )

  (gset-alpha-key (key-seq BLUE "B")      'lse-goto-last-mark-window)
  (gset-alpha-key (key-seq BLUE "C")      'lse-tpu:capitalize-strongly)
  (gset-alpha-key (key-seq BLUE "D")      'lse-insert-dd-mm-yyyy+blank)
  (gset-alpha-key (key-seq BLUE GOLD "E") 'lse-kill-buffer)
  (gset-alpha-key (key-seq BLUE "F")      'lse-goto-buffer+maybe-create)
  (gset-alpha-key (key-seq BLUE GOLD "F") 'lse-goto-buffer-other-window)
  (gset-alpha-key (key-seq BLUE "G")      'lse-goto-mark-and-pop-window)
  (gset-alpha-key (key-seq BLUE "H")      'lse-goto-home-mark-window)
  (gset-alpha-key (key-seq BLUE GOLD "H") 'lse-set-home-mark-window)
  (gset-alpha-key (key-seq BLUE "I")      'lse-visit-file-other-window)
  (gset-alpha-key (key-seq BLUE GOLD "I") 'lse-insert-file)
  (gset-alpha-key (key-seq BLUE "L")      'lse-learn-key)
  (gset-alpha-key (key-seq BLUE "M")      'lse-push-mark-window)
  (gset-alpha-key (key-seq BLUE "N")      'lse-goto-prev-buffer)
  (gset-alpha-key (key-seq BLUE "O")      'lse-change-output-file)
  (gset-alpha-key (key-seq BLUE "P")      'lse-insert-buffer)
  (gset-alpha-key (key-seq BLUE "S")      'cd)
  (gset-alpha-key (key-seq BLUE "T")      'lse-toggle-mark-window)
  (gset-alpha-key (key-seq BLUE "V")      'lse-align-to-previous-word-and-down)
  (gset-alpha-key (key-seq BLUE "W")      'lse-write-current-buffer)
  (gset-alpha-key (key-seq BLUE GOLD "W") 'save-some-buffers)
  (gset-alpha-key (key-seq BLUE "X")      'subprocess-command)
  (gset-alpha-key (key-seq BLUE "Z")      'lse-insert-user-full-name)
  (gset-alpha-key (key-seq BLUE GOLD "Z") 'lse-insert-user-name)

  (global-set-key (key-seq BLUE "/")      'lse-align-and-down)
  (global-set-key (key-seq BLUE "=")      'lse-split-window)
  (global-set-key (key-seq BLUE GOLD "=") 'lse-delete-window)
  (global-set-key (key-seq BLUE GOLD "-") 'lse-delete-other-windows)
  (global-set-key (key-seq BLUE      "~") 'lse-tpu:trim-line-ends)
  (global-set-key (key-seq BLUE GOLD "~") 'lse-fill-range)
  (global-set-key (key-seq BLUE "?")      'describe-key)
  (global-set-key (key-seq BLUE "*")      'lse-show-all-buffers)
  (global-set-key (key-seq BLUE "^")      'lse-push-window-configuration)
  (global-set-key (key-seq BLUE GOLD "^") 'lse-pop+restore-window-configuration)
  (global-set-key (key-seq BLUE " ")      'lse-align-to-previous-word)
  (global-set-key (key-seq BLUE GOLD " ") 'set-fill-column)
  (global-set-key (key-seq BLUE "(")      'lse-select-paren-range)
  (global-set-key (key-seq BLUE "[")      'lse-select-bracket-range)
  (global-set-key (key-seq BLUE "{")      'lse-select-brace-range)
  (global-set-key (key-seq BLUE GOLD "(") 'lse-remove-parentheses)
  (global-set-key (key-seq BLUE GOLD "[") 'lse-remove-brackets)
  (global-set-key (key-seq BLUE GOLD "{") 'lse-remove-braces)

  (global-set-key (key-seq BLUE "-")      'negative-argument)
  (let ((i ?0))
    (while (<= i ?9)
      (global-set-key (key-seq BLUE (char-to-string i)) 'digit-argument)
      (setq i (1+ i))
    )
  )

  (global-set-key (key-seq BLUE "\C-e")      'lse-reexpand-fill-in)
  (global-set-key (key-seq BLUE GOLD "\C-i") 'auto-fill-mode)
  (global-set-key (key-seq BLUE "\C-j")      'lse-tpu:delete-prev-word-append)
  (global-set-key (key-seq BLUE GOLD "\C-k") 'lse-kill-all-optional-fill-ins)
  (global-set-key (key-seq BLUE      "\C-m") 'lse-split-line)
  (global-set-key (key-seq BLUE GOLD "\C-m") 'lse-tpu:toggle-newline-and-indent)
  (global-set-key (key-seq BLUE "\C-r")      'lse-rereplace-fill-in)
  (global-set-key (key-seq BLUE GOLD "\C-r") (key-cmd (progn (setq lse_replaced_fill-in nil) (lse-tpu:update-mode-line))))
  (global-set-key (key-seq BLUE "\C-w")      'lse-set-buffer-nowrite)
  (global-set-key (key-seq BLUE "\177")      'lse-tpu:delete-prev-char-append)
; swing-define-blue-keys
) 

(defun swing-define-red-keys ()
  (gset-alpha-key (key-seq RED  "a")         'beginning-of-defun)
  (gset-alpha-key (key-seq RED  "b")         'backward-sexp)
  (gset-alpha-key (key-seq RED  GOLD "b")    (key-cmd (backward-sexp -1)))
  (gset-alpha-key (key-seq RED  "d")         'down-list)
  (gset-alpha-key (key-seq RED  "e")         'end-of-defun)
  (gset-alpha-key (key-seq RED  "f")         'forward-sexp)
  (gset-alpha-key (key-seq RED  GOLD "f")    (key-cmd (forward-sexp -1)))
  (gset-alpha-key (key-seq RED  "h")         'backward-up-list)
  (gset-alpha-key (key-seq RED  GOLD "h")    (key-cmd (backward-up-list -1)))
  (gset-alpha-key (key-seq RED  "n")         'forward-list)
  (gset-alpha-key (key-seq RED  "p")         'backward-list)
  (gset-alpha-key (key-seq RED  "t")         'transpose-sexps)
  (gset-alpha-key (key-seq RED  "u")         'up-list)
  (gset-alpha-key (key-seq RED  "y")         'eval-last-sexp)
  (global-set-key (key-seq RED  "E4")        'mark-sexp)
  (global-set-key (key-seq RED  "KP.")       'mark-sexp)
  (global-set-key (key-seq RED  GOLD "E4")   'mark-defun)
  (global-set-key (key-seq RED  GOLD "KP.")  'mark-defun)
  (global-set-key (key-seq RED  "\C-i")      'indent-sexp)
  (global-set-key (key-seq RED  "\C-k")      'kill-sexp)
  (global-set-key (key-seq RED  GOLD "\C-k") 'yank)
; swing-define-red-keys
) 

(defun swing-define-blue-tab-keys ()
  "Define keys of BLUE-TAB keymap"
  (global-set-key (key-seq BLUE-TAB " ")    'just-one-space)
  (gset-alpha-key (key-seq BLUE-TAB "t")    'lse-tpu:toggle-newline-and-indent)
  (global-set-key (key-seq BLUE-TAB "\C-i") 'delete-indentation)
  (global-set-key (key-seq BLUE-TAB "\C-m") 'delete-blank-lines)

  (global-set-key (key-seq BLUE-TAB "KP0")  'split-line)
  (global-set-key (key-seq BLUE-TAB "KP3")  'lse-tpu:trim-line-end)
  (global-set-key (key-seq BLUE-TAB "E2")   'back-to-indentation)
; swing-define-blue-tab-keys
) 

(swing-replace-std-emacs-bindings)
(swing-create-swing-keymaps)
(swing-redefine-std-emacs-keys)
(swing-define-gold-keys)
(swing-define-blue-keys)
(swing-define-red-keys)
(swing-define-blue-tab-keys)
;;;(load-library "vmsproc")
;;(setq debug-on-error t)
;;(setq debug-on-error nil)

; (gmhist-make-magic 'find-tag           'regexp-history)
; (gmhist-make-magic 'tags-search        'regexp-history)
; (gmhist-make-magic 'tags-query-replace 'regexp-history)
