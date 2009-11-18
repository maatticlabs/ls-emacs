;-*- unibyte: t; coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work

;;;;unix_ms_filename_correspondency lse-menu:el lse-menu:el
;;;; Copyright (C) 1996-2009 Mag. Christian Tanzer. All rights reserved.
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

;;;;
;;;;++
;;;; Name
;;;;    lse-menu
;;;;
;;;; Purpose
;;;;    Provide menu keymaps and other menu functionality for LS-Emacs
;;;;
;;;; Revision Dates
;;;;    16-Oct-1996 (CT) Creation
;;;;    18-Dec-1997 (CT) Extensions to help menu added
;;;;    29-Dec-1997 (CT) `lse-version' added
;;;;    28-Dec-1999 (CT) Bindings for expansion functions added
;;;;     1-Jan-2000 (CT) Bindings for lse-fill-in-mark functions added
;;;;     1-Jan-2000 (CT) Binding for `repeat' added to standard edit-menu
;;;;     3-Jan-2000 (CT) `lse-menu:add-menubar-index' added
;;;;     9-Jan-2000 (CT) `Options' added
;;;;    28-Mar-2007 (CT) `lse-menu:*-menu-bar` added
;;;;     2-Oct-2007 (CT) Add `Show Emacs Version` to Emacs 22 Help Menu
;;;;     4-Oct-2007 (CT) `toggle-editing-direction` removed
;;;;     5-Oct-2007 (CT) Search direction removed
;;;;     9-Oct-2007 (CT) `lse-tpu:toggle-regexp` replaced by
;;;;                     `lse-tpu:change-search-mode`
;;;;    13-Oct-2007 (CT) `lse-byte-compile` commands added to `emacs-lisp` menu
;;;;    13-Oct-2007 (CT) Disabled standard byte-compile entries in
;;;;                     `emacs-lisp` menu for LS-Emacs files
;;;;    29-Sep-2008 (CT) `lse-key:toggle-tab` added
;;;;    18-Nov-2009 (CT) `lse-fill-in-marks:goto-open-head` and
;;;;                     `lse-fill-in-marks:goto-open-tail` added
;;;;    ««revision-date»»···
;;;;--

(provide 'lse-menu)

(eval-when-compile
  (require 'lse-byte-compile)
  (require 'lse-flat-fill-in)
  (require 'lse-interactive)
  (require 'lse-tpu-keys)
  (require 'imenu)
)

(defvar lse-menu:lse-menu (make-sparse-keymap "LSE")
  "Menu keymap for fill-in commands of LS-Emacs"
)

(defvar lse-menu:fill-in (make-sparse-keymap "Fill-In")
  "Menu keymap for fill-in commands of LS-Emacs"
)

;;; (setq   lse-menu:fill-in (make-sparse-keymap "Fill-In"))
(add-to-list 'menu-bar-final-items 'fill-in)

(define-key global-map        [menu-bar fill-in]
  (cons "LSE" lse-menu:lse-menu)
)
(define-key lse-menu:lse-menu [show-lse-language]
              '("Show language" . lse-show-language)
)
(put 'lse-show-language 'menu-enable 'lse-language:name)

(define-key lse-flat-fill-in:keymap [down-mouse-3] lse-menu:fill-in)

(define-key lse-menu:lse-menu [fill-in] (cons "Fill-In" lse-menu:fill-in))

(define-key lse-menu:fill-in [prev-mark-tail-fi]
            '("Prev tail" . lse-fill-in-marks:goto-prev-tail)
)
(put 'lse-fill-in-marks:goto-prev-tail 'menu-enable 'lse-language:name)

(define-key lse-menu:fill-in [prev-mark-head-fi]
            '("Prev head" . lse-fill-in-marks:goto-prev-head)
)
(put 'lse-fill-in-marks:goto-prev-head 'menu-enable 'lse-language:name)

(define-key lse-menu:fill-in [next-mark-tail-fi]
            '("Next tail" . lse-fill-in-marks:goto-next-tail)
)
(put 'lse-fill-in-marks:goto-next-tail 'menu-enable 'lse-language:name)

(define-key lse-menu:fill-in [next-mark-head-fi]
            '("Next head" . lse-fill-in-marks:goto-next-head)
)
(put 'lse-fill-in-marks:goto-next-head 'menu-enable 'lse-language:name)

(define-key lse-menu:fill-in [prev-exp]
            '("Previous expansion" . lse-goto-prev-expansion)
)
(put 'lse-goto-prev-expansion 'menu-enable 'lse-language:name)

(define-key lse-menu:fill-in [next-exp]
            '("Next expansion" . lse-goto-next-expansion)
)
(put 'lse-goto-next-expansion 'menu-enable 'lse-language:name)

(define-key lse-menu:fill-in [parent-fi]
            '("Parent" . lse-goto-parent-expansion-head)
)
(put 'lse-goto-parent-expansion-head 'menu-enable 'lse-language:name)

(define-key lse-menu:fill-in [separator-fi-move-2]
            '("--")
)

(define-key lse-menu:fill-in [tail-replacement]
            '("Tail of replacement" . lse-fill-in-marks:goto-open-tail)
)
(put 'lse-fill-in-marks:goto-open-tail 'menu-enable 'lse_replaced_fill-in)

(define-key lse-menu:fill-in [head-replacement]
            '("Head of replacement" . lse-fill-in-marks:goto-open-head)
)
(put 'lse-fill-in-marks:goto-open-head 'menu-enable 'lse_replaced_fill-in)

(define-key lse-menu:fill-in [separator-fi-move-1]
            '("--")
)

(define-key lse-menu:fill-in [last-pos-fi]
            '("Last Position" . lse-goto-last-position)
)
(put 'lse-goto-last-position 'menu-enable 'lse-language:name)

(define-key lse-menu:fill-in [prev-fi]
            '("Previous" . lse-goto-prev-fill-in)
)
(put 'lse-goto-prev-fill-in 'menu-enable 'lse-language:name)

(define-key lse-menu:fill-in [next-fi]
            '("Next" . lse-goto-next-fill-in)
)
(put 'lse-goto-next-fill-in 'menu-enable 'lse-language:name)

(define-key lse-menu:fill-in [separator-fi-move]
            '("--")
)

(define-key lse-menu:fill-in [kill-fi]
            '("Kill" . lse-kill-fill-in)
)
(put 'lse-kill-fill-in 'menu-enable 'lse-language:name)

(define-key lse-menu:fill-in [unkill-fi]
            '("Un-Kill" . lse-unkill-fill-in)
)
(put 'lse-unkill-fill-in 'menu-enable 'lse-language:name)

(define-key lse-menu:fill-in [separator-fi-kill]
            '("--")
)

(define-key lse-menu:fill-in [reexpand-fi]
            '("Re-Expand" . lse-reexpand-fill-in)
)
(put 'lse-reexpand-fill-in 'menu-enable 'lse-language:name)

(define-key lse-menu:fill-in [unexpand-fi]
            '("Un-Expand" . lse-unexpand-fill-in)
)
(put 'lse-unexpand-fill-in 'menu-enable 'lse-language:name)

(define-key lse-menu:fill-in [expand-fi]
            '("Expand" . lse-expand)
)
(put 'lse-expand 'menu-enable '(lse_inside_fill-in))

(define-key lse-menu:fill-in [separator-fi-expand]
            '("--")
)

(define-key lse-menu:fill-in [replicate-fi]
            '("Replicate" . lse-replicate-fill-in)
)
(put 'lse-replicate-fill-in 'menu-enable '(lse_inside_fill-in))

(define-key lse-menu:fill-in [replicate-menu-fi]
            '("Replicate Menu" . lse-replicate-menu)
)
(put 'lse-replicate-menu 'menu-enable '(lse_inside_fill-in))

(define-key lse-menu:fill-in [separator-fi-replicate]
            '("--")
)

(define-key lse-menu:fill-in [help-fi]
            '("Help" . lse-help-fill-in)
)
(put 'lse-help-fill-in 'menu-enable '(lse_inside_fill-in))

(define-key lse-menu:fill-in [describe-fi]
            '("Describe" "bla bla" . lse-describe-fill-in)
)
(put 'lse-describe-fill-in 'menu-enable '(lse_inside_fill-in))

(if (fboundp 'menu-bar-make-toggle)
    ;; menu-bar-make-toggle should be provided by the standard library
    ;; menu-bar.el
    (progn
      (defvar lse-menu:options         (make-sparse-keymap "Options"))
      (defvar lse-menu:options:search  (make-sparse-keymap "Search Options"))
      (defvar lse-menu:options:editing (make-sparse-keymap "Editing Options"))
      (define-key lse-menu:lse-menu [options]
        (cons "Options" lse-menu:options)
      )
      (define-key lse-menu:options [search-options]
        (cons "Search" lse-menu:options:search)
      )
      (define-key lse-menu:options [editing-options]
        (cons "Editing" lse-menu:options:editing)
      )
      (define-key lse-menu:options:search [change-search-mode]
         '("Search mode" . lse-tpu:change-search-mode)
      )
      (define-key lse-menu:options:search [case-replace]
        (menu-bar-make-toggle toggle-case-replace case-replace
                        "Case folding in replacements"
                        "Case folding in replacements %s"
                        "Case insensitive replacements"
        )
      )
      (define-key lse-menu:options:search [case-fold-search]
        (menu-bar-make-toggle toggle-case-fold-search case-fold-search
                        "Case folding in searches"
                        "Case folding in searches %s"
                        "Case insensitive searches"
        )
      )
      (define-key lse-menu:options:editing [rectangular-mode]
        (menu-bar-make-toggle toggle-rectangular-mode
                              lse-tpu:rectangular-p
                              "Rectangular cut and paste"
                              "Rectangular cut and paste %s"
                              "Use rectangle mode for cut and paste"
                              (lse-tpu:toggle-rectangle)
        )
      )
      (define-key lse-menu:options:editing [overwrite-mode]
        (menu-bar-make-toggle toggle-overwrite-mode
                              overwrite-mode
                              "Overwrite mode" "Overwrite mode %s"
                              "Overwrite mode"
                              (lse-tpu:toggle-overwrite-mode)
        )
      )
      (define-key lse-menu:options:editing [newline-and-indent]
        (menu-bar-make-toggle toggle-newline-and-indent
                              lse-tpu:newline-and-indent-p
                              "Indent after newline" "Indent after newline %s"
                              "Indent after newline"
                              (lse-tpu:toggle-newline-and-indent)
        )
      )
      (define-key lse-menu:options:editing [lse-split-line]
        (menu-bar-make-toggle toggle-lse-split-line
                              lse-key:toggle-tab-p
                              "Tab binds to lse-tabulator"
                              "Tab binds to lse-tabulator"
                              "Tab binds to lse-tabulator"
                              (lse-key:toggle-tab)
        )
      )
      (define-key lse-menu:options:editing [lse-tabulator]
        (menu-bar-make-toggle toggle-lse-tabulator
                              lse-split-line:old-key
                              "Return binds to newline"
                              "Return binds to newline %s"
                              "Return binds to newline"
                              (lse-toggle-lse-split-line)
        )
      )
      (define-key lse-menu:options:editing [case-fold-search]
        (menu-bar-make-toggle toggle-dabbrev-case-fold-search
                              dabbrev-case-fold-search
                              "Ignore case for dabbrev's"
                              "Ignore case for dabbrev's %s"
                              "Ignore case for dabbrev's"
        )
      )
      (define-key lse-menu:options:editing [auto-fill-mode]
        (menu-bar-make-toggle toggle-auto-fill-mode
                              auto-fill-function
                              "Auto Fill (word wrap)"
                              "Auto Fill (word wrap) %s"
                              "Auto fill mode"
          (if auto-fill-function
              (auto-fill-mode 0)
            (auto-fill-mode 1)
          )
        )
      )
    )
)

;;; extend standard help menu
(defvar menu-bar-lse-help-menu (make-sparse-keymap "LSE"))

(define-key menu-bar-lse-help-menu [show-lse-version]
  '("Show Version" . lse-version)
); 29-Dec-1997

(define-key menu-bar-lse-help-menu [show-keys-matching]
  '("Show keys matching" . lse-tpu-keys:show-keys-matching)
); 28-Dec-1997

(define-key menu-bar-lse-help-menu [show-list-sexp-keys]
  '("Show list/sexp keys" . lse-tpu-keys:show-list-sexp-keys)
)

(define-key menu-bar-lse-help-menu [show-list-keys]
  '("Show list keys" . lse-tpu-keys:show-list-keys)
)

(define-key menu-bar-lse-help-menu [show-sexp-keys]
  '("Show sexp keys" . lse-tpu-keys:show-sexp-keys)
)

(define-key menu-bar-lse-help-menu [show-fill-in-keys]
  '("Show fill-in keys" . lse-tpu-keys:show-fill-in-keys)
)

(define-key menu-bar-help-menu [lse-help-menu]
  (cons "LSE" menu-bar-lse-help-menu)
)

(if lse-emacs22-p ;  2-Oct-2007
    (define-key menu-bar-help-menu [lse-show-emacs-version]
      '("Show Emacs Version" . emacs-version)
    )
)

(define-key menu-bar-edit-menu [lse-repeat]
  '("Repeat command" . repeat)
)

;;; 13-Oct-2007
(define-key emacs-lisp-mode-map [menu-bar emacs-lisp separator-LSE] '("--"))

(define-key emacs-lisp-mode-map [menu-bar emacs-lisp lse-byte-compile-all]
  '("Byte-compile All LS-Emacs Files" . lse-byte-compile:all)
)

(define-key emacs-lisp-mode-map [menu-bar emacs-lisp lse-byte-compile-current]
  '("Byte-compile Current LS-Emacs File" . lse-byte-compile:current)
)

(put 'lse-byte-compile:current
  'menu-enable '(lse-byte-compile:is-lse-file-p)
)
(put 'emacs-lisp-byte-compile
  'menu-enable '(not (lse-byte-compile:is-lse-file-p))
)
(put 'emacs-lisp-byte-compile-and-load
  'menu-enable '(not (lse-byte-compile:is-lse-file-p))
)
(put 'byte-recompile-directory
  'menu-enable '(not (lse-byte-compile:is-lse-file-p))
)

;;;  3-Jan-2000
(defun lse-menu:add-menubar-index ()
  (interactive)
  (if (fboundp 'imenu-add-menubar-index) (imenu-add-menubar-index))
; lse-menu:add-menubar-index
)

;;; 28-Mar-2007
(defvar lse-menubar-flag t)

;;; 28-Mar-2007
(defun lse-menu:hide-menu-bar ()
  "Disable menu bar on all frames."
  (interactive)
  (setq lse-menubar-flag nil)
  (menu-bar-mode lse-menubar-flag)
; lse-menu:hide-menu-bar
)

;;; 28-Mar-2007
(defun lse-menu:show-menu-bar ()
  "Enable menu bar on all frames."
  (interactive)
  (setq lse-menubar-flag t)
  (menu-bar-mode lse-menubar-flag)
; lse-menu:show-menu-bar
)

;;; 28-Mar-2007
(defun lse-menu:toggle-menu-bar ()
  "Toggle menu bar on all frames."
  (interactive)
  (setq lse-menubar-flag (not lse-menubar-flag))
  (menu-bar-mode lse-menubar-flag)
; lse-menu:toggle-menu-bar
)

;;; __END__ lse-menu.el
