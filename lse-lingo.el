;-*- coding: utf-8 -*-
 
;;;; Copyright (C) 1996 Mag. Christian Tanzer. All rights reserved.
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
;;;;    lse-lingo
;;;;
;;;; Purpose
;;;;    Define functions for definition, loading and use of lse linguistic
;;;;    languages 
;;;;    
;;;;    A lingo-token is similar to a simple token of an LS-Emacs language
;;;;    and to dynamic expansions (dabbrevs) of standard Emacs.
;;;;    
;;;;    The differences are:
;;;;    
;;;;    - A lingo-token expands to the token name.
;;;;    - A lingo-token is associated to a lingo-language.
;;;;    - A lingo-language is a set of lingo-tokens which are all taken from
;;;;      some natural language like English or German.
;;;;    - The user can add lingo-tokens dynamically.
;;;;
;;;; Revision Dates
;;;;    30-Mar-1996 (CT) Creation
;;;;--
;;;;
(provide 'lse-lingo)

(defvar lse-lingo:master_prefix "lse-lingo-")
(defvar lse-lingo:extension     ".lsc")

(defvar                      lse_lingo:language_table      (make-vector  47 0))
(defvar                      lse_lingo:token_table         (make-vector 137 0))
(make-variable-buffer-local 'lse_lingo:token_table)
(defvar                      lse-lingo:language            nil)
(make-variable-buffer-local 'lse-lingo:language)
(defvar                      lse-lingo:default-language    "german")

;;; 30-Mar-1996 
(defun lse-lingo@write-one-token (tok)
  (if (equal tok 0)
      t
    (princ "(lse-lingo:define-token "   (current-buffer))
    (prin1 (symbol-name     tok)        (current-buffer))
    (princ ")"                          (current-buffer))
    (terpri                             (current-buffer))
  )
; lse-lingo@write-one-token
)

;;; 30-Mar-1996 
(defun lse-lingo@expand (token bt et expansion)
  (delete-region bt et)
  (lse-fill-in-insert expansion)
  t
; lse-lingo@expand
)
;;; 30-Mar-1996 
(defun lse-lingo:define-token (name)
  (let ((tsym (intern name lse_lingo:token_table))
       )
    (set  tsym name)
    (fset tsym nil)
  )
; lse-lingo:define-token
)

;;; 30-Mar-1996 
(defun lse-lingo:language (name &optional token-size)
  (let ((lsym (intern-soft name lse_lingo:language_table))
       )
    (if (not lsym)
        (progn
          (setq lsym (intern name lse_lingo:language_table))
          (put  lsym 'token-table (make-vector (or token-size 137) 0))
        )
    )
    lsym
  )
; lse-lingo:language
)

;;; 30-Mar-1996 
(defun lse-lingo:load (&optional name)
  "Load lingo language from file."
  (interactive)
  (let ((lsym (lse-lingo:language name))
        (load-path lse-load-path)
        lse_lingo:token_table
       )
    (setq lse_lingo:token_table (get lsym 'token-table))
    (if (not
          (load (concat lse-lingo:master_prefix name lse-lingo:extension)
                t nil t
          )
        )
        (error "Error in definition of lingo-language %s" name)
      (put lsym 'token-table lse_lingo:token_table)
      (put lsym 'loaded t)
    )
    lsym
  )
; lse-lingo:load
)

;;; 30-Mar-1996 
(defun lse-lingo@write-language (name lsym)
  (let ((file-name (concat lse-lingo:master_prefix name lse-lingo:extension))
       )
    (save-window-excursion 
      (set-buffer (get-buffer-create file-name))
      (erase-buffer)
      (setq buffer-file-name   (concat lse-directory "/" file-name))
      (setq lse_lingo:token_table (get lsym 'token-table))
      (mapatoms 'lse-lingo@write-one-token lse_lingo:token_table)
      (save-buffer nil)    
    )
  )
  (lse-ring-bell)
; lse-lingo:write-language
)

;;; 30-Mar-1996 
(defun lse-lingo:buffer-init ()
  (lse-lingo:use lse-lingo:default-language)
; lse-lingo:buffer-init
)

;;; 30-Mar-1996 
(defun lse-lingo:use-german ()
  (lse-lingo:use "german")
; lse-lingo:use-german
)

;;; 30-Mar-1996 
(defun lse-lingo:use-english ()
  (lse-lingo:use "english")
; lse-lingo:use-english
)

(or (assq 'lse-lingo:buffer-init find-file-hooks)
    (lse-add-to-list find-file-hooks 'lse-lingo:buffer-init)
)

(lse-command:add-new "use lingo language" 'lse-lingo:use)
