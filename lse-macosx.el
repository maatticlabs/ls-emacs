;-*- coding: utf-8 -*-

;;;; Copyright (C) 2009-2018 Mag. Christian Tanzer. All rights reserved
;;;; Glasauergasse 32, A--1130 Wien, Austria. tanzer@swing.co.at
;;;; ****************************************************************************
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the Free
;;;; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;; ****************************************************************************
;;;;
;;;;++
;;;; Name
;;;;    lse-macosx
;;;;
;;;; Purpose
;;;;    Customizations of LS-Emacs for Mac OSX
;;;;
;;;; Revision Dates
;;;;    14-May-2009 (CT) Creation
;;;;     3-Jun-2011 (CT) `provide` added
;;;;     7-Nov-2014 (CT) Add guard
;;;;    12-Dec-2014 (CT) Remove Aquamacs specific code
;;;;    12-Dec-2014 (CT) Add `lse-macosx:define-keys:german-keyboard`
;;;;    11-Dec-2017 (CT) Adapt to Cocoa-based Emacs
;;;;    25-Jan-2018 (CT) Add more translations for `escape <key>` to `M-<key>`
;;;;    ««revision-date»»···
;;;;--

(provide 'lse-macosx)

;;; https://emacsformacosx.com/tips

(setq
  ns-command-modifier         'alt
  ns-function-modifier        nil
  ns-option-modifier          'super
  ns-right-alternate-modifier  nil
  ns-right-command-modifier    nil
)

(defvar lse-keys:function-key-map-bindings
  '(
    ([f9]         [insert])
    ([f10]        [red])
    ([f11]        [blue])
    ([f12]        [gold])
    ([s-f12]      [do])
    ;; Bindings copied from lse-config.el
    ([(super \#)] [letter-prefix])
   )
)

;;; Putting "…" into function-key-map doesn't work --> put it into
;;; key-translation-map instead
(define-key key-translation-map "…"    [cancel])
(define-key key-translation-map "∞"    [select])
(define-key key-translation-map [S-f9] [select])

;;; As we don't have a free key for Meta, map `ESC key` to `M-key`
(define-key input-decode-map [escape deletechar]     [M-delete])
(define-key input-decode-map [escape C-deletechar]   [C-M-delete])
(define-key input-decode-map [escape down]           [M-down])
(define-key input-decode-map [escape backspace]      [M-backspace])
(define-key input-decode-map [escape C-backspace]    [C-M-backspace])
(define-key input-decode-map [escape f9]             [M-insert])
(define-key input-decode-map [escape C-f9]           [C-M-insert])
(define-key input-decode-map [escape left]           [M-left])
(define-key input-decode-map [escape C-left]         [C-M-left])
(define-key input-decode-map [escape right]          [M-right])
(define-key input-decode-map [escape C-right]        [C-M-right])
(define-key input-decode-map [escape up]             [M-up])

(define-key input-decode-map [escape ?b]             [?\M-b])
(define-key input-decode-map [escape ?f]             [?\M-f])
(define-key input-decode-map [escape ?l]             [?\M-l])
(define-key input-decode-map [escape ?n]             [?\M-n])
(define-key input-decode-map [escape ?\s-n]          [?\M-\s-n])
(define-key input-decode-map [escape ?p]             [?\M-p])
(define-key input-decode-map [escape ?\s-p]          [?\M-\s-p])
(define-key input-decode-map [escape ?R]             [?\M-R])
(define-key input-decode-map [escape ?r]             [?\M-r])
(define-key input-decode-map [escape ?v]             [?\M-v])

(define-key input-decode-map [escape ?0]             [?\M-0])
(define-key input-decode-map [escape ?1]             [?\M-1])
(define-key input-decode-map [escape ?2]             [?\M-2])
(define-key input-decode-map [escape ?3]             [?\M-3])
(define-key input-decode-map [escape ?4]             [?\M-4])
(define-key input-decode-map [escape ?5]             [?\M-5])
(define-key input-decode-map [escape ?6]             [?\M-6])
(define-key input-decode-map [escape ?7]             [?\M-7])
(define-key input-decode-map [escape ?8]             [?\M-8])
(define-key input-decode-map [escape ?9]             [?\M-9])

(define-key input-decode-map [escape ?\-]            [?\M-\-])
(define-key input-decode-map [escape ?\+]            [?\M-\+])

;;;; __END__ lse-macosx.el
