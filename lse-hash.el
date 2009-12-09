;-*- unibyte: t; coding: iso-8859-1; -*-

;;;; Copyright (C) 2009 Mag. Christian Tanzer All rights reserved
;;;; Glasauergasse 32, A--1130 Wien, Austria. tanzer@swing.co.at
;;;; ****************************************************************************
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;; ****************************************************************************
;;;;
;;;;++
;;;; Name
;;;;    lse-hash
;;;;
;;;; Purpose
;;;;    Provide functions for dealing with hash tables
;;;;
;;;; Revision Dates
;;;;     9-Dec-2009 (CT) Creation
;;;;    ««revision-date»»···
;;;;--

(provide 'lse-hash)
(require 'cl)

;;;  9-Dec-2009
(defun lse-hash:clear (hash)
  "Remove all entries from `hash`."
  (clrhash hash)
; lse-hash:clear
)

;;;  9-Dec-2009
(defun lse-hash:count (hash)
  "Return number of entries in `hash`"
  (hash-table-count hash)
; lse-hash:count
)

;;;  9-Dec-2009
(defun lse-hash:del (hash key)
  "Remove `key` from `hash`."
  (remhash key hash)
; lse-hash:del
)

;;;  9-Dec-2009
(defun lse-hash:get (hash key &optional default)
  "Return value of `key` in `hash` or `default`."
  (gethash key hash default)
; lse-hash:get
)

;;;  9-Dec-2009
(defun lse-hash:items (hash)
  "Return list of `(key value)` pairs in `hash`."
  (let (result)
    (maphash
      (function (lambda (k v) (push (list k v) result)))
      hash
    )
    result
  )
; lse-hash:items
)

;;;  9-Dec-2009
(defun lse-hash:keys (hash)
  "Return list of keys in `hash`."
  (let (result)
    (maphash
      (function (lambda (k v) (push k result)))
      hash
    )
    result
  )
; lse-hash:keys
)

;;;  9-Dec-2009
(defun lse-hash:new (&optional alist parent &rest arg)
  "Return new hash initialized as a copy of `parent` and updated by contents of `alist`. "
  (let* ((kw (or arg (list :test 'equal)))
         (result
           (if parent
               (copy-hash-table parent)
             (apply 'make-hash-table kw)
           )
         )
        )
    (lse-hash:update result alist)
  )
; lse-hash:new
)

;;;  9-Dec-2009
(defun lse-hash:put (hash key val)
  "Map `key` to `val` in `hash` table."
  (puthash key val hash)
; lse-hash:put
)

;;;  9-Dec-2009
(defun lse-hash:update (hash alist)
  "Update `hash` with contents of `alist`."
  (let (item key val)
    (while alist
      (setq item  (car alist))
      (setq alist (cdr alist))
      (setq key   (car item))
      (setq val   (cdr item))
      (lse-hash:put hash key val)
    )
  )
  hash
; lse-hash:update
)

;;;  9-Dec-2009
(defun lse-hash:values (hash)
  "Return list of keys in `hash`."
  (let (result)
    (maphash
      (function (lambda (k v) (push v result)))
      hash
    )
    result
  )
; lse-hash:values
)

;;;; Multimap using list
(defalias 'lse-hash:mml:clear  'lse-hash:clear)
(defalias 'lse-hash:mml:count  'lse-hash:count)
(defalias 'lse-hash:mml:del    'lse-hash:del)
(defalias 'lse-hash:mml:get    'lse-hash:get)
(defalias 'lse-hash:mml:items  'lse-hash:items)
(defalias 'lse-hash:mml:keys   'lse-hash:keys)
(defalias 'lse-hash:mml:values 'lse-hash:values)

;;;  9-Dec-2009
(defun lse-hash:mml:new (&optional alist parent &rest arg)
  "Return new multi-map (list) initialized as a copy of `parent` and updated by contents of `alist`."
  (letf (((symbol-function 'lse-hash:put) (symbol-function 'lse-hash:mml:put))
        )
    (apply 'lse-hash:new alist parent arg)
  )
; lse-hash:mml:new
)

;;;  9-Dec-2009
(defun lse-hash:mml:put (hash key val)
  "Add `val` to list mapped by `key` in multi-map (list) `hash`."
  (let* ((has_key (lse-hash:get hash key))
         (list    (or has_key nil))
        )
    (push val list)
    (puthash key list hash)
  )
; lse-hash:mml:put
)

;;;  9-Dec-2009
(defun lse-hash:mml:update (hash alist)
  "Update multi-map (list) `hash` with contents of `alist`."
  (letf (((symbol-function 'lse-hash:put) (symbol-function 'lse-hash:mml:put))
        )
    (lse-hash:update hash alist)
  )
; lse-hash:mml:update
)

;;;; Multimap using set
(defalias 'lse-hash:mms:clear  'lse-hash:clear)
(defalias 'lse-hash:mms:count  'lse-hash:count)
(defalias 'lse-hash:mms:del    'lse-hash:del)
(defalias 'lse-hash:mms:get    'lse-hash:get)
(defalias 'lse-hash:mms:items  'lse-hash:items)
(defalias 'lse-hash:mms:keys   'lse-hash:keys)
(defalias 'lse-hash:mms:values 'lse-hash:values)

;;;  9-Dec-2009
(defun lse-hash:mms:new (&optional alist parent &rest arg)
  "Return new multi-map (set) initialized as a copy of `parent` and updated by contents of `alist`."
  (letf (((symbol-function 'lse-hash:put) (symbol-function 'lse-hash:mms:put))
        )
    (apply 'lse-hash:new alist parent arg)
  )
; lse-hash:mms:new
)

;;;  9-Dec-2009
(defun lse-hash:mms:put (hash key val)
  "Add `val` to list mapped by `key` in multi-map (set) `hash`."
  (let* ((has_key (lse-hash:get hash key))
         (list    (or has_key nil))
        )
    (or (member val list) (push val list))
    (puthash key list hash)
  )
; lse-hash:mms:put
)

;;;  9-Dec-2009
(defun lse-hash:mms:update (hash alist)
  "Update multi-map (set) `hash` with contents of `alist`."
  (letf (((symbol-function 'lse-hash:put) (symbol-function 'lse-hash:mms:put))
        )
    (lse-hash:update hash alist)
  )
; lse-hash:mms:update
)

;;; __END__ lse-hash.el
