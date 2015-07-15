;-*- coding: utf-8; -*-

;;;; Copyright (C) 2015 Mag. Christian Tanzer All rights reserved
;;;; Glasauergasse 32, A--1130 Wien, Austria. tanzer@swing.co.at
;;;; #*** <License> ************************************************************#
;;;; This file is part of the LS-Emacs, a package built on top of GNU Emacs.
;;;;
;;;; This file is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This file is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this file. If not, see <http://www.gnu.org/licenses/>.
;;;; #*** </License> ***********************************************************#
;;;;
;;;;++
;;;; Name
;;;;    lse-python
;;;;
;;;; Purpose
;;;;    Provide wrapper around python.el
;;;;
;;;; Revision Dates
;;;;    15-Jul-2015 (CT) Creation (factored from `lse-language-python`)
;;;;                     + Use `python`, not `python-mode`
;;;;    ««revision-date»»···
;;;;--

(eval-when-compile
  (require 'easymenu)
  (require 'lse-tpu)
  (require 'python)
  (require 'rx)
)

;;; 15-Jul-2015
(defun python-skeleton-add-menu-items ()
  t
; python-skeleton-add-menu-items
)

(eval-when-compile
  (defconst lse-python:rx-constituents
    `(
      (class       . ,(rx symbol-start "class" symbol-end))
      (def         . ,(rx symbol-start "def"   symbol-end))
      ;; copy of python.el: python-rx-constituents
      (block-start          . ,(rx symbol-start
                                 (or "def" "class" "if" "elif" "else" "try"
                                     "except" "finally" "for" "while" "with")
                                 symbol-end))
      (dedenter            . ,(rx symbol-start
                                   (or "elif" "else" "except" "finally")
                                   symbol-end))
      (block-ender         . ,(rx symbol-start
                                  (or
                                   "break" "continue" "pass" "raise" "return")
                                  symbol-end))
      (decorator            . ,(rx line-start (* space) ?@ (any letter ?_)
                                   (* (any word ?_))))
      (defun                . ,(rx symbol-start (or "def" "class") symbol-end))
      (if-name-main         . ,(rx line-start "if" (+ space) "__name__"
                                   (+ space) "==" (+ space)
                                   (any ?' ?\") "__main__" (any ?' ?\")
                                   (* space) ?:))
      (symbol-name          . ,(rx (any letter ?_) (* (any word ?_))))
      (open-paren           . ,(rx (or "{" "[" "(")))
      (close-paren          . ,(rx (or "}" "]" ")")))
      (simple-operator      . ,(rx (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%)))
      ;; FIXME: rx should support (not simple-operator).
      (not-simple-operator  . ,(rx
                                (not
                                 (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))))
      ;; FIXME: Use regexp-opt.
      (operator             . ,(rx (or "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
                                       "=" "%" "**" "//" "<<" ">>" "<=" "!="
                                       "==" ">=" "is" "not")))
      ;; FIXME: Use regexp-opt.
      (assignment-operator  . ,(rx (or "=" "+=" "-=" "*=" "/=" "//=" "%=" "**="
                                       ">>=" "<<=" "&=" "^=" "|=")))
      (string-delimiter . ,(rx (and
                                ;; Match even number of backslashes.
                                (or (not (any ?\\ ?\' ?\")) point
                                    ;; Quotes might be preceded by a escaped quote.
                                    (and (or (not (any ?\\)) point) ?\\
                                         (* ?\\ ?\\) (any ?\' ?\")))
                                (* ?\\ ?\\)
                                ;; Match single or triple quotes of any kind.
                                (group (or  "\"" "\"\"\"" "'" "'''")))))
      (coding-cookie . ,(rx line-start ?# (* space)
                            (or
                             ;; # coding=<encoding name>
                             (: "coding" (or ?: ?=) (* space) (group-n 1 (+ (or word ?-))))
                             ;; # -*- coding: <encoding name> -*-
                             (: "-*-" (* space) "coding:" (* space)
                                (group-n 1 (+ (or word ?-))) (* space) "-*-")
                             ;; # vim: set fileencoding=<encoding name> :
                             (: "vim:" (* space) "set" (+ space)
                                "fileencoding" (* space) ?= (* space)
                                (group-n 1 (+ (or word ?-))) (* space) ":"))))
     )
  )
)

;; copy of python.el: python-rx
(defmacro python-rx (&rest regexps)
  "Python mode specialized rx macro.
This variant of `rx' supports common Python named REGEXPS."
  (let ((rx-constituents (append lse-python:rx-constituents rx-constituents)))
    (cond ((null regexps)
           (error "No regexp"))
          ((cdr regexps)
           (rx-to-string `(and ,@regexps) t))
          (t
           (rx-to-string (car regexps) t)))))


(defconst python-nav-beginning-of-block-regexp
  (python-rx block-start)
)

(defconst python-nav-beginning-of-class-regexp
  (python-rx line-start (* space) class (+ space) (group symbol-name))
  "Regexp matching class definition.
The name of the class should be grouped so it can be retrieved
via `match-string'."
)

(defconst python-nav-beginning-of-def-regexp
  (python-rx line-start (* space) def (+ space) (group symbol-name))
  "Regexp matching function definition.
The name of the def should be grouped so it can be retrieved
via `match-string'."
)

(defconst python-nav-end-comment-regexp
  "^ +# *end +"
)
(defconst lse-python:blank-or-comment-re "[ \t]*\\($\\|#\\)"
  "Regular expression matching a blank or comment line."
)

;;;  4-Jul-2012
(defvar lse-python:var-def-pattern "^ *[a-zA-Z_0-9]+ *=")
(defvar lse-python:var-def-pattern-g "^[a-zA-Z_0-9]+ *=")

;;;  9-Mar-1998
(defun lse-python:indent-line ()
  (interactive "*")
  (if (or (equal      (char-syntax (following-char)) ?\) )
          (looking-at "[-+*/,]")
      )
       (lse-indent-line)
    (condition-case nil
        (python-indent-line)
      (error (lse-indent-line))
    )
  )
; lse-python:indent-line
)

;;; 15-Jul-2015
(defun lse-python::end-comment-pos (indent)
  (save-excursion
    (save-match-data
      (catch 'result
        (while (not (eobp))
          (let* ((empty-p (looking-at "^ *$"))
                 (current-indent
                   (if empty-p (1+ indent) (current-indentation))
                 )
                )
            (cond
              ((and
                 (= current-indent indent)
                 (lse-python::looking-at-line
                   python-nav-end-comment-regexp 'no-syntax
                 )
               )
               (throw 'result (point))
              )
              ((> current-indent indent)
               (lse-tpu:forward-line 1)
              )
              (t
               (throw 'result nil)
              )
            )
          )
        )
      )
    )
  )
; lse-python::end-comment-pos
)

;;; 15-Jul-2015
(defun lse-python::backward-syntactically (regexp)
  (python-nav--syntactically
    (lambda () (re-search-backward regexp nil t))
    '<
  )
; lse-python::backward-syntactically
)

;;; 15-Jul-2015
(defun lse-python::forward-syntactically (regexp)
  (python-nav--syntactically
    (lambda () (re-search-forward regexp nil t))
    '>
  )
; lse-python::forward-syntactically
)

;;; 18-Nov-2002
(defun lse-python::goto-end-of-last-statement ()
  (lse-tpu:next-end-of-line 0)
  (while (and (save-excursion
                (beginning-of-line)
                (looking-at lse-python:blank-or-comment-re)
              )
              (not (bobp))
         )
    (lse-tpu:next-end-of-line -1)
  )
; lse-python::goto-end-of-last-statement
)

;;; 18-Nov-2002
(defun lse-python::goto-next-statement (num)
  (dotimes (i (if (and (numberp num) (> num 0)) num 1))
    (unless (eolp) (lse-tpu:next-end-of-line 1))
    (python-nav-forward-statement)
  )
; lse-python::goto-next-statement
)

;;; 18-Nov-2002
(defun lse-python::goto-prev-statement (num)
  (dotimes (i (if (and (numberp num) (> num 0)) num 1))
    (let ((current-pos (point))
         )
      (python-nav-beginning-of-statement)
      (when (= current-pos (point))
        (python-nav-backward-statement)
      )
      (back-to-indentation)
    )
  )
; lse-python::goto-prev-statement
)

;;; 15-Jul-2015
(defun lse-python::goto-regexp-head (regexp)
  (save-match-data
    (let ((current-indent (current-indentation))
          (current-pos    (point))
          (inside-p
            (not
              (or
                (lse-python::looking-at-line regexp)
                (lse-python::looking-at-line
                    python-nav-end-comment-regexp 'no-syntax
                )
              )
            )
          )
          found
         )
      (setq found
        (lse-python::backward-syntactically regexp)
      )
      (while
        (and
          found
          (< (point) current-pos)
          (cond
            (inside-p
             (>= (current-indentation) current-indent)
            )
            (t
             (>  (current-indentation) current-indent)
            )
          )
        )
        (setq found
          (lse-python::backward-syntactically regexp)
        )
      )
      (when found
        (lse-indent:goto-indent-pos)
      )
      found
    )
  )
; lse-python::goto-regexp-head
)

;;; 15-Jul-2015
(defun lse-python::goto-regexp-tail (regexp)
  (save-match-data
    (let* ((current-pos (point))
           (line-head   (lse-tpu:line-head-pos))
           (orig-pos    current-pos)
           (at-begin-p  (lse-python::looking-at-line regexp))
           (at-end-p
             (lse-python::looking-at-line
               python-nav-end-comment-regexp 'no-syntax
             )
           )
           head-found
          )
      (if at-end-p
          (lse-python::forward-syntactically regexp)
        (unless at-begin-p
          (setq head-found (lse-python::goto-regexp-head regexp))
        )
      )
      (lse-python::goto-regexp-tail:inner regexp)
      (setq at-end-p (= (point) line-head))
      (when (and head-found (or at-end-p (= (point) (1+ orig-pos))))
        (when at-end-p
          (lse-python::forward-syntactically regexp)
        )
        (setq head-found (lse-python::goto-regexp-head regexp))
        (lse-python::goto-regexp-tail:inner regexp)
      )
      (if (> (point) current-pos)
          (progn
            (if (looking-at " *#+ end +")
                (lse-tpu:next-end-of-line 1)
              (lse-tpu:previous-end-of-line 1)
            )
          )
        (when (and head-found (< (point) orig-pos))
          (goto-char orig-pos)
        )
      )
    )
  )
;y lse-python::goto-regexp-tail
)

;;; 15-Jul-2015
(defun lse-python::goto-regexp-tail:inner (regexp)
  (let ((current-indent (current-indentation))
        (current-pos    (point))
        end-pos
       )
      (while
        (progn
          (python-nav-end-of-statement)
          (python-util-forward-comment 1)
          (and
            (> (current-indentation) current-indent)
            (not (eobp))
          )
        )
      )
      (python-util-forward-comment -1)
      (setq end-pos (lse-python::end-comment-pos current-indent))
      (when end-pos
        (goto-char end-pos)
      )
      (forward-line 1)
      ;; Ensure point moves forward.
      (and (> current-pos (point)) (goto-char current-pos))
  )
; lse-python::goto-regexp-tail:inner
)

;;; 15-Jul-2015
(defun lse-python::looking-at-line (regexp &optional no-syntax)
  (and (or no-syntax (not (python-syntax-context-type (syntax-ppss))))
       (save-excursion
         (beginning-of-line 1)
         (looking-at regexp)
       )
  )
; lse-python::looking-at-line
)

;;; 24-Sep-1999
(defun lse-python:goto-begin-of-block ()
  "Goto begin of current python block"
  (interactive)
  (if (progn
        (python-nav-beginning-of-statement)
        (looking-at python-nav-beginning-of-block-regexp)
      )
      (lse-tpu:previous-end-of-line 1)
  )
  (python-nav-beginning-of-block)
; lse-python:goto-begin-of-block
)

;;; 24-Sep-1999
(defun lse-python:goto-end-of-block ()
  "Got end of current python block"
  (interactive)
  (python-nav-end-of-block)
; lse-python:goto-end-of-block
)

;;; 24-Feb-2012
(defun lse-python:goto-class-head ()
  "Goto head of containing class"
  (interactive)
  (lse-python::goto-regexp-head python-nav-beginning-of-class-regexp)
; lse-python:goto-class-head
)

;;; 24-Feb-2012
(defun lse-python:goto-class-tail ()
  "Goto tail of containing class"
  (interactive)
  (lse-python::goto-regexp-tail python-nav-beginning-of-class-regexp)
; lse-python:goto-class-tail
)

;;; 24-Feb-2012
(defun lse-python:goto-function-head ()
  "Goto head of containing function"
  (interactive)
  (lse-python::goto-regexp-head python-nav-beginning-of-def-regexp)
; lse-python:goto-function-head
)

;;; 24-Feb-2012
(defun lse-python:goto-function-tail ()
  "Goto tail of containing function"
  (interactive)
  (lse-python::goto-regexp-tail python-nav-beginning-of-def-regexp)
; lse-python:goto-function-tail
)

;;; 30-Aug-2010
(defun lse-python:goto-near-top-pos ()
  "Goto position near the top of a Python module."
  (interactive)
  (let ((ntp (lse-python:near-top-pos))) (if ntp (goto-char ntp)))
; lse-python:goto-near-top-pos
)

(defun lse-python:goto-next-class-head (count &optional limit)
  "Goto next head of class."
  (interactive "p")
  (lse-tpu:goto_occurrence "^ *class " limit count 're-search-forward t)
; lse-python:goto-next-class-head
)

;;;  4-Jul-2012
(defun lse-python:goto-next-doctest (count &optional limit)
  "Goto next doctest"
  (interactive "p")
  (lse-tpu:goto_occurrence "^ +>>> " limit count 're-search-forward t)
; lse-python:goto-next-doctest
)

;;; 24-Feb-2012
(defun lse-python:goto-next-function-head (count &optional limit)
  "Goto next head of function."
  (interactive "p")
  (lse-tpu:goto_occurrence "^ *def " limit count 're-search-forward t)
; lse-python:goto-next-function-head
)

;;;  4-Jul-2012
(defun lse-python:goto-next-var (count &optional limit)
  "Got next variable definition"
  (interactive "p")
  (lse-tpu:goto_occurrence
     lse-python:var-def-pattern limit count 're-search-forward t
  )
; lse-python:goto-next-var
)

;;;  4-Jul-2012
(defun lse-python:goto-next-var-g (count &optional limit)
  "Got next global variable definition"
  (interactive "p")
  (lse-tpu:goto_occurrence
     lse-python:var-def-pattern-g limit count 're-search-forward t
  )
; lse-python:goto-next-var
)

;;;  4-Jul-2012
(defun lse-python:goto-prev-doctest (count &optional limit)
  "Goto previous doctest"
  (interactive "p")
  (lse-tpu:goto_occurrence "^ +>>> " limit count 're-search-backward t)
; lse-python:goto-prev-doctest
)

;;;  8-Sep-2002
(defun lse-python:goto-next-statement (num)
  "Goto next statement"
  (interactive "P")
  (lse-python::goto-next-statement num)
; lse-python:goto-next-statement
)

;;; 18-Nov-2002
(defun lse-python:goto-next-statement-end (num)
  "Goto end of current statement"
  (interactive "P")
  (if (eolp)
      (lse-python::goto-next-statement 1)
  )
  (lse-python::goto-next-statement num)
  (lse-python::goto-end-of-last-statement)
; lse-python:goto-next-statement-end
)

;;;  4-Jul-2012
(defun lse-python:goto-prev-var (count &optional limit)
  "Got previous variable definition"
  (interactive "p")
  (lse-tpu:goto_occurrence
     lse-python:var-def-pattern limit count 're-search-backward t
  )
; lse-python:goto-next-var
)

;;;  8-Sep-2002
(defun lse-python:goto-prev-statement (num)
  "Goto previous statement"
  (interactive "P")
  (lse-python::goto-prev-statement num)
; lse-python:goto-prev-statement
)

;;; 18-Nov-2002
(defun lse-python:goto-prev-statement-end (num)
  "Goto end of previous statement"
  (interactive  "P")
  (lse-python:goto-prev-statement num)
  (lse-python::goto-end-of-last-statement)
; lse-python:goto-prev-statement-end
)

;;;  4-Jul-2012
(defun lse-python:goto-prev-var-g (count &optional limit)
  "Got previous global variable definition"
  (interactive "p")
  (lse-tpu:goto_occurrence
     lse-python:var-def-pattern-g limit count 're-search-backward t
  )
; lse-python:goto-next-var
)

;;; 30-Aug-2010
(defun lse-python:near-top-pos ()
  (save-excursion
    (save-match-data
      (lse-tpu:move-to-beginning)
      (re-search-forward "^#--")
      (skip-chars-forward " \t\n")
      (if (looking-at "\"\"\"")
          (progn
            (lse-tpu:forward-char 3)
            (re-search-forward "\"\"\"")
            (skip-chars-forward " \t\n")
          )
      )
      (point)
    )
  )
; lse-python:near-top-pos
)

(provide 'lse-python)

;;;; __END__ lse-python
