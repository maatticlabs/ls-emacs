;-*- coding: iso-8859-15; -*-

;;;; Copyright (C) 2007-2013 Mag. Christian Tanzer. All rights reserved
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
;;;;    lse-config
;;;;
;;;; Purpose
;;;;    Custom configurations for LS-Emacs
;;;;
;;;; Revision Dates
;;;;    18-Oct-2007 (CT) Creation (factored from swing-default.el)
;;;;     9-Sep-2008 (CT) Add `Py-Version-Update` to Python menu in menu-bar
;;;;    14-May-2009 (CT) `lse-macosx` added
;;;;    28-Jan-2011 (CT) `unibyte` and `multibyte` removed
;;;;    17-May-2011 (CT) Set `inhibit-field-text-motion` to `t`
;;;;    18-May-2011 (CT) Guard for `py-mode-map` added
;;;;    26-May-2011 (CT) `set-language-environment` removed
;;;;    29-May-2011 (CT) Set `inhibit-field-text-motion` to `nil` (minibuffer!)
;;;;     4-Apr-2013 (CT) Change `show-paren-style` from `expression` to `mixed`
;;;;    17-May-2013 (CT) Use `whitespace-cleanup`, whitespace-mode;
;;;;                     set `whitespace-style`
;;;;    ««revision-date»»···
;;;;--

(when (eq window-system 'mac)
  (require 'lse-macosx)
)

(when (eq system-type 'windows-nt)
  (require 'lse-w32)
)

; (setq debug-on-error t)
; (setq debug-on-error nil)

(setq vc-handled-backends nil);  1-Oct-2007

(defvar lse-keys:function-key-map-bindings
    '( ;; Legacy bindings
       ([kp-f2]         [blue])
       ([f16]           [do])
       ([kp-f1]         [gold])
       ([s-kp-f1]       [gray])
       ([s-f17]         [green])
       ([f15]           [help])
       ([s-kp-f2]       [pink])
       ([f17]           [red])
       ;; Bindings copied from lse-keys-v19.el
       ([scroll]        [blue]); Windows NT
       ([scroll_lock]   [blue]); GNU/Linux (Gentoo)
       ([f12]           [do])
       ([pause]         [gold])
       ([s-pause]       [gray])
       ([s-print]       [green])
       ([f11]           [help])
       ([s-scroll]      [pink])
       ([s-scroll_lock] [pink])
       ([print]         [red])
     )
  "Override this in your .emacs file to define which keys to use for [gold],
  [blue], [red], ..."
)

(require 'ls-emacs)

;;; 28-Nov-2001
(if lse-emacs21-p
    (if (boundp 'lse-toolbar-flag)
        (progn
          (tool-bar-mode lse-toolbar-flag)
        )
    )
)

(if lse-emacs19-p
    (progn
      (setq                 message-log-max t); 30-May-1996
      (setq                 version-control t)
      (setq-default         version-control t)
      ;; replace all symbolic names with target names
      (setq find-file-visit-truename t);  1-Jan-2000
      ;; file-name-handler-alist not needed currently (no nfs used)
      (setq                 file-name-handler-alist nil)
    )
)

;;;  8-Oct-1996 ;  copied from files.el (19.34) and modified to avoid sh-mode
(setq interpreter-mode-alist
  '(;;; ("perl" . perl-mode)
    ;;; ("perl5" . perl-mode)
    ("wish" . tcl-mode)
    ("wishx" . tcl-mode)
    ("tcl" . tcl-mode)
    ("tclsh" . tcl-mode)
    ("awk" . awk-mode)
    ("mawk" . awk-mode)
    ("nawk" . awk-mode)
    ("gawk" . awk-mode)
    ("scm" . scheme-mode)
    ("bash" . lse-bash-mode)
    ;;;    ("ash" . sh-mode)
    ;;;    ("bash" . sh-mode)
    ;;;    ("csh" . sh-mode)
    ;;;    ("dtksh" . sh-mode)
    ;;;    ("es" . sh-mode)
    ;;;    ("itcsh" . sh-mode)
    ;;;    ("jsh" . sh-mode)
    ;;;    ("ksh" . sh-mode)
    ;;;    ("oash" . sh-mode)
    ;;;    ("pdksh" . sh-mode)
    ;;;    ("rc" . sh-mode)
    ;;;    ("sh" . sh-mode)
    ;;;    ("sh5" . sh-mode)
    ;;;    ("tcsh" . sh-mode)
    ;;;    ("wksh" . sh-mode)
    ;;;    ("wsh" . sh-mode)
    ;;;    ("zsh" . sh-mode)
    ("tail" . text-mode)
    ("more" . text-mode)
    ("less" . text-mode)
    ("pg" . text-mode)
   )
)

;;; 3-Jan-1994;;; (setq gc-cons-threshold 1000000)        ; turn down garbage collection
(setq-default indent-tabs-mode nil)     ; indentation done by spaces only
                                        ; (don't like tabs)

(auto-fill-mode 1)                      ; turn on auto fill mode
(setq         fill-column 77)           ; don't waste too much space
(setq-default fill-column 77)           ; don't waste too much space
(setq         case-replace nil)         ; do not change case of replacement
(setq-default case-replace nil)         ; do not change case of replacement

(setq         major-mode                  'text-mode)

(setq         initial-major-mode          'emacs-lisp-mode)
(setq         inhibit-startup-message      t)
(setq         trim-versions-without-asking t); 13-Mar-1995 worked for Emacs 19.22
(setq         delete-old-versions          t); 13-Mar-1995 Emacs 19.27 names

(setq         require-final-newline        t);  8-Oct-1996

(setq         adaptive-fill-mode           nil); 27-Oct-1996
(setq         inhibit-field-text-motion    nil); 29-May-2011

(setq         sentence-end-double-space    nil); 17-Dec-1997

;;;  3-Oct-1996
(setq display-time-24hr-format t)

;;; 28-Dec-1997
(setq vc-initial-comment t)

;;;  8-Oct-1996
(setq vc-follow-symlinks nil)
;;; 23-Jan-1996
(setq vc-make-backup-files t)           ; make backup files despite version control
(setq vc-mistrust-permissions nil)      ; use file permissions to decide if
                                        ; file is currently locked

;;; 13-Dec-1997
(setq dabbrev-case-fold-search nil); don't ignore case for dynamic abbreviations

;;;  2-Jan-1998
(setq ediff-use-long-help-message  nil); don't start with big ediff control-frame
(setq ediff-ignore-similar-regions t); ignore whitespace

(defun swing-terminal-setup ()
  (if lse-emacs19-p
      t
    (lse-terminal-setup)
  )
  (auto-fill-mode 1)
  (add-hook 'pre-command-hook 'lse-tpu:shift-mark-hook); 29-Dec-1997
)

(put     'minibuffer-history 'hist-ignore "self-insert-command")
(put     'minibuffer-history 'cursor-end  t)
(put     'narrow-to-region   'disabled nil)
(put     'eval-expression    'disabled nil)

(if lse-emacs19-p
    (add-hook 'after-init-hook 'swing-terminal-setup)
  (setq term-setup-hook 'swing-terminal-setup)
)

;;; 15-Oct-1996
(setq mail-host-address (lse-system-domain))

;;;  5-Feb-1998
(setq mail-signature   t)
(setq mail-self-blind  t)
(setq mail-yank-prefix "> ")

(if lse-emacsX-p; 31-May-1996
    (progn
      (if (boundp 'x-pointer-left-ptr)
         (setq x-pointer-shape x-pointer-left-ptr)
      )

      (if (and (boundp 'x-display-color-p) x-display-color-p)
         (set-mouse-color "RoyalBlue")
       (set-mouse-color (cdr (assq 'mouse-color (frame-parameters))))
      )

      (require 'paren)
      (setq show-paren-face 'highlight)
      (setq blink-matching-paren nil)
      (if lse-emacs20-p ;  4-Dec-1997
          (progn
            ;; 'expression  : show the entire expression enclosed by the paren
            ;; 'mixed       : show parens if both are visible else expression
            ;; 'parenthesis : highlight the matching parentheses
            (setq show-paren-style 'mixed)
          )
      )
      (show-paren-mode t)

      (setq x-display-name "Emacs")

      (setq mark-even-if-inactive t);; ???
      (setq highlight-nonselected-windows nil);  3-Jan-1998

      ;; 13-Dec-1997 ;; enabled font-lock-mode
      (global-font-lock-mode t)
    )
)

(if (fboundp 'auto-compression-mode); 17-Dec-1997
    (auto-compression-mode 1)
)

(if (fboundp 'hscroll-global-mode); 17-Dec-1997
    (progn
      (hscroll-global-mode 1)
      (setq hscroll-margin 1)
      (when (boundp 'hscroll-mode-name)    (setq hscroll-mode-name    " <>"))
      (when (boundp 'hscroll-step-percent) (setq hscroll-step-percent 5))
    )
)

;;; 11-Nov-2001
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0)
)
(if (fboundp 'whitespace-cleanup)
    (progn
      (setq whitespace-style
        '(face tabs trailing lines-tail space-before-tab empty)
      )
      (add-hook 'before-save-hook 'whitespace-cleanup)
    )
  (when (fboundp 'delete-trailing-whitespace)
    (add-hook 'before-save-hook 'delete-trailing-whitespace)
  )
)
(when (boundp 'x-stretch-cursor)
  (setq x-stretch-cursor t)
)

;;; 29-Dec-1997
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-list
        try-expand-line
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
       )
)

(require 'msb)
(setq msb-menu-cond
      (cons '((and (fboundp 'python-mode) (eq major-mode 'python-mode))
              3011 "Python Files (%d)"
             )
            msb-menu-cond
      )
)
(setq msb-max-menu-items 50)
(if (and (fboundp 'msb-mode) (not msb-mode)) (msb-mode));  3-Jan-2000

;;; don't convert eol automatically
(setq inhibit-eol-conversion t);  3-Jan-2000

;;; 29-Apr-1998
;;; print command customization

(defconst ps-print-color-p nil)
(defconst ps-lpr-command   "lpr")
(defconst ps-paper-type    'a4); 23-Feb-2000
(defconst ps-spool-duplex  nil); 23-Feb-2000
(setq     lpr-command      "a2ps")

;;;  1-Oct-2007
(setq indicate-buffer-boundaries 'right)
(setq indicate-empty-lines t)

(defconst ps-zebra-stripes nil); 23-Feb-2000

;;; enable highlighting of current line
(global-hl-line-mode 1); 24-Nov-2003

;;;  1-Oct-2007
(mouse-wheel-mode t)

;;;  4-Oct-2007
(add-hook 'before-save-hook 'lse-file:update-copyright)

;;;  5-Oct-2007
(defun Py-Version-Update ()
  "Update all python version files"
  (interactive)
  (lse-python:update-patchlevel-many
    "_CDT:Version.py" "_GCD:Version.py" "_XCD:Version.py"
    "_NDT:Version.py" "_GND:Version.py" "_XND:Version.py"
    "_DLT:Version.py"
  )
; Py-Version-Update
)

(global-set-key [red gold ?V] 'Py-Version-Update)

(when (boundp 'py-mode-map);; 18-May-2011
  ;;;  9-Sep-2008
  (define-key py-mode-map [menu-bar Python separator-LSE] '("--"))
  (define-key py-mode-map [menu-bar Python Py-Version-Update]
    '("Py-Version-Update" . Py-Version-Update)
  )
)

;;;  5-Feb-2008
(font-lock-add-keywords 'python-mode '(("### XXX" 1 font-lock-warning-face)))

;;;  5-Feb-2008
(if (fboundp 'whitespace-mode)
    (whitespace-mode t)
  (setq-default show-trailing-whitespace nil)
)

;;;; __END__ lse-config.el
