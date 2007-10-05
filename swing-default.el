;-*- unibyte: t; coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work

;;;;unix_ms_filename_correspondency swing-default.el swi_dflt.el
;;;; Copyright (C) 1994-2007 Mag. Christian Tanzer. All rights reserved.
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
;;;;    swing-default
;;;;
;;;; Purpose
;;;;    Default emacs initialization file for site swing
;;;;
;;;; Revision Dates
;;;;    29-May-1994 (CT) Creation (of comment)
;;;;    29-May-1994 (CT) require-list moved from swing-keys to this file
;;;;     6-Jan-1995 (CT) Mail alias commands
;;;;    13-Mar-1995 (CT) Defined delete-old-versions (for Emacs 19.27)
;;;;    23-Jan-1996 (CT) Set some variables for the version-control package vc
;;;;    30-May-1996 (CT) Set `message-log-max' (for Emacs 19.29)
;;;;    30-May-1996 (CT) Took some tricks from S.u.S.E. default.el
;;;;    31-May-1996 (CT) Took some more tricks from S.u.S.E. default.el and
;;;;                     term/function-keys.el
;;;;     2-Oct-1996 (CT) Use paren.el instead of stig-paren.el
;;;;     8-Oct-1996 (CT) Set vc-follow-symlinks to nil
;;;;     8-Oct-1996 (CT) Set `interpreter-mode-alist' to avoid sh-mode
;;;;    27-Oct-1996 (CT) Set `adaptive-fill-mode' to nil
;;;;    25-May-1997 (CT) Redefined `vc-lock-from-permissions'
;;;;    13-Dec-1997 (CT) font-lock-mode enabled
;;;;     2-Jan-1998 (CT) Some ediff options set
;;;;     2-Jan-1998 (CT) `lse-keys:override-emacs-control-keys' defined
;;;;    10-Jan-1998 (CT) Define `lse-keys:override-emacs-control-keys' only
;;;;                     if not running under `window-system'
;;;;     5-Feb-1998 (CT) mail- parameters added
;;;;    17-Mar-1998 (CT) (require 'msb) added
;;;;    19-Mar-1998 (CT) msb-max-menu-items set to 50
;;;;    21-Mar-1998 (CT) Removed mail aliases (better defined in /etc/aliases)
;;;;    29-Apr-1998 (CT) Customization of emacs print commands
;;;;    19-Dec-1999 (CT) `lazy-lock-minimum-size' set to 1024
;;;;    19-Dec-1999 (CT) `enable-multibyte-characters' set to nil
;;;;     1-Jan-2000 (CT) `find-file-visit-truename' set to t
;;;;     3-Jan-2000 (CT) Execute `msb-mode' if necessary
;;;;     3-Jan-2000 (CT) `inhibit-eol-conversion' set to t
;;;;     3-Jan-2000 (CT) `server-window' set to extra frame
;;;;    11-Nov-2001 (CT) `tool-bar-mode` disabled
;;;;    11-Nov-2001 (CT) `delete-trailing-whitespace` added to
;;;;                     `write-file-hooks`
;;;;    13-Sep-2002 (MG) `boundp` guards added for `x-pointer-left-ptr` and
;;;;                     `x-display-color-p`
;;;;    13-Nov-2002 (CT) Don't make server window under windows-nt
;;;;     1-Oct-2007 (CT) `lazy-lock-mode` guarded against Emacs 22 (where it
;;;;                     doesn't exist anymore)
;;;;     1-Oct-2007 (CT) Call to `set-language-environment` added
;;;;     1-Oct-2007 (CT) Various modifications for Emacs 22
;;;;     3-Oct-2007 (CT) `lse-keys:override-emacs-control-keys` removed
;;;;     3-Oct-2007 (CT) `lse-keys:function-key-map-bindings` added
;;;;     4-Oct-2007 (CT) `lse-file:update-copyright` added to `before-save-hook`
;;;;     5-Oct-2007 (CT) `Py-Version-Update` added and bound to `[red gold ?V]`
;;;;    ««revision-date»»···
;;;;--
; (setq debug-on-error t)
; (setq debug-on-error nil)
;;;  4-Oct-2007
(setq safe-local-variable-values
  (list '(unibyte . t))
)

(setq-default enable-multibyte-characters nil); 19-Dec-1999
(set-language-environment "Latin-9"); 1-Oct-2007

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
      (setq                 directory-abbrev-alist
       '(
          ("^/swing/.*/generic_folien"           . "/swing/folien")
          ("^/swing/.*org/aufwandserfassung"     . "/swing/aufwand")
          ("^/swing/.*org/buchhaltung"           . "/swing/buchhaltung")
          ("^/swing/.*org/finanzplanung"         . "/swing/finanzplanung")
          ("^/swing/.*org/kassabuch"             . "/swing/kassabuch")
          ("^/swing/.*project/ek"                . "/swing/ek")
          ("^/swing/.*/seminare/sb"              . "/swing/seminar_def")
          ("^/swing/.*system/awk"                . "/swing/awk")
          ("^/swing/.*system/com"                . "/swing/com")
          ("^/swing/.*system/emacs"              . "/swing/emacs")
          ("^/swing/.*system/kartei"             . "/swing/kartei")
          ("^/swing/.*system/login"              . "/swing/login")
          ("^/swing/.*system/perl"               . "/swing/perl")
          ("^/swing/.*system/stydoc"             . "/swing/stydoc")
          ("^/swing/work/marketing"              . "/swing/marketing")
          ("^/swing/work/org"                    . "/swing/org")
          ("^/swing/work/private"                . "/swing/private")
          ("^/swing/work/project"                . "/swing/project")
          ("^/swing/work/system"                 . "/swing/system")
        )
      )
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

(setq         default-major-mode          'text-mode); 30-May-1996 taken from suse default.el

(setq         initial-major-mode          'emacs-lisp-mode)
(setq         inhibit-startup-message      t)
(setq         trim-versions-without-asking t); 13-Mar-1995 worked for Emacs 19.22
(setq         delete-old-versions          t); 13-Mar-1995 Emacs 19.27 names

(setq         require-final-newline        t);  8-Oct-1996

(setq         adaptive-fill-mode           nil); 27-Oct-1996

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

;;; 25-May-1997
;;; change vc-lock-from-permissions to allow editing if group has write
;;; permission
(defun vc-lock-from-permissions (file)
  ;; If the permissions can be trusted for this file, determine the
  ;; locking state from them.  Returns (user-login-name), `none', or nil.
   ;;   This implementation assumes that any file which is under version
  ;; control and has -rw-r--r-- is locked by its owner.  This is true
  ;; for both RCS and SCCS, which keep unlocked files at -r--r--r--.
  ;; We have to be careful not to exclude files with execute bits on;
  ;; scripts can be under version control too.  Also, we must ignore the
  ;; group-read and other-read bits, since paranoid users turn them off.
  ;;   This hack wins because calls to the somewhat expensive
  ;; `vc-fetch-master-properties' function only have to be made if
  ;; (a) the file is locked by someone other than the current user,
  ;; or (b) some untoward manipulation behind vc's back has changed
  ;; the owner or the `group' or `other' write bits.
  (let ((attributes (file-attributes file)))
    (if (not (vc-mistrust-permissions file))
	(cond ((string-match ".r-..-..-." (nth 8 attributes))
	       (vc-file-setprop file 'vc-locking-user 'none))
              ((string-match ".rw..w..[-w]." (nth 8 attributes));; 25-May-1997 Swing
	       (vc-file-setprop file 'vc-locking-user 'none))   ;; 25-May-1997 Swing
	      ((and (= (nth 2 attributes) (user-uid))
		    (string-match ".rw..-..-." (nth 8 attributes))
               )
	       ; (vc-file-setprop file 'vc-locking-user (user-login-name))
               (vc-file-setprop file 'vc-locking-user 'none);; 10-Sep-2001 CT
              )
	      (nil)))))

;;; 13-Dec-1997
(setq dabbrev-case-fold-search nil); don't ignore case for dynamic abbreviations

;;;  2-Jan-1998
(setq ediff-use-long-help-message  nil); don't start with big ediff control-frame
(setq ediff-ignore-similar-regions t); ignore whitespace

; (setq ispell-command-options "-a -s -t -p /swing/kartei/.ispell_words")
; (setq ispell-command-options nil)

(defun swing-terminal-setup ()
  (if lse-emacs19-p
      t
    (lse-terminal-setup)
  )
  (auto-fill-mode 1)
  (lse-tpu:toggle-regexp)
  (add-hook 'pre-command-hook 'lse-tpu:shift-mark-hook); 29-Dec-1997
)
; (debug-on-entry (quote swing-terminal-setup))


(require 'swing-command)
(load "swing-keys")
(load "swing-kartei")

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
      ;; 30-May-1996 taken from suse default.el
      ;;
      ;; Show corresponding braces
      ;; -------------------------
      ;;  2-Oct-1996 replaced stig-paren by paren
      ;;             stig-paren doesn't work with 19.34
      (require 'paren)
      (setq show-paren-face 'highlight)
      (setq blink-matching-paren nil)
      (if lse-emacs20-p ;  4-Dec-1997
          (progn
            ;; show the entire expression enclosed by the paren
            (setq show-paren-style 'expression)
          )
      )
      (show-paren-mode t);  4-Dec-1997 added this to support Emacs 20.n
      ;;        (require 'stig-paren)
      ;;        (setq paren-dingaling-mode t)
      ;;        (global-set-key [?\C-\(] 'stig-paren-toggle-dingaling-mode)
      ;;        (global-set-key [?\C-\)] 'stig-paren-toggle-sexp-mode)

      (setq x-display-name "Emacs")

      (setq mark-even-if-inactive t);; ???
      (setq highlight-nonselected-windows nil);  3-Jan-1998
      ;;
      ;;   Automatically replacing of fore- and background.
      ;; (set-face-background 'region (cdr (assq 'foreground-color (frame-parameters ))))
      ;; (set-face-foreground 'region (cdr (assq 'background-color (frame-parameters ))))

      ;; 13-Dec-1997 ;; enabled font-lock-mode
      (global-font-lock-mode t)
      (if (not lse-emacs22-p);  1-Oct-2007
          (progn
            (setq font-lock-support-mode 'lazy-lock-mode); 17-Dec-1997
            (setq lazy-lock-minimum-size 1024); 19-Dec-1999
          )
      )
      (if (eq system-type 'windows-nt); 13-Nov-2002
          t
        (setq server-window ;  3-Jan-2000
          (lse-frame:make "LSE-Server"
                          nil nil
                          '((visibility . icon) (width . 80) (height . 30))
          )
        )
      )
    )
)
; (byte-recompile-directory "/swing/system/emacs" t)

(if (fboundp 'auto-compression-mode); 17-Dec-1997
    (auto-compression-mode 1)
)

(if (fboundp 'hscroll-global-mode); 17-Dec-1997
    (progn
      (hscroll-global-mode 1)
      (setq hscroll-mode-name " <>")
      (setq hscroll-margin 1)
      (setq hscroll-step-percent 5)
    )
)

;;; 11-Nov-2001
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0)
)
(if (fboundp 'delete-trailing-whitespace)
    (add-hook 'write-file-hooks 'delete-trailing-whitespace)
)
(if (boundp 'x-stretch-cursor)
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
(setq default-indicate-buffer-boundaries 'right)

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

;;; __END__ swing-default.el
