;-*- unibyte: t; coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work
 
;;;; Copyright (C) 1997 Mag. Christian Tanzer. All rights reserved.
;;;; Glasauergasse 32, A--1130 Wien, Austria. tanzer.co.at
;;;; Glasauergasse 32, A--1130 Wien, Austria

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
;;;;    lse-byte-compile
;;;;
;;;; Purpose
;;;;    Compile ls-emacs elisp source files which are compilable without errors
;;;;
;;;; Revision Dates
;;;;    14-Dec-1997 (CT) Creation
;;;;    ««revision-date»»···
;;;;--
;;;;

(setq lse-elisp-source-dir "/swing/emacs/")

(byte-compile-file (concat lse-elisp-source-dir "lse-basics.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-list-util.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-buffer.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-buffer-list.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-command.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-comment.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-compilation.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-completion.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-deep-fill-in.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-define.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-editing.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-face.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-file.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-fill-in.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-fill-in-info.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-fill-in-history.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-fill-in--delimiters.el"))
;;; compilation of lse-fill-in--search.el goes into a loop
;;; (byte-compile-file (concat lse-elisp-source-dir "lse-fill-in--search.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-flat-fill-in.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-frame.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-indent.el"))
;;; after compilation of lse-interactive.el flat fill-ins don't work !!!!????
;;; (byte-compile-file (concat lse-elisp-source-dir "lse-interactive.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-keys.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-kill.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-language.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-learn-key.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-mark-stack.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-menu.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-mode-alist.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-range.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-session.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-tpu.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-window.el"))
(byte-compile-file (concat lse-elisp-source-dir "lse-tpu-keys.el"))
