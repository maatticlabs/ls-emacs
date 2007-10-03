;-*- unibyte: t; coding: iso-8859-1; -*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work

;;;;unix_ms_filename_correspondency lse-command:el lse_cmd:el
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
;;;;    lse-command
;;;;
;;;; Purpose
;;;;    Functions providing a completion interface for use of emacs commands
;;;;
;;;; Revision Dates
;;;;    26-May-1994 (CT) Creation (of comment)
;;;;    26-May-1994 (CT) Interactive functions moved to lse-interactive
;;;;    18-Jun-1994 (CT) Factored out swing-specific stuff
;;;;     1-Aug-1994 (CT) lse_command:last added
;;;;     8-Sep-1994 (CT) Commands 'mail send' and 'mail read' added
;;;;     8-Sep-1994 (CT) case-fold parameter passed to lse-complete
;;;;    17-Sep-1994 (CT) lse-language:check added
;;;;    28-Mar-2007 (CT) `lse-menu:toggle-menu-bar` added
;;;;     3-Oct-2007 (CT) `lse-visit-file-new` used instead of homegrown code
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-command)

(defvar lse_command:completion_buffer nil)
(defvar lse_command:initialized       nil)
(defvar lse_command:last              nil)

(defvar lse_command:cmd_list          nil)

(defun lse-command:add (name binding)
  (let ((old-binding (cdr (assoc name lse_command:cmd_list)))
       )
    (if old-binding
        (lse-remove-from-list lse_command:cmd_list (cons name old-binding))
    )
    (lse-command:add-new name binding)
  )
)

(defun lse-command:add-new (name binding)
  (lse-add-to-list lse_command:cmd_list (cons name binding))
  (setq lse_command:initialized nil)
)


(lse-command:add-new "capitalize word"          'lse-tpu:capitalize-strongly)
(lse-command:add-new "check language"           'lse-language:check)
(lse-command:add-new "compile language"         'lse-language:compile)
(if lse-emacs19-p
    (lse-command:add-new "emacs command"        'execute-extended-command)
  (lse-command:add-new "emacs command"          'gmhist-execute-extended-command)
)
(lse-command:add-new "enlarge window"           '(enlarge-window 4))
(lse-command:add-new "exit"                     'lse-tpu:exit)
(lse-command:add-new "goto buffer other window" 'lse-goto-buffer-other-window)
(lse-command:add-new "goto buffer"              'lse-goto-buffer)
(lse-command:add-new "goto buffer/create"       'lse-goto-buffer+maybe-create)
(lse-command:add-new "goto file alternate"      'lse-visit-alternate-file)
(lse-command:add-new "goto file other window"   'lse-visit-file-other-window)
(lse-command:add-new "goto file"                'lse-visit-file)
(lse-command:add-new "goto new file"            'lse-visit-file-new)
(lse-command:add-new "include file"             'lse-insert-file)
(lse-command:add-new "next buffer"              'lse-goto-next-buffer)
(lse-command:add-new "previous buffer"          'lse-goto-prev-buffer)
(lse-command:add-new "quit"                     'lse-tpu:quit)
(lse-command:add-new "reload language"          'lse-language:reload)
(lse-command:add-new "revert buffer"            'lse-revert-buffer)
(lse-command:add-new "save some buffers"        'save-some-buffers)
(lse-command:add-new "search"                   'lse-tpu:search)
(lse-command:add-new "mail read"                'rmail);  8-Sep-1994
(lse-command:add-new "mail send"                'mail) ;  8-Sep-1994
(lse-command:add-new "show buffer"              'lse-show-buffers)
(lse-command:add-new "shrink window"            '(shrink-window 4))
(lse-command:add-new "substitute all"           'lse-tpu:replace-all)
(lse-command:add-new "substitute"               'lse-tpu:replace)
(lse-command:add-new "toggle menu bar current"  'lse-frame:toggle-menu-bar); 28-Mar-2007
(lse-command:add-new "toggle menu bar all"      'lse-menu:toggle-menu-bar); 28-Mar-2007
(lse-command:add-new "toggle rectangle"         'lse-tpu:toggle-rectangle)
(lse-command:add-new "toggle regexp"            'lse-tpu:toggle-regexp)
(lse-command:add-new "use language"             'lse-language:use)
(lse-command:add-new "what line"                'lse-show-position)
(lse-command:add-new "write selection"          'write-region)
