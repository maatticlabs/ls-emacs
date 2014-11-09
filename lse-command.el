;-*- coding: utf-8 -*-

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
;;;;     1-Aug-1994 (CT) lse-command:last added
;;;;     8-Sep-1994 (CT) Commands 'mail send' and 'mail read' added
;;;;     8-Sep-1994 (CT) case-fold parameter passed to lse-complete
;;;;    17-Sep-1994 (CT) lse-language:check added
;;;;    28-Mar-2007 (CT) `lse-menu:toggle-menu-bar` added
;;;;     3-Oct-2007 (CT) `lse-visit-file-new` used instead of homegrown code
;;;;     5-Oct-2007 (CT) Replace `search` by `search-forward` and
;;;;                     `search-reverse`
;;;;     9-Oct-2007 (CT) `lse-tpu:toggle-regexp` replaced by
;;;;                     `lse-tpu:change-search-mode`
;;;;    13-Oct-2007 (CT) Byte compile commands added
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-command)

(defvar lse-command:completion_buffer nil)
(defvar lse-command:initialized       nil)
(defvar lse-command:last              nil)

(defvar lse-command:cmd_list          nil)

(defun lse-command:add (name binding)
  (let ((old-binding (cdr (assoc name lse-command:cmd_list)))
       )
    (if old-binding
        (lse-remove-from-list lse-command:cmd_list (cons name old-binding))
    )
    (lse-command:add-new name binding)
  )
)

(defun lse-command:add-new (name binding)
  (lse-add-to-list lse-command:cmd_list (cons name binding))
  (setq lse-command:initialized nil)
)


(lse-command:add-new "byte compile LS-Emacs"       'lse-byte-compile:all)
(lse-command:add-new "byte compile current buffer" 'lse-byte-compile:current)
(lse-command:add-new "capitalize word"             'lse-tpu:capitalize-strongly)
(lse-command:add-new "change search mode"          'lse-tpu:change-search-mode)
(lse-command:add-new "check language"              'lse-language:check)
(lse-command:add-new "compile language"            'lse-language:compile)
(lse-command:add-new "emacs command"               'execute-extended-command)
(lse-command:add-new "enlarge window"              '(enlarge-window 4))
(lse-command:add-new "exit"                        'lse-tpu:exit)
(lse-command:add-new "goto buffer other window"    'lse-goto-buffer-other-window)
(lse-command:add-new "goto buffer"                 'lse-goto-buffer)
(lse-command:add-new "goto buffer/create"          'lse-goto-buffer+maybe-create)
(lse-command:add-new "goto file alternate"         'lse-visit-alternate-file)
(lse-command:add-new "goto file other window"      'lse-visit-file-other-window)
(lse-command:add-new "goto file"                   'lse-visit-file)
(lse-command:add-new "goto new file"               'lse-visit-file-new)
(lse-command:add-new "include file"                'lse-insert-file)
(lse-command:add-new "mail send"                   'mail) ;  8-Sep-1994
(lse-command:add-new "next buffer"                 'lse-goto-next-buffer)
(lse-command:add-new "previous buffer"             'lse-goto-prev-buffer)
(lse-command:add-new "quit"                        'lse-tpu:quit)
(lse-command:add-new "reload language"             'lse-language:reload)
(lse-command:add-new "revert buffer"               'lse-revert-buffer)
(lse-command:add-new "save some buffers"           'save-some-buffers)
(lse-command:add-new "search forward"              'lse-tpu:search-forward);  5-Oct-2007
(lse-command:add-new "search reverse"              'lse-tpu:search-reverse);  5-Oct-2007
(lse-command:add-new "show buffer"                 'lse-show-buffers)
(lse-command:add-new "shrink window"               '(shrink-window 4))
(lse-command:add-new "substitute all"              'lse-tpu:replace-all)
(lse-command:add-new "substitute"                  'lse-tpu:replace)
(lse-command:add-new "toggle menu bar all"         'lse-menu:toggle-menu-bar); 28-Mar-2007
(lse-command:add-new "toggle menu bar current"     'lse-frame:toggle-menu-bar); 28-Mar-2007
(lse-command:add-new "toggle rectangle"            'lse-tpu:toggle-rectangle)
(lse-command:add-new "use language"                'lse-language:use)
(lse-command:add-new "what line"                   'lse-show-position)
(lse-command:add-new "write selection"             'write-region)
