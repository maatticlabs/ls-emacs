;-*-unibyte: t;-*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work
 
;;;;unix_ms_filename_correspondency lse-compilation:el lse_cmpi:el
;;;; (c) 1995 Swing Informationssysteme GmbH. All rights reserved.

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
;;;;    lse-compilation
;;;;
;;;; Purpose
;;;;    LS-Emacs interface to Emacs source file compilation
;;;;
;;;; Revision Dates
;;;;    20-Feb-1995 (CT) Creation
;;;;    23-Feb-1995 (CT) Save window configuration before executing
;;;;                     compile-command or grep
;;;;    17-Mar-1995 (CT) Use lse-push-window-configuration to save window
;;;;                     configuration 
;;;;    29-Mar-1995 (CT) Pop window configuration before pushing one
;;;;    31-Mar-1995 (CT) Don't pop window configuration before pushing one
;;;;     9-Jun-1995 (CT) lse-set-compile-command added
;;;;     7-Sep-1995 (CT) Remove key bindings for Space and Delete from
;;;;                     compilation keymap (they are too stupid!!!)
;;;;     9-Sep-1995 (CT) lse-compilation-mode-hook added
;;;;    10-Jan-1998 (CT) Moved Control-Keys to Alt-Keys
;;;;    ««revision-date»»···
;;;;--
(provide 'lse-compilation)

(require 'compile)
(require 'lse-tpu-keys-v19)

(let ((map compilation-minor-mode-map))
  (define-key map           [?\A-f]    'compile-goto-error)
  (define-key map           [?\A-n]    'compilation-next-error)
  (define-key map           [?\A-p]    'compilation-previous-error)
  (define-key map           [?\ ]      nil);  7-Sep-1995
  (define-key map           "\177"     nil);  7-Sep-1995
  (lse-define-alpha-key map [gold] "b" 'lse-compilation:goto-last-mark)
  (lse-define-alpha-key map [gold] "p" 'compilation-previous-file)
  (lse-define-alpha-key map [gold] "n" 'compilation-next-file)
)

;;;  9-Sep-1995 
(defun lse-compilation-mode-hook ()
  (let ((map (current-local-map)))
    (define-key map           [?\A-f]    'compile-goto-error)
    (define-key map           [?\A-n]    'compilation-next-error)
    (define-key map           [?\A-p]    'compilation-previous-error)
    (define-key map           [?\ ]      nil);  7-Sep-1995
    (define-key map           "\177"     nil);  7-Sep-1995
    (lse-define-alpha-key map [gold] "b" 'lse-compilation:goto-last-mark)
    (lse-define-alpha-key map [gold] "p" 'compilation-previous-file)
    (lse-define-alpha-key map [gold] "n" 'compilation-next-file)
  )
  (auto-fill-mode)
  (lse-tpu:toggle-newline-and-indent)
; lse-compilation-mode-hook
)

(add-hook 'compilation-mode-hook 'lse-compilation-mode-hook); 9-Sep-1995 

(defvar lse-language:c:compile-switch nil);  2-Mar-1995 

(defvar lse-compilation:last@mark     nil); 14-Dec-1997

(defun lse-compilation:next-error (&optional prefix)
  "Visit the locus of the next compiler error message or grep match"
  (interactive "P")
  (let ((lse-compilation:cp (point-marker))
       )
    (condition-case nil
        (unwind-protect
            (next-error prefix)
          (setq lse-compilation:last@mark lse-compilation:cp)
        )
      (error
        (lse-goto-mark lse-compilation:cp 'ignore)
      )
    )
  )
; lse-compilation:next-error
)

(defun lse-compilation:goto-last-mark ()
  "Return to position before last next-error command"
  (interactive)
  (if (markerp lse-compilation:last@mark)
      (lse-goto-mark lse-compilation:last@mark 'ignore)
  )
; lse-compilation:goto-last-mark
)

(defun lse-compile-defun ()
  "Evaluates the current lisp defun"
  (interactive)
  (save-excursion 
    (beginning-of-defun)
    (eval-defun nil)
  )
; lse-compile-defun
)

(defun lse-compile ()
  "Compiles the current buffer"
  (interactive)
  (cond ((equal (lse-file-name-extension (buffer-name(current-buffer))) ".lse")
         (lse-language:compile)
        )
        ((equal mode-name "Emacs-Lisp")
         (eval-current-buffer)
        )
        (t
         (let (lse-window:wb-assoc)
           ;; 17-Mar-1995;; (lse-window:make-wb-list) (lse-window:save-temp-hidden)
           (lse-push-window-configuration)
           (compile compile-command)
         )
        )
  )
; lse-compile
)

(defvar lse-compilation:grep-command "egrep -n -i "
        "Default command used by lse-grep"
)

(defun lse-grep (grep-args)
  "Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to.

This command uses a special history list for its arguments, so you can
easily repeat a grep command."
  (interactive
   (list (read-from-minibuffer ""
		lse-compilation:grep-command nil nil 'grep-history
         )
   )
  )
  (let (lse-window:wb-assoc)
    ;; 17-Mar-1995 ; (lse-window:make-wb-list) (lse-window:save-temp-hidden)
    (lse-push-window-configuration)
    (grep grep-args)
  )
; lse-grep
) 

;;;  9-Jun-1995 
(defun lse-set-compile-command (cc)
  "Change compile command for current buffer"
  (interactive
    (list (read-from-minibuffer
             "Compile command: " compile-command nil nil 'compile-history
             
          )
    )
  )
  (set (make-local-variable 'compile-command) cc)
; lse-set-compile-command
)

