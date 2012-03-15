;-*- coding: iso-8859-15; -*-

;;;;unix_ms_filename_correspondency lse-frame:el lse_fram:el
;;;; Copyright (C) 1996-2012 Mag. Christian Tanzer. All rights reserved
;;;; Glasauergasse 32, A--1130 Wien, Austria. tanzer@swing.co.at

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
;;;;    lse-frame
;;;;
;;;; Purpose
;;;;    Provide frame related functions
;;;;
;;;; Revision Dates
;;;;    12-Aug-1996 (CT) Creation
;;;;    27-Aug-1996 (CT) Use `lse-user-initials-r' instead of
;;;;                     `lse-user-initials'
;;;;    27-Sep-1996 (CT) Use 'title for emacs versions newer than 19.30
;;;;     2-Oct-1996 (CT) Use numeric comparison for version
;;;;     2-Oct-1996 (CT) Don't use 'name for versions newer than 19.30
;;;;     3-Oct-1996 (CT) frame-title-format redefined
;;;;     3-Oct-1996 (CT) lse-frame:make added
;;;;    11-Oct-1996 (CT) icon-title-format  redefined
;;;;     5-Mar-1997 (CT) lse-frame:set-width and lse-frame:set-height added
;;;;     9-Apr-1998 (CT) lse-frame:make-small added
;;;;     1-May-1998 (CT) lse-frame:wbc-ht changed to 22
;;;;                     `max' used to restrict parameter of
;;;;                           set-frame-position for y-position
;;;;     3-Jan-2000 (CT) `lse-frame:make' returns the newly made frame
;;;;     7-Sep-2002 (CT) `lse-frame:set-width:<nnn>` added
;;;;     8-Sep-2002 (CT) `lse-frame:set-height:<nnn>` added
;;;;    28-Mar-2007 (CT) `lse-frame:large-height` changed from 72 to 66
;;;;    28-Mar-2007 (CT) `lse-frame:*-menu-bar` added
;;;;    17-Nov-2009 (CT) `lse-frame:set-parameter` and
;;;;                     `lse-frame:window-list` added
;;;;    17-Nov-2009 (CT) `lse-frame:desktop-save` and
;;;;                     `lse-frame:restore-saved-config` added and hooked
;;;;    17-Nov-2009 (CT) `frame-setup` added to save/restore
;;;;    18-Nov-2009 (CT) Don't save/restore `name` as it fixes the frame title
;;;;     8-Dec-2009 (CT) `lse-frame:select` and `lse-frame:list:...` added
;;;;     9-Dec-2009 (CT) `lse-frame:restore-saved-config` changed to
;;;;                     explicitly act on `visibility`
;;;;     9-Dec-2009 (CT) `lse-frame:list:restrict` added
;;;;    15-Mar-2012 (CT) Factor `lse-frame:title-prefix`, LSE_FRAME_TITLE_PREFIX
;;;;    ��revision-date�����
;;;;--

(provide 'lse-frame)
(require 'lse-face)
(require 'lse-hash)

;;; 15-Mar-2012
(defvar lse-frame:title-prefix (or (getenv "LSE_FRAME_TITLE_PREFIX") "LSE")
  "Prefix for frame title"
)

;;;  8-Sep-2002
(defvar lse-frame:std-width 80
  "Standard width of frames (in characters) created by LS-Emacs"
)

;;;  8-Sep-2002
(defvar lse-frame:wide-width 132
  "Width of wide frames (in characters) created by LS-Emacs"
)

;;;  8-Sep-2002
(defvar lse-frame:double-width 162
  "Width of double wide frames (in characters) created by LS-Emacs"
)

;;;  8-Sep-2002
(defvar lse-frame:std-height 48
  "Standard height of frames (in lines) created by LS-Emacs"
)

;;;  8-Sep-2002
(defvar lse-frame:small-height 30
  "Height of small frames (in lines) created by LS-Emacs"
)

;;;  8-Sep-2002
(defvar lse-frame:large-height 66 ;; 28-Mar-2007 s/72/66/
  "Height of large frames (in lines) created by LS-Emacs"
)

;;; 12-Aug-1996
(defun lse-set-frame-title (nam &optional fram)
  (if lse-emacsX-p
    (modify-frame-parameters
        (or fram (selected-frame))
        (list
          (cons 'name  nam)
          (cons 'title nam)
        )
    )
  )
; lse-set-frame-title
)

;;; 12-Aug-1996
(defun lse-set-hosted-frame-title (&optional nam fram)
  (interactive)
  (or nam (setq nam lse-frame:title-prefix))
  (lse-set-frame-title
      (concat nam " " (lse-system-name) "::" (lse-user-initials-r))
      fram
  )
; lse-set-hosted-frame-title
)

;;; 12-Aug-1996
(defun lse-set-shorthosted-frame-title (&optional nam fram)
  (interactive)
  (or nam (setq nam lse-frame:title-prefix))
  (lse-set-frame-title
      (concat nam
              " "
              (substring (lse-system-name) 0 1)
              (substring (lse-system-name) -1 )
              "::"
              (lse-user-initials-r)
      )
      fram
  )
; lse-set-shorthosted-frame-title
)

;;;  3-Oct-1996
(defun lse-frame:n ()
  (length (frame-list))
; lse-frame:n
)

;;;  3-Oct-1996
(defun lse-frame:make (&optional nam pos siz alist)
  "Make a frame named `nam' at position `pos' with size `siz'. `alist is passed to make-frame'"
  (let ((fram (make-frame alist)))
    (and (consp   pos) (set-frame-position fram (car pos) (cdr pos)))
    (and (consp   siz) (set-frame-size     fram (car siz) (cdr siz)))
    (and (stringp nam) (lse-set-shorthosted-frame-title nam fram))
    fram
  )
; lse-frame:make
)

;;;  9-Apr-1998
(defun lse-frame:make-small (&optional ht nam alist)
  "Make a small frame with height `ht' (default: 30)"
  (interactive "p")
  (if (eq ht 1) (setq ht lse-frame:small-height))
  (lse-frame:make nam nil (cons lse-frame:std-width ht) alist)
; lse-frame:make-small
)

(setq frame-title-format (list (concat lse-frame:title-prefix " %b")))
(setq icon-title-format  (list (concat lse-frame:title-prefix " %b")))

;;;  5-Mar-1997
(defun lse-frame:parameter (nsym &optional fram)
  (or fram (setq fram (selected-frame)))
  (cdr (assoc nsym (frame-parameters fram)))
; lse-frame:parameter
)

;;; 17-Nov-2009
(defun lse-frame:set-parameter (nsym value &optional fram)
  (or fram (setq fram (selected-frame)))
  (modify-frame-parameters fram (list (cons nsym value)))
; lse-frame:set-parameter
)

;;;  5-Mar-1997
(defvar lse-frame:wbc-wd -5)    ; window border correction for width
(defvar lse-frame:wbc-ht +22)   ; window border correction for height
                                ;    1-May-1998 `22' ; instead of `12'

(defvar lse-frame:save-pos-table-w nil)
(defvar lse-frame:save-wd-table nil)
;;; (setq   lse-frame:save-pos-table-w nil)

;;;  5-Mar-1997
(defun lse-frame:set-width (wd &optional fram)
  "Set width of frame to `wd'"
  (interactive "NFrame-width: ")
  (or fram (setq fram (selected-frame)))
  (let ((old-wd (frame-width fram))
        (saved-wd (if (assoc fram lse-frame:save-wd-table)
                      (cdr (assoc fram lse-frame:save-wd-table))
                    0
                  )
        )
       )
    (or (> wd 0)  (setq wd saved-wd))
    (if (eq wd old-wd)
        t
      (set-frame-size fram wd (frame-height fram))
      (or (assoc fram lse-frame:save-wd-table)
          (setq lse-frame:save-wd-table
                (cons (cons fram old-wd) lse-frame:save-wd-table)
          )
      )
      (let ((delta (- (x-display-pixel-width) (frame-pixel-width fram)))
           )
        (if (> wd old-wd)
            (if (> (lse-frame:parameter 'left fram) delta)
                (progn
                  (or (assoc fram lse-frame:save-pos-table-w)
                      (setq lse-frame:save-pos-table-w
                            (cons (cons fram (lse-frame:parameter 'left fram))
                                  lse-frame:save-pos-table-w
                            )
                      )
                  )
                  (set-frame-position fram
                      (- delta
                         (or (lse-frame:parameter 'scroll-bar-width fram) 0)
                      )
                      (max (- (lse-frame:parameter 'top fram) lse-frame:wbc-ht)
                           lse-frame:wbc-ht
                      )
                  )
                )
            )
          (let ((saved (assoc fram lse-frame:save-pos-table-w))
               )
            (if saved
                (progn
                  (set-frame-position fram (cdr saved)
                      (max (- (lse-frame:parameter 'top fram) lse-frame:wbc-ht)
                           lse-frame:wbc-ht
                      )
                  )
                  (delete saved lse-frame:save-pos-table-w)
                )
            )
          )
        )
      )
    )
  )
; lse-frame:set-width
)

;;;  7-Sep-2002
(defun lse-frame:set-width:80 (wd &optional fram)
  "Set width of frame `fram` to prefix argument or 80 (standard width)"
  (interactive "P")
  (lse-frame:set-width (or wd lse-frame:std-width) fram)
; lse-frame:set-width:80
)

;;;  7-Sep-2002
(defun lse-frame:set-width:132 (wd &optional fram)
  "Set width of frame `fram` to prefix argument or 132 (wide width)"
  (interactive "P")
  (lse-frame:set-width (or wd lse-frame:wide-width) fram)
; lse-frame:set-width:132
)

;;;  7-Sep-2002
(defun lse-frame:set-width:162 (wd &optional fram)
  "Set width of frame `fram` to prefix argument or 162 (double width)"
  (interactive "P")
  (lse-frame:set-width (or wd lse-frame:double-width) fram)
; lse-frame:set-width:162
)

;;;  5-Mar-1997
(defvar lse-frame:save-pos-table-h nil)
(defvar lse-frame:save-ht-table    nil)

;;;  5-Mar-1997
(defun lse-frame:set-height (ht &optional fram)
  "Set height of frame to `ht'"
  (interactive "NFrame-height: ")
  (or fram (setq fram (selected-frame)))
;;;  8-Sep-2002 (or (< ht lse-frame:large-height) (setq ht lse-frame:large-height))
  (let ((old-ht   (frame-height fram))
        (saved-ht (if (assoc fram lse-frame:save-ht-table)
                      (cdr (assoc fram lse-frame:save-ht-table))
                    0
                  )
        )
       )
    (or (> ht 0) (setq ht saved-ht))
    (if (eq ht old-ht)
        t
      (set-frame-size fram (frame-width fram) ht)
      (or (assoc fram lse-frame:save-ht-table)
          (setq lse-frame:save-ht-table
                (cons (cons fram old-ht) lse-frame:save-ht-table)
          )
      )
      (let ((delta (- (x-display-pixel-height) (frame-pixel-height fram)))
           )
        (if (> ht old-ht)
            (if (> (lse-frame:parameter 'top fram) delta)
                (progn
                  (or (assoc fram lse-frame:save-pos-table-h)
                      (setq lse-frame:save-pos-table-h
                            (cons (cons fram (lse-frame:parameter 'top fram))
                                  lse-frame:save-pos-table-h
                            )
                      )
                  )
                  (set-frame-position fram
                      (lse-frame:parameter 'left fram)
                      (- delta lse-frame:wbc-wd)
                  )
                )
            )
          (let ((saved (assoc fram lse-frame:save-pos-table-h))
               )
            (if saved
                (progn
                  (set-frame-position fram
                                      (lse-frame:parameter 'left fram)
                                      (cdr saved)
                  )
                  (delete saved lse-frame:save-pos-table-h)
                )
            )
          )
        )
      )
    )
  )
; lse-frame:set-height
)

;;;  8-Sep-2002
(defun lse-frame:set-height:48 (ht &optional fram)
  "Set height of frame `fram` to prefix argument or 48 (standard height)"
  (interactive "P")
  (lse-frame:set-height (or ht lse-frame:std-height) fram)
; lse-frame:set-height:48
)

;;;  8-Sep-2002
(defun lse-frame:set-height:30 (ht &optional fram)
  "Set height of frame `fram` to prefix argument or 30 (small height)"
  (interactive "P")
  (lse-frame:set-height (or ht lse-frame:small-height) fram)
; lse-frame:set-height:30
)

;;;  8-Sep-2002
(defun lse-frame:set-height:72 (ht &optional fram)
  "Set height of frame `fram` to prefix argument or 72 (large height)"
  (interactive "P")
  (lse-frame:set-height (or ht lse-frame:large-height) fram)
; lse-frame:set-height:72
)

;;; 28-Mar-2007
(defun lse-frame@change_menu_bar (status &optional fram)
  (or fram (setq fram (selected-frame)))
  (modify-frame-parameters fram (list (cons 'menu-bar-lines status)))
; lse-frame@change_menu_bar
)

;;; 28-Mar-2007
(defun lse-frame:disable-menu-bar (&optional fram)
  "Disable menu bar for frame"
  (interactive)
  (lse-frame@change_menu_bar 0)
; lse-frame:disable-menu-bar
)

;;; 28-Mar-2007
(defun lse-frame:enable-menu-bar (&optional fram)
  "Disable menu bar for frame"
  (interactive)
  (lse-frame@change_menu_bar 1)
; lse-frame:enable-menu-bar
)

;;; 28-Mar-2007
(defun lse-frame:toggle-menu-bar (&optional fram)
  "Toggle menu bar for frame"
  (interactive)
  (lse-frame@change_menu_bar
    (- 1 (frame-parameter fram 'menu-bar-lines))
  )
; lse-frame:toggle-menu-bar
)

;;; 17-Nov-2009
(defun lse-frame:window-list (&optional fram)
  "Returns the list of windows of frame"
  (or fram (setq fram (selected-frame)))
  (let (windows)
    (walk-windows
      (lambda (w) (setq windows (cons w windows)))
      "no-mini-buf"
      fram
    )
    windows
  )
; lse-frame:window-list
)

;;;; Saving of frame information
;;; 17-Nov-2009
(add-hook 'after-init-hook
  (function
    (lambda () (lse-frame:set-parameter 'root-p t))
  )
)

;;; 17-Nov-2009
(defun lse-frame:desktop-save ()
  (let ((out (current-buffer)))
    (insert "\n;; Frame configuration section:\n")
    (insert "(setq lse-frame:saved-config '(" )
    (dolist (frame (frame-list))
      (lse-frame:save-one frame out)
    )
    (insert "))\n\n")
  )
; lse-frame:desktop-save
)

;;; 17-Nov-2009
(defun lse-frame:restore-saved-config ()
  (when (boundp 'lse-frame:saved-config)
    (let ((start-frame (selected-frame))
         )
      (dolist (frame-infos lse-frame:saved-config)
        (let* ((root-p       (nth 0 frame-infos))
               (frame-params (nth 1 frame-infos))
               (window-infos (nth 2 frame-infos))
               (frame-setup  (nth 3 frame-infos))
               (visibility   (cdr (assoc 'visibility frame-params)))
               (first t)
               frame
             )
          (save-window-excursion
            (if root-p
                (progn
                  (select-frame start-frame)
                  (modify-frame-parameters start-frame frame-params)
                )
              (select-frame (make-frame frame-params))
            )
            (let ((frame (selected-frame)))
              (if frame-setup
                  (eval frame-setup)
                (dolist (window-info window-infos)
                  (unless first
                    (lse-split-window)
                    (lse-previous-window)
                  )
                  (lse-goto-buffer+maybe-create (nth 0 window-info))
                  (goto-char                    (nth 1 window-info))
                  (when (and root-p first)
                    (lse-set-home-mark-global (point-marker))
                  )
                  (setq first nil)
                )
              )
              (cond ((equal visibility 'icon) (iconify-frame))
                    ((not   visibility)       (make-frame-invisible))
              )
            )
          )
        )
      )
      (select-frame-set-input-focus start-frame)
    )
    (makunbound 'lse-frame:saved-config)
  )
; lse-frame:restore-saved-config
)

;;; 17-Nov-2009
(defun lse-frame:save-one (frame out)
  (let* ((params      (frame-parameters frame))
         ;; We use`Font` instead of `font` because bloody Emacs 23.1.1 converts
         ;; "6x13" to a font-string that gives an error on restore
         (Font        (cdr (assoc 'Font        params)))
         (frame-setup (cdr (assoc 'frame-setup params)))
         (height      (cdr (assoc 'height      params)))
         (left        (cdr (assoc 'left        params)))
         (root-p      (cdr (assoc 'root-p      params)))
         (top         (cdr (assoc 'top         params)))
         (visibility  (cdr (assoc 'visibility  params)))
         (width       (cdr (assoc 'width       params)))
         buffer
         buffer-name
         window-infos
        )
    (unless frame-setup
      (dolist (window (lse-frame:window-list frame))
        (setq buffer      (window-buffer window))
        (setq buffer-name (buffer-name   buffer))
        (when (and buffer-name (lse-buffer:is-lse-buffer buffer))
          (setq window-infos
            (cons
              (list buffer-name
                (save-window-excursion (select-window window) (point))
              )
              window-infos
            )
          )
        )
      )
    )
    (when (or frame-setup window-infos)
      (let ((fpl
              (list
                (cons 'height     height)
                (cons 'left       left)
                (cons 'top        top)
                (cons 'visibility visibility)
                (cons 'width      width)
              )
            )
            result
           )
        (when Font
          ;; We set `Font` because bloody Emacs 23.1.1 converts "6x13" to a
          ;; font-string that gives an error on restore
          (setq fpl (cons (cons 'font  Font)  fpl))
          (setq fpl (cons (cons 'Font  Font)  fpl))
        )
        (setq  result (list root-p fpl window-infos frame-setup))
        (print result out)
      )
    )
  )
; lse-frame:save-one
)

(add-hook 'desktop-after-read-hook 'lse-frame:restore-saved-config)
(add-hook 'desktop-save-hook       'lse-frame:desktop-save)

;;;; Commands for frame management
;;;  8-Dec-2009
(defun lse-frame:select (frame)
  (make-frame-visible frame)
  (raise-frame frame)
  (select-frame-set-input-focus frame)
; lse-frame:select
)

;;;  8-Dec-2009
(defvar lse-frame:list:buffer      nil)
(defvar lse-frame:list:buffer-name " $LSE-Frame-List$")
(defvar lse-frame:list:buffer-map  (lse-hash:mms:new))
(defvar lse-frame:list:keymap      nil)
(defvar lse-frame:list:overlay     nil)

;;;  8-Dec-2009
(defun lse-frame:list:abort ()
  (interactive)
  (let ((to-hide (eq (cdr (assq 'lse-frame-list-p (frame-parameters))) t))
       )
    (when to-hide (make-frame-invisible))
  )
; lse-frame:list:abort
)

;;;  8-Dec-2009
(defun lse-frame:list:define-keys ()
  (let ((lmap (current-local-map)))
    (local-set-key [?\A-g]          'lse-frame:list:abort)
    (local-set-key [?\C-g]          'lse-frame:list:abort)

    (local-set-key [gold ?f]        'lse-frame:list:restrict)
    (local-set-key [blue ?f]        'lse-frame:list:unstrict)
    (local-set-key [?r]             'lse-frame:list:restrict)
    (local-set-key [?u]             'lse-frame:list:unstrict)

    (local-set-key [mouse-2]        'lse-frame:list:select)
    (local-set-key [return]         'lse-frame:list:select)
    (local-set-key [select]         'lse-frame:list:select)
    (local-set-key [tab]            'lse-frame:list:select)
  )
; lse-frame:list:define-keys
)

;;;  9-Dec-2009
(defun lse-frame:list:restrict (&optional buffer)
  "Restrict display to frames containing `buffer`"
  (interactive)
  (setq buffer
    (or buffer
      (lse-buffer:read-name "Restrict to buffer: " (lse-tpu:selection) t)
    )
  )
  (lse-frame:list:unstrict)
  (let ((inhibit-read-only t)
        (name buffer)
       )
    (when (and name (not (equal name "")))
      (let ((relevants (lse-hash:mms:get lse-frame:list:buffer-map name)))
        (goto-char (point-min))
        (put-text-property (point-min) (point-max) 'invisible t)
        (while (not (eobp))
          (let* ((pos   (point))
                 (frame (get-text-property pos 'frame))
                 (head
                   (or (previous-single-property-change pos 'frame) (point-min))
                 )
                 (tail
                   (or (next-single-property-change     pos 'frame) (point-max))
                 )
                )
            (when (and frame (member frame relevants))
              (put-text-property head tail 'invisible nil)
            )
            (goto-char tail)
          )
        )
      )
    )
    (set-buffer-modified-p nil)
  )
; lse-frame:list:restrict
)

;;;  8-Dec-2009
(defun lse-frame:list:select ()
  (interactive)
  (let ((frame   (get-text-property (point) 'frame))
        (window  (get-text-property (point) 'window))
        (to-hide (eq (cdr (assq 'lse-frame-list-p (frame-parameters))) t))
       )
    (when to-hide (make-frame-invisible))
    (when frame   (lse-frame:select frame))
    (when window  (select-window    window))
  )
; lse-frame:list:select
)

;;;  8-Dec-2009
(defun lse-frame:list:setup-buffer ()
  (unless (and (bufferp     lse-frame:list:buffer)
               (buffer-name lse-frame:list:buffer)
          )
    (setq lse-frame:list:buffer  (get-buffer-create lse-frame:list:buffer-name))
    (setq lse-frame:list:keymap  (make-sparse-keymap))
    (setq lse-frame:list:overlay (make-overlay 1 1))
    (save-current-buffer
      (set-buffer      lse-frame:list:buffer)
      (use-local-map   lse-frame:list:keymap)
      (overlay-put     lse-frame:list:overlay 'face 'lse-face:completion)
      (lse-frame:list:define-keys)
      (set (make-local-variable 'hl-line-face) 'lse-face:fl:current)
      (hl-line-mode t)
      (setq buffer-read-only t)
    )
  )
  (let ((window (get-buffer-window lse-frame:list:buffer 0)))
    (if window
        (let ((frame (window-frame window)))
          (make-frame-visible frame)
          (raise-frame frame)
        )
      (let* ((frame  (with-current-buffer lse-frame:list:buffer (make-frame)))
             (window (frame-selected-window frame))
            )
        (set-window-buffer      window lse-frame:list:buffer)
        (set-window-dedicated-p window t)
        (lse-frame:set-parameter 'lse-frame-list-p t frame)
      )
    )
  )
  (lse-hash:mms:clear lse-frame:list:buffer-map)
; lse-frame:list:setup-buffer
)

;;;  8-Dec-2009
(defun lse-frame:list:show ()
  "Show a list of frames with the buffers displayed inside."
  (interactive)
  (lse-frame:list:setup-buffer)
  (save-excursion
    (let ((inhibit-read-only t))
      (set-buffer lse-frame:list:buffer)
      (erase-buffer)
      (let ((standard-output lse-frame:list:buffer))
        (dolist (frame (frame-list))
          (let* ((head (point))
                 (params      (frame-parameters frame))
                 (height      (cdr (assoc 'height      params)))
                 (left        (cdr (assoc 'left        params)))
                 (name        (cdr (assoc 'name        params)))
                 (top         (cdr (assoc 'top         params)))
                 (width       (cdr (assoc 'width       params)))
                )
            (princ (format "%-40s %dx%d+%d+%d\n" name height width left top))
            (add-text-properties head (point) (list 'face  'lse-face:fl:frame))
            (dolist (window (lse-frame:window-list frame))
              (let* ((head (point))
                     (buffer (window-buffer window))
                     (bufnam (buffer-name   buffer))
                     (height (window-height window))
                     (top    (nth 1         (window-edges window)))
                    )
                (when (and bufnam (lse-buffer:is-lse-buffer buffer))
                  (princ (format "    %-50s %4d @%4d\n" bufnam height top))
                  (add-text-properties head (point)
                    (list
                      'face   'lse-face:fl:buffer
                      'window window
                    )
                  )
                  (lse-hash:mms:put lse-frame:list:buffer-map bufnam frame)
                )
              )
            )
            (add-text-properties head (point) (list 'frame frame))
            (princ "\n")
          )
        )
        (goto-char (point-min))
        (delete-blank-lines)
        (set-buffer-modified-p nil)
      )
    )
  )
; lse-frame:list:show
)

;;;  9-Dec-2009
(defun lse-frame:list:unstrict ()
  "Don't restrict display to frames containing a specific buffer."
  (interactive)
  (let ((inhibit-read-only t)
       )
    (put-text-property (point-min) (point-max) 'invisible nil)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
  )
; lse-frame:list:unstrict
)

;;; __END__ lse-frame.el
