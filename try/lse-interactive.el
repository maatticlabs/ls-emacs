;;;; (c) 1994 Swing Informationssysteme GmbH. All rights reserved.
;;;;++
;;;; Name
;;;;    lse-interactive
;;;;
;;;; Purpose
;;;;    Provides all interactive commands for emacs lse system
;;;;
;;;; Revision Dates
;;;;    26-May-1994 (CT) Creation
;;;;    12-Jun-1994 (CT) Auto-replication added
;;;;    15-Jun-1994 (CT) Disable overwrite-mode during expansion
;;;;    26-Jun-1994 (CT) Set lse-flat-fill-in:no-vanguard while replacing
;;;;    27-Jun-1994 (CT) lse-flush-replacement added
;;;;     1-Aug-1994 (CT) lse_command:last added to lse-command:do
;;;;     8-Sep-1994 (CT) case-fold parameter passed to lse-complete
;;;;    13-Sep-1994 (CT) lse-language:fill-in-refs & lse-language:fill-in-defs
;;;;                     in lse-language:compile
;;;;    17-Sep-1994 (CT) Use lse-script-dir instead of literal path 
;;;;    30-Sep-1994 (CT) Removed optional parameter `quiet' from lse-expand-token
;;;;    21-Oct-1994 (CT) lse-set-tab-increment defined
;;;;    22-Jan-1995 (CT) Moved body of lse-unkill-fill-in to lse-kill.el
;;;;    22-Jan-1995 (CT) Moved body of lse-replicate-fill-in and
;;;;                     lse-replicate-fill-in-by-older to lse-flat-fill-in.el 
;;;;    22-Jan-1995 (CT) lse-replicate-fill-in chooses between
;;;;                     lse_replicate_fill_in and
;;;;                     lse_replicate_fill_in_by_older  
;;;;--
(provide 'lse-interactive)

;;;++ 
;;; user level commands for expansion
;;;--
(defun lse-expand-token ()
  "Expand the current token (word(s) before the cursor)."
  (interactive "*P")
  (let ((wcount 5)
        overwrite-mode         ; 15-Jun-1994 ; don't overwrite during expansion
        result
        lse_current_fill-in    ; 30-Sep-1994 ; just in case this is called
                               ;               during expansion of a fill-in
       )
    (while (and (not result) (> wcount 0))
      (or (setq result 
             (lse_expand_token:try lse-tpu:blank-sep-word-chars wcount)
          )
          (setq result (lse_expand_token:try lse-tpu:word-chars wcount))
      )
      (setq wcount (1- wcount))
    )
    result
  )
; lse-expand-token
)

(defun lse-expand ()
  "Expand fill-in (if inside) or token (if previous word is one). "
  ;; primary expansion function for user (this is bound to the appropriate key)
  (interactive "*")
  (let ((name (lse_inside_fill-in))
        overwrite-mode         ; 15-Jun-1994 ; don't overwrite during expansion
        th tt
        p
       )
    ;; this save-*-excursion fuss is necessary to avoid change of
    ;; position in other windows showing the same buffer
    ;; (shame on emacs!!! at least I don't understand why this happens)
    (save-window-excursion
      (save-excursion
        (if (eobp)
            (save-excursion 
              (lse-fill-in-insert " \n")
              (setq th (point-marker))
              (lse-fill-in-insert
                   " \n"
                   (or lse_comment_head_delim "")
                   " end of buffer " (buffer-name (current-buffer))
                   (or lse_comment_tail_delim "")
                   "\n"
              )
              (setq tt (point-marker))
            )
        )
        (cond (name
               (lse-flat-fill-in:expand 
                   (lse-fill-in:symbol lse_current_fill-in) name
               )
              )
              ((lse-expand-token))
              (t (message "Nothing to expand"))
        )
        (setq p (point-marker))
        (and th tt (delete-region th tt))
        (setq th nil)
        (setq tt nil)
      ); save-excursion
    ); save-window-excursion
    (goto-char p)
    (setq p nil)
  ); let
; lse-expand
)

;;;++
;;; user level commands for replacement
;;;-- 
(defun lse-replace-fill-in ()
  "Start or finish fill-in replacement."
  (interactive "*")
  (if lse_replaced_fill-in
      (lse_shut_fill-in_replacement)
    (lse-tpu:unselect); avoid surprises due to spurious selection
    (if (lse_start_replacement_if_in_fill-in)
        t
      (error "No fill-in to replace")
    )
  )
)

(defun lse-flush-replacement ()
  "Flush pending replacement (use in emergencies)"
  (interactive)
  (setq lse_replaced_fill-in              nil)
  (setq lse-flat-fill-in:open-replacement nil)
  (lse-tpu:update-mode-line)
; lse-flush-replacement
)
;;;++
;;; user level commands for fill-in replication
;;;--
(defun lse-replicate-fill-in ()
  "Replicate the value of the last occurence of the fill-in."
  (interactive "*")
  (if lse_replaced_fill-in             ; 22-Jan-1995 
      (lse_replicate_fill_in_by_older) ; 22-Jan-1995 
    (lse_replicate_fill_in)
  )
; lse-replicate-fill-in
)

(defun lse-replicate-fill-in-by-older ()
  "Replace current replication by next older one."
  (interactive "*")
  (lse_replicate_fill_in_by_older)
; lse-replicate-fill-in-by-older
)

;;;++
;;; commands for help about fill-ins
;;;--
(defun lse-describe-fill-in ()
  (interactive)
  (let* ((name (lse_inside_fill-in))
         psym 
        )
    (if name
        (or (lse_show_fill-in_description
               (lse-fill-in:symbol lse_current_fill-in)
            )
            (message "No description available for %s" name)
        )
      (message "Not inside fill-in")
    )
  )
; lse-describe-fill-in
)

(defun lse-help-fill-in ()
  (interactive)
  (let* ((name (lse_inside_fill-in))
         psym 
        )
    (if name
        (or (lse_show_fill-in_help (lse-fill-in:symbol lse_current_fill-in))
            (message "No help available for %s" name)
        )
      (message "Not inside fill-in")
    )
  )
; lse-help-fill-in
)

;;;++
;;; user level commands for un/re-expansion
;;;-- 
(defun lse-unexpand-fill-in ()
  (interactive "*")
  (if lse_replaced_fill-in 
      (error "Cannot unexpand while currently replacing a fill-in")
    (lse_unfill_fill-in "un-expand")
  )
)

(defun lse-reexpand-fill-in ()
  (interactive "*")
  (if lse_replaced_fill-in 
      (error "Cannot reexpand while currently replacing a fill-in")
    (let ((last_fill-in (lse_fill-in_history:last_unexpansion))
         )
      (if (not last_fill-in)
          (error "No fill-in to re-expand")
        (lse_fill-in_history:remove_last_unexpansion)
        (lse_fill-in_history:add_expansion
             (lse_toggle_fill-in_expansion last_fill-in)
        )
        (save-excursion                         ; 12-Jun-1994 auto-replication
          (mapcar 'lse_toggle_fill-in_expansion
                  (lse-fill-in:descendents last_fill-in)
          )
        )
      )
    )
  )
)

;;;++
;;; user level commands for un/re-replacement
;;;-- 
(defun lse-unreplace-fill-in ()
  (interactive "*")
  (if lse_replaced_fill-in
      (lse@shut_fill-in_replacement t)
  )
  (lse_unfill_fill-in "un-replace")
  (lse-flush-replacement)
)

(defun lse-rereplace-fill-in ()
  (interactive "*")
  (let ((last_fill-in (lse_fill-in_history:last_unexpansion))
       )
    (if (or (not last_fill-in) 
            (not (eq (lse-fill-in:fill-type last_fill-in) 'lse@replaced))
        )
        (error "No fill-in to re-replace")
      (lse_fill-in_history:remove_last_unexpansion)
      (setq lse_replaced_fill-in (lse_toggle_fill-in_expansion last_fill-in))
    )
  )
)

;;;+
;;; Commands for killing and unkilling fill-ins
;;;-
(defun lse-kill-all-optional-fill-ins ()
  "Kill all optional fill-in from point to end of buffer."
  (interactive "*")
  (if (lse_inside_fill-in)
      (lse_kill_current_fill-in_if_optional)
  )
  (while (lse-goto-next-fill-in t)
    (lse_kill_current_fill-in_if_optional)
  )
; lse-kill-all-optional-fill-ins
)

(defvar lse-kill:tail-delim-pattern
        (concat "[" lse_fill-in_tail_delim_chars "]")
)

(defun lse-kill-fill-in (&optional dont-move)
  "Kill the next fill-in"
  (interactive "*P")
  (if (or (and (lse_inside_fill-in)
               (not (looking-at lse-kill:tail-delim-pattern))
          )
          (lse-goto-next-fill-in)
      )
      (lse_kill_current_fill-in)
  )
  (if dont-move
      t
    (lse-goto-next-fill-in t)
  )
; lse-kill-fill-in
) 

(defun lse-unkill-fill-in ()
  "Unkill the fill-in which was killed last."
  (interactive "*")
  (lse_unkill_fill_in)
; lse-unkill-fill-in
)

;;;+
;;; Interface to emacs and non-lse commands
;;;-
(defun lse-command:do (&optional cmd)
  (interactive)
  (let (result
       )
    (let ((lse_completion_buffer lse_command:completion_buffer)
         )
      (if lse_command:initialized
          t
        (save-excursion
          (set-buffer (lse_completion:buffer "LSE command"))
          (lse_completion:show "" lse_command:cmd_list nil)
          (setq lse_command:completion_buffer (current-buffer))
        )
        (setq lse_command:initialized t)
      )
      (setq result
        (lse-complete cmd lse_command:cmd_list nil t nil lse_command:last t)
      )
      (and result (setq lse_command:last result))
    )
    (if result
        (let ((binding (cdr (assoc result lse_command:cmd_list)))
             )
          (cond ((commandp binding) (call-interactively binding))
                ((symbolp binding)  (funcall binding))
                ((consp binding)    (eval binding))
                (t (error "Command %s has invalid binding: %s" result binding))
          )
        )
    )
  )
)

;;;+
;;; Commands for moving to fill-ins
;;;-
(defun lse-goto-next-fill-in (&optional quiet name)
  (interactive "P")
  (if (eobp)
      nil
    (lse-goto-@-fill-in 'lse_search_fill-in:forward 'eobp quiet name)
  )
) 

(defun lse-goto-prev-fill-in (&optional quiet name)
  (interactive "P")
  (if (bobp)
      nil
    (let ((cp (point))
          result
         )
      (if (lse_inside_fill-in)
          (progn
            (goto-char 
                 (lse-range:head-pos (lse-fill-in:range lse_current_fill-in))
            )
            (if (not (bobp)) (forward-char -1))
          )
      )
      (setq result
            (lse-goto-@-fill-in 'lse_search_fill-in:backward 'bobp quiet name)
      )
      (if (or (bobp) (not result))
          (goto-char cp)
      )
      result
    )
  )
) 

(defun lse-goto-last-position ()
  (interactive)
  (if lse_last_position (goto-char lse_last_position))
) 

;;;++
;;; lse-split-line: handle comments properly
;;;--
(defun lse-split-line ()
  (interactive "*")
  (if (bolp)
      (lse-fill-in-insert "\n"); (newline) not used to avoid marker confusion
    (if (integerp lse@current-expansion-indent)
        ;; expanding a fill-in
        (lse-newline-and-indent)
      ;; not expanding a fill-in: determine comment leading and trailer for
      ;; lse-newline-and-indent
      (let (lse@expansion-line-leading 
            lse@expansion-line-leading-indent
            lse@expansion-line-trailer
            lse@expansion-line-trailer-indent
           )
        (lse_comment:setup_expansion_leading)
        (lse_comment:setup_expansion_trailer)
        (lse-newline-and-indent)
      )
    )
  )
; lse-split-line
)

(defun lse-set-tab-increment (inc)
  "Sets tab increment to `inc'"
  (interactive "NTab increment: \n")
  (if (integerp inc)
      (progn
        (setq lse-language:tab-increment inc)
        (message (format "Tab increment of current buffer set to %d" inc))
      )
  )
; lse-set-tab-increment
)

(defun lse-expand-or-tabulator ()
  (interactive "*")
  (if (lse_inside_fill-in)
      (lse-expand)
    (lse-tabulator)
  )
; lse-expand-or-tabulator
)

(defun lse-expand-or-goto-next ()
  (interactive "*")
  (if (lse_inside_fill-in)
      (lse-expand)
    (lse-goto-next-fill-in)
  )
; lse-expand-or-goto-next
)

;;;+
;;; Compilation and use of lse languages
;;;-
(defun lse-language:read-name (&optional name)
  (or name 
      (setq name (lse-complete "" lse-language:table nil nil nil nil t))
      (if lse-emacs19-p
          (setq name (completing-read
                          "Language: " lse-language:table nil nil nil
                          'minibuffer-history 
                     )
          )
        (setq name (completing-read-with-history-in 
                        'minibuffer-history "Language: " lse-language:table
                   )
        )
      )
  )
  name
; lse-language:read-name
)

(defun lse-language:compile (&optional name)
  "Compile a LSE-language from source files."
  (interactive)
  (setq name (lse-language:read-name name))
  (let ((lse-language:@compiling t)
        (file-name (concat "lse-language-" name lse-language:compil_ext))
       )
    (save-window-excursion
      (set-buffer (get-buffer-create " $LSE-definition-log$"))
      (erase-buffer)
      (set-buffer (get-buffer-create " $LSE-fill-in:refs$"))
      (erase-buffer)
      (set-buffer (get-buffer-create " $LSE-fill-in:defs$"))
      (erase-buffer)

      (set-buffer (get-buffer-create file-name))
      (erase-buffer)
      (setq buffer-file-name   (concat lse-directory "/" file-name))
      (lse-language:use-loaded (lse-language:load-from-source name))
      (mapatoms 'lse-compile@write-one-fill-in lse_fill-in_table)
      (mapatoms 'lse-compile@write-one-token   lse_token_table)
      (save-buffer nil)
      (mapcar (function (lambda (x)
                          (let ((b (get-buffer " $LSE-fill-in:defs$")))
                            (princ x b) (terpri  b)
                          )
                        )
              )
              lse-language:fill-in-defs
      )
      (mapcar (function (lambda (x)
                          (let ((b (get-buffer " $LSE-fill-in:refs$")))
                            (princ x b) (terpri  b)
                          )
                        )
              )
              lse-language:fill-in-refs
      )
    )
  )
  (lse-ring-bell)
; lse-language:compile
)

(defun lse-language:use (&optional name)
  "Use a specific LSE language for the current buffer."
  (interactive)
  (setq name (lse-language:read-name name))
  (let ((lsym (intern-soft (downcase name) lse-language:table))
       )
    (if (or (not lsym) (not (get lsym 'loaded)))
        (or (setq lsym (lse-language:load-compiled    name))
            (setq lsym (lse-language:load-from-source name))
            (error "Can't find language %s"           name)
        )
    )
    (lse-language:use-loaded lsym t)
  )
; lse-language:use
)

(defun lse-language:reload (&optional name)
  "Reload a specific LSE language from sources."
  (interactive)
  (setq name (lse-language:read-name name))
  (let ((lsym (intern-soft (downcase name) lse-language:table))
        (lse-language:@compiling t)
       )
    (or (setq lsym (lse-language:load-compiled    name))
        (setq lsym (lse-language:load-from-source name))
        (error "Can't find language %s"           name)
    )
  )
; lse-language:reload
)

;;; 13-Sep-1994 
(defun lse-language:check (&optional name)
  "Check a LSE-language for references to undefined fill-ins."
  (interactive)
  (setq name (lse-language:read-name name))
  (lse-language:compile name)
  (save-excursion 
    (set-buffer (get-buffer " $LSE-fill-in:refs$"))
    (write-region (point-min) (point-max) (concat "/tmp/" name ".refs") nil 1)
    (set-buffer (get-buffer " $LSE-fill-in:defs$"))
    (write-region (point-min) (point-max) (concat "/tmp/" name ".defs") nil 1)
  )
  (with-output-to-temp-buffer " $LSE-fill-in:undefined"
    (call-process
       (concat lse-script-dir "/check_lse_language")
       nil
       " $LSE-fill-in:undefined"
       t
       (concat "/tmp/" name ".defs")
       (concat "/tmp/" name ".refs")
       (concat lse-directory "/" "lse-language-" name lse-language:compil_ext)
    )
  )
; lse-language:check
) 
