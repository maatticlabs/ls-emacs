;;;; (c) 1994 Swing Informationssysteme GmbH. All rights reserved.
;;;;++
;;;; Name
;;;;    lse-flat-fill-in
;;;;
;;;; Purpose
;;;;    Functions for dealing with flat (i.e., unexpanded) fill-ins
;;;;
;;;; Revision Dates
;;;;    24-May-1994 (CT) Creation (of comment)
;;;;    24-May-1994 (CT) Clarification of structure (major surgery!)
;;;;    26-May-1994 (CT) Simple tokens: handling of lists (consp)
;;;;    26-May-1994 (CT) Interactive functions moved to lse-interactive
;;;;    12-Jun-1994 (CT) Correction of errors
;;;;    12-Jun-1994 (CT) Auto-replication added
;;;;    26-Jun-1994 (CT) replacement-vanguard added
;;;;    27-Jun-1994 (CT) Variable lse-flat-fill-in:open-replacement added
;;;;     8-Sep-1994 (CT) case-fold parameter passed to lse-complete
;;;;    22-Sep-1994 (CT) lse-insert-replica-or-flat added
;;;;    24-Sep-1994 (CT) Error corrected
;;;;    24-Sep-1994 (CT) lse-insert-replica-or-flat-opt added
;;;;    26-Sep-1994 (CT) Error in token expansion corrected (did not work
;;;;                     for menu and function fill-in's)
;;;;    30-Sep-1994 (CT) Force manual completion of menu's
;;;;                     (otherwise a menu containing an empty entry is never
;;;;                     seen by the user)
;;;;    30-Sep-1994 (CT) Removed optional parameter `quiet' from
;;;;                     lse_expand_token:try 
;;;;    30-Sep-1994 (CT) Recover collapsed range of fill-in
;;;;                     Such a collapse can happen by actions performed by
;;;;                     completion-leading or one of its friends (e.g., if
;;;;                     `just-one-space' is executed and the replacement
;;;;                     consisted of blanks only)
;;;;    22-Jan-1995 (CT) lse_replicate_fill_in and
;;;;                     lse_replicate_fill_in_by_older moved in here (were
;;;;                     contained inline in lse-replicate-fill-in and
;;;;                     lse-replicate-fill-in-by-older in lse-interactive.el)
;;;;    22-Jan-1995 (CT) lse_replicate-fill-in and
;;;;                     lse-replicate-fill-in-by-older stay in replacement
;;;;                     mode 
;;;;-- 
(provide 'lse-flat-fill-in)

(defmacro with_lse_fill_environment_at (pos &rest forms)
  (` (let (lse@current-expansion-indent 
           lse@original-expansion-indent
           lse@environment-expansion-indent
           lse@expansion-line-leading
           lse@expansion-line-trailer
           lse@expansion-line-leading-indent
           lse@expansion-line-trailer-indent
          )
       (save-excursion
         (and (, pos) (goto-char (, pos)))
         (setq lse@current-expansion-indent     (current-column))
         (setq lse@original-expansion-indent    (current-column))
         (setq lse@environment-expansion-indent (current-indentation))
         (lse_comment:setup_expansion_leading)
         (lse_comment:setup_expansion_trailer)
       )
       (,@ forms)
     )
  )
; with_lse_fill_environment_at
)

(defun lse_show_fill-in_description (psym)
  (let ((desc (get psym 'description)))
    (if (stringp desc) (message desc))
  )
; lse_show_fill-in_description
)

(defun lse_show_fill-in_help (psym)
  (let ((help (get psym 'help))
        (desc (get psym 'description))
       )
    (if (stringp help)
        (with-output-to-temp-buffer " $lse help$"
            (princ "Fill-in : ")
            (princ (symbol-name psym))
            (princ "\n")
            (princ "Type    : ")
            (princ (get psym 'type))
            (princ "\n\n\n")
            (princ desc)
            (princ "\n\n")
            (princ help)
            (princ "\n")
        )
      (if (stringp desc) (message desc))
    )
  )
; lse_show_fill-in_help
)

(defun lse-fill-in:option_fill-in (psym option)
  (let ((result (get psym option))
       )
    (if result (lse_fill-in:definition (symbol-name result)))
  )
; lse-fill-in:option_fill-in
)

(defun lse-fill-in:rcompletion-action (psym)
  (lse-fill-in:option_fill-in psym 'rcompletion-action)
)

(defun lse-fill-in:rcompletion-leading (psym)
  (lse-fill-in:option_fill-in psym 'rcompletion-leading)
)

(defun lse-fill-in:rcompletion-trailer (psym)
  (lse-fill-in:option_fill-in psym 'rcompletion-trailer)
)

(defun lse-fill-in:replacement-leading (psym)
  (lse-fill-in:option_fill-in psym 'replacement-leading)
)

(defun lse-fill-in:replacement-trailer (psym)
  (lse-fill-in:option_fill-in psym 'replacement-trailer)
)

;;; 26-Jun-1994 
(defun lse-fill-in:replacement-vanguard (psym)
  (lse-fill-in:option_fill-in psym 'replacement-vanguard)
)

(defun lse-flat-fill-in:clean-current (psym flat-range)
  ;; PRECONDITION (equal (point) (lse-range:head-pos flat-range))
  (let (dupl-range)
    (if (not (lse_current_fill-in_repeats))
        (lse-range:clean flat-range)
      (setq dupl-range
            (lse_convert_current_fill-in_to_duplicate psym flat-range)
      )
    )
    (lse-range:change-tail-pos flat-range (lse-range:head-pos flat-range))
    dupl-range
  )
; lse-flat-fill-in:clean-current
)

(defvar                      lse-flat-fill-in:open-replacement nil)
(make-variable-buffer-local 'lse-flat-fill-in:open-replacement)

(defun lse-flat-fill-in:open-replacement (psym name fill-type)
  (setq lse-flat-fill-in:open-replacement fill-type)
  (let ((rvanguard    (lse-fill-in:replacement-vanguard psym)); 26-Jun-1994 
       )
    (if (and rvanguard (not lse-flat-fill-in:no-vanguard))
        (progn
          (goto-char
               (lse-range:head-pos (lse-fill-in:range lse_current_fill-in))
          )
          (lse-flat-fill-in:interpret-replacement_in_env rvanguard)
          (lse-goto-next-fill-in) ; updates lse_current_fill-in's head-pos
        )
    )
  )
  (let* ((flat-range  (lse-fill-in:range                lse_current_fill-in))
         (inner-range (lse-fill-in:inner-range          lse_current_fill-in))
         (head-pos    (lse-range:head-pos               flat-range))
         (complement  (lse-range:contents               flat-range))
         (rleading    (lse-fill-in:replacement-leading  psym))
         (rtrailer    (lse-fill-in:replacement-trailer  psym))
         p
         dupl-range 
        )
    (goto-char head-pos)
    (setq dupl-range (lse-flat-fill-in:clean-current psym flat-range))
    (lse-flat-fill-in:interpret-replacement_in_env rleading)
    (setq p (point-marker))
    (save-excursion 
        (lse-flat-fill-in:interpret-replacement_in_env rtrailer head-pos)
    )
    (if inner-range
        (progn
          (lse-range:change-head-pos inner-range p)
          (lse-range:change-tail-pos inner-range p)
        )
      (setq inner-range (lse-range:new p p))
      (lse-fill-in:change-inner-range lse_current_fill-in inner-range)
    )
    (lse-fill-in:change-state      lse_current_fill-in 'lse@deep)
    (lse-fill-in:change-fill-type  lse_current_fill-in fill-type)
    (lse-fill-in:change-complement lse_current_fill-in complement)
    (lse-fill-in:change-duplicate  lse_current_fill-in dupl-range)
    lse_current_fill-in
  )
; lse-flat-fill-in:open-replacement
)

(defun lse-flat-fill-in:close-replacement
           (psym &optional dont-save-for-unexpand)
  (let* ((range      (lse-fill-in:range      lse_current_fill-in))    
         (head-pos   (lse-range:head-pos     range))
         (rcaction   (lse-fill-in:rcompletion-action  psym))
         (rcleading  (lse-fill-in:rcompletion-leading psym))
         (rctrailer  (lse-fill-in:rcompletion-trailer psym))
         lse_replaced_fill-in ; 30-Sep-1994 avoid infinite recursion in case
                              ; that rcompletion-* calls lse-expand-token
        )
    (save-excursion 
      (and rcleading
           (goto-char head-pos)
           (lse-flat-fill-in:interpret-replacement_in_env rcleading)
      )
      (and rctrailer
           (goto-char (lse-range:tail-pos range))
           (lse-flat-fill-in:interpret-replacement_in_env rctrailer head-pos)
      )
    )
    (and rcaction (lse-flat-fill-in:interpret-replacement_in_env rcaction))
    (if (lse-range:is-collapsed range); 30-Sep-1994 avoid errors
        (lse-range:change-tail-pos range (lse-range:head range))
    )
    (if (lse-fill-in:duplicate lse_current_fill-in)
        (lse-range:change-head (lse-fill-in:duplicate lse_current_fill-in)
                               (lse-range:tail range)
        )
    )
    (lse-flat-fill-in:auto-replicate psym (lse-range:contents range))
    (or dont-save-for-unexpand (get psym 'no-history)
        (lse_fill-in_history:add_expansion lse_current_fill-in)
    )
    (setq lse-flat-fill-in:open-replacement nil)
    range
  )
; lse-flat-fill-in:close-replacement
)

(defun lse-flat-fill-in:expand-replacement
           (psym name &optional dont-save-for-unexpand expansion)
  (let ((lse-flat-fill-in:expansion-in-progress t)
        range
       )
    (lse-flat-fill-in:open-replacement psym name 'lse@expanded)
    (if expansion
        (lse-fill-in-insert expansion)
      (lse-flat-fill-in:interpret-replacement_in_env psym)
    )
    (setq range
          (lse-flat-fill-in:close-replacement psym dont-save-for-unexpand)
    ) 
    (lse_fill-in_history:purge_unexpansion psym range)
    (setq lse_current_fill-in nil)
    (or (lse_inside_fill-in)
        (lse_goto_first_fill-in_of_range 
            (lse-range:head-pos range)
            (lse-tpu:line-tail-pos (or (get psym 'max-line-move) 1))
        )
    )
    range
  )
; lse-flat-fill-in:expand-replacement
)

(defun lse-flat-fill-in:expand-inner-replacement
           (psym name &optional expansion)
  (let ((rleading  (lse-fill-in:replacement-leading psym))
        (rtrailer  (lse-fill-in:replacement-trailer psym))
        (rcleading (lse-fill-in:rcompletion-leading psym))
        (rctrailer (lse-fill-in:rcompletion-trailer psym))
        (head-pos  (point))
       )
    (lse-flat-fill-in:interpret-replacement_in_env rleading)
    (save-excursion 
        (lse-flat-fill-in:interpret-replacement_in_env rtrailer head-pos)
    )
    (if expansion
        (lse-fill-in-insert expansion)
      (lse-flat-fill-in:interpret-replacement_in_env psym)
    )
    (save-excursion 
      (goto-char head-pos)
      (lse-flat-fill-in:interpret-replacement_in_env rcleading)
    )
    (lse-flat-fill-in:interpret-replacement_in_env rctrailer head-pos)
    t
  )
; lse-flat-fill-in:expand-inner-replacement
)

(defun lse-flat-fill-in:expand-menu 
           (psym name &optional dont-save-for-unexpand)
  (let ((expansion (lse_expand_menu psym name (get psym 'menu))))
    (cond ((not expansion))
          ((symbolp expansion)
           (or lse@expansion@separator
               (setq lse@expansion@separator (get psym 'separator))
           )
           (lse-flat-fill-in:expand
                expansion (symbol-name expansion) dont-save-for-unexpand
           )
           (setq lse@expansion@separator nil); 24-Sep-1994 reset!
          )
          ((stringp expansion)
           (lse-flat-fill-in:expand-replacement
                psym name dont-save-for-unexpand expansion 
           )
          )
          (t (error "Invalid menu %s" name))
    )
  )
; lse-flat-fill-in:expand-menu
)

(defun lse-flat-fill-in:expand (psym name &optional dont-save-for-unexpand)
  (let ((fill-in-type (get psym 'type)))
    (cond ((eq fill-in-type 'terminal)
           (lse_show_fill-in_description psym)
          )
          ((eq fill-in-type 'replacement)
           (lse-flat-fill-in:expand-replacement
               psym name dont-save-for-unexpand
           )
          )
          ((eq fill-in-type 'menu)
           (lse-flat-fill-in:expand-menu
                psym name dont-save-for-unexpand
           )
          )
          ((eq fill-in-type 'function)
           (lse-flat-fill-in:interpret-replacement psym 'function)
          )
          ((not fill-in-type)
           (error "Undefined fill-in: `%s'" name)
          )
          (t (error "Unknown fill-in type: `%s'" name))
    )
  )
; lse-flat-fill-in:expand
)

;;;;++ 
;;;; Second internal level for expansion 
;;;;--
(defvar lse@expansion@separator      nil)
(defvar lse-flat-fill-in:no-vanguard nil); 26-Jun-1994 

;;;+ 
;;; lse-flat-fill-in:interpret-replacement performs the actual expansion of fill-in
;;; `psym':  
;;;    - here the structure of the fill-in is interpreted 
;;;    - no explicit change in position before expansion
;;;    - no change of environment
;;; after expansion the cursor position is just at the end of the deep text
;;;-
(defun lse-flat-fill-in:interpret-replacement (psym &optional kind)
  (if psym
      (let ((body  (get psym (or kind 'replacement)))
            next
           )
        (if body
            (progn
              (if (eobp)
                  (save-excursion            ; otherwise strange things happen!
                    (lse-fill-in-insert " ")
                    (lse-newline) 
                  )
              )
              (while body
                (setq next (car body))
                (setq body (cdr body))
                (cond ((stringp next) (lse-fill-in-insert next))
                      ((symbolp next) 
                       (if (symbol-function next)
                           (funcall next)
                         (if (stringp (symbol-value next))
                             (lse-fill-in-insert (symbol-value next))
                           (error "Invalid value for symbol %s: %s" 
                                  (symbol-name next) (symbol-value next)
                           )
                         )
                       )
                      )
                      ((consp next) (eval next))
                      (t (error "Don't understand `%s'" next))
                )
              )
            ); progn
        ); if body
      ); let
  ); if psym
  psym; 12-Jun-1994 
; lse-flat-fill-in:interpret-replacement
)

(defun lse-flat-fill-in:interpret-replacement_in_env (psym &optional env-pos)
  (if psym 
      (with_lse_fill_environment_at env-pos
        (lse-flat-fill-in:interpret-replacement psym)
      )
  )
  psym; 12-Jun-1994 
; lse-flat-fill-in:interpret-replacement_in_env
)

(defun lse-auto-expand-menu-fill-in (psym name)
  (let ((expansion (lse_expand_menu psym name (get psym 'menu)))
       )
    (cond ((symbolp expansion)
           (lse-auto-expand-fill-in (symbol-name expansion))
          )
          ((stringp expansion)
           (lse-flat-fill-in:expand-inner-replacement
                psym name expansion
           )
          )
          (t (error "Invalid menu %s" name))
    )
  )
)

;;;;++ 
;;;; First expansion level 
;;;;--

;;; expand the fill-in associated to a token 
(defun lse-expand-fill-in-token
           (psym token token-head token-tail &optional inner-replacement)
  (if inner-replacement
      (progn
        (delete-region token-head token-tail)
        (lse-flat-fill-in:interpret-replacement_in_env psym)
        (let (lse_replaced_fill-in) ; otherwise fatal  recursion ; 12-Jun-1994 
          (lse_goto_first_fill-in_of_range                       ; 12-Jun-1994 
              token-head
              (lse-tpu:line-tail-pos (or (get psym 'max-line-move) 1))
          )                                                      ; 12-Jun-1994 
        )                                                        ; 12-Jun-1994 
        psym                                                     ; 12-Jun-1994 
      )
    (setq lse_current_fill-in
          (lse-fill-in:new
               psym 
               (symbol-name psym)
               'lse@flat
               'lse@expanded
               (lse-range:new token-head token-tail)
               nil
               token
          )
    )
    ;; 26-Sep-1994 `:expand' instead of `:expand-replacement' (otherwise
    ;; menu- and function-fill-in's fail) 
    (lse-flat-fill-in:expand psym (symbol-name psym))
  )
; lse-expand-fill-in-token
)

;;; expansion of embedded fill-in's: @ "name"
(defun lse-auto-expand-fill-in (name)
  (let* ((psym (lse_fill-in:definition name))
         (fill-in-type (get psym 'type))
        )
    (cond ((eq fill-in-type 'terminal)
           (lse_show_fill-in_description psym)
          )
          ((eq fill-in-type 'replacement)
           (lse-flat-fill-in:expand-inner-replacement psym name)
          )
          ((eq fill-in-type 'menu)
           (lse-auto-expand-menu-fill-in psym name)
          )
          ((eq fill-in-type 'function); 26-Sep-1994 
           (lse-flat-fill-in:interpret-replacement psym 'function)
          )
    )
  )
)

(defun lse_expand_menu:entry_list (the-entries)
  (let (result
        entry
        entry-name
        entry-sym
       )
    (while the-entries
      (setq entry       (car the-entries))
      (setq the-entries (cdr the-entries))
      (cond ((eq entry '@)
             (setq entry       (car the-entries))
             (setq the-entries (cdr the-entries))
             (setq entry-sym
                   (lse_fill-in:definition
                        (if (symbolp entry) (symbol-name entry) entry)
                   )
             )
             (setq result 
                   (append (lse_expand_menu:entry_list
                              (get entry-sym 'menu)
                           )
                           result nil
                   )
             )
            )
            ((symbolp entry)
             (setq entry-name (symbol-name entry))
             (setq entry-sym  (lse_fill-in:definition entry-name))
             (setq result (cons (cons entry-name entry-sym) result))
            )
            ((stringp entry)
             (setq result (cons (cons entry "") result))
            )
            ((consp   entry)
             (setq result (cons entry result))
            )
            (t
             (error "Invalid menu entry %s" entry)
            )
      )
    )
    (reverse result)
  )
; lse_expand_menu:entry_list
)

(defun lse_expand_menu (psym name the-entries)
  (let (choice
        result
        (entries (lse_expand_menu:entry_list the-entries)) 
       )
    (setq choice
          (lse-complete "" entries (not (get psym 'sort)) nil t nil t)
    ); 30-Sep-1994 force completion (otherwise empty menu entries don't work)
    (if choice
        (if (not (setq result (cdr (assoc choice entries))))
            (error "undefined menu entry : %s" choice)
          (cond ((symbolp result)
                 (setq result (lse_fill-in:definition (symbol-name result)))
                )
                (t
                 (setq result choice)
                )
          )
        )
    )
    result
  )
; lse_expand_menu
)

;;;;++
;;;; First internal level for token expansion
;;;;-- 
(defun lse_replace_token_by_expansion 
            (token-given token-head token-tail expansion)
  (let (result)
    (if expansion
        (let* ((token-sym   (intern-soft     expansion lse_token_table))
               (token-val   (symbol-value    token-sym))
               (token-fct   (symbol-function token-sym))
               (token-range (lse-range:new   token-head token-tail))
               (inner-replacement lse_replaced_fill-in)
              )
          (if token-fct
              (progn
                (setq result 
                      (funcall token-fct 
                         (lse_fill-in:definition token-val) expansion
                         token-head token-tail
                         inner-replacement
                      )
                )
                (if inner-replacement
                    (lse_shut_fill-in_replacement)
                )
              )
            (if inner-replacement (lse_shut_fill-in_replacement token-given))
            (delete-region token-head token-tail)
            (setq result t)
            (if (stringp token-val)
                (lse-fill-in-insert token-val)
              (if (consp token-val)
                  (eval token-val)
                (lse-message "Token `%s' has invalid definition: `%s'"
                             token-given token-val
                )
                (setq result nil)
              )
            )
          )
          (if result                           ; 12-Jun-1994 allow unexpansion
              (lse_fill-in_history:add_expansion ; of token-given
                   (lse-fill-in:new            
                        token-sym (symbol-name token-sym)
                        'lse@flat 'lse@expanded
                        token-range nil token-given
                   )
              )
          )
        )
      (setq result "")
    )
    result
  )
; lse_replace_token_by_expansion
)

(defun lse_expand_token:make_entries (e)
  (let ((tsym (intern (downcase e) lse_token_table)))
    (if (symbol-function tsym)
        (cons e (lse_fill-in:definition (symbol-value tsym)))
      (cons e (format "%s" (symbol-value tsym)))
    )
  )
)

(defun lse_expand_token:try (token-delims word-count)
  (let* ((lse-tpu:word-chars token-delims)
         (bt (lse-tpu:prev-word-head-pos word-count (lse-tpu:line-head-pos)))
         (et (point))
         matching-tokens token expansion result lse@expansion@separator
        )
    (if (or (bobp) (not (> et bt)))
        (setq result nil)
      (setq token (downcase (buffer-substring bt et)))
      (setq matching-tokens (all-completions token lse_token_table))
      (cond ((eq (length matching-tokens) 1)
             (setq expansion (car matching-tokens))
            )
            ((> (length matching-tokens) 1); partial match
             (setq expansion 
                   (lse-complete token 
                        (mapcar 'lse_expand_token:make_entries matching-tokens)
                        nil nil nil nil t
                   )
             )
            )
      )
      (if expansion
          (setq result 
                (lse_replace_token_by_expansion token bt et expansion)
          )
      )
    )
    result
  )
; lse_expand_token:try
)

;;; 
;;; lse_replaced_fill-in: fill-in-info describing pending replacement
;;; 
(defvar                      lse_replaced_fill-in    nil)
(make-variable-buffer-local 'lse_replaced_fill-in)
;;; 
;;; lse-fill-in:currently-replaced: string used for mode line while
;;; replacement is pending 
;;; 
(defvar                      lse-fill-in:currently-replaced)
(make-variable-buffer-local 'lse-fill-in:currently-replaced)

;;; 
;;; install fill-in-replacement as a minor mode
;;; 
(or (assq 'lse_replaced_fill-in minor-mode-alist)
    (lse-add-to-list minor-mode-alist 
                       '(lse_replaced_fill-in lse-fill-in:currently-replaced)
    )
)

;;;;++
;;;; Internals for replacement
;;;;-- 
(defun lse@shut_fill-in_replacement (&optional quiet by-replacement)
  (let ((lse_current_fill-in                   lse_replaced_fill-in)
        (range       (lse-fill-in:range        lse_replaced_fill-in))
        (inner-range (lse-fill-in:inner-range  lse_replaced_fill-in))
        (psym        (lse-fill-in:symbol       lse_replaced_fill-in))
        replaced-by-msg
       )
    (if (and (not by-replacement)
             (or (lse-range:is-collapsed inner-range)
                 (lse-range:is-empty     inner-range)
             )
        )
        (if (or (equal (point-marker) (lse-range:head-pos range))
                (lse-range:inside range)
            )
            (progn
              (lse-range:clean range)
              (lse-fill-in-insert(lse-fill-in:complement lse_replaced_fill-in))
              (setq lse_replaced_fill-in nil)
              (lse-goto-prev-fill-in)
            )
        )
      ;; range of replacement is neither empty nor collapsed
      (lse-flat-fill-in:close-replacement psym)
      (if by-replacement
          (setq replaced-by-msg by-replacement)
        (if (< (lse-range:length range) 46)
            (setq replaced-by-msg
                  (buffer-substring (lse-range:head-pos range) 
                                    (lse-range:tail-pos range)
                  )
            )
          (setq replaced-by-msg
                (concat (buffer-substring 
                                (lse-range:head-pos range)
                             (+ (lse-range:head-pos range) 20)
                        )
                        "..."
                        (buffer-substring 
                             (- (lse-range:tail-pos range) 20)
                                (lse-range:tail-pos range)
                        )
                )
          )
        )
      )
      (if (not quiet)
          (message "Fill-In `%s' replaced by `%s'" 
                   (lse-fill-in:symbol lse_replaced_fill-in) replaced-by-msg
          )
      )
      (setq lse_replaced_fill-in nil)
    )
    (lse-tpu:update-mode-line)
  )
)

(defun lse_shut_fill-in_replacement (&optional by-replacement)
  (if lse_replaced_fill-in
      (lse@shut_fill-in_replacement nil by-replacement)
  )
)

(defun lse_shut_fill-in_replacement/if_outside ()
  (if (and lse_replaced_fill-in 
           (not (lse-range:inside (lse-fill-in:range lse_replaced_fill-in)))
      )
      (lse@shut_fill-in_replacement)
  )
)

;;;;++ 
;;;; internals for replacement
;;;;-- 
(defun lse_open_fill-in_replacement (name)
  ;; name is the fill-in's name to be replaced
  ;; `name' must be the lse_current_fill-in 
  (let ((psym (lse-fill-in:symbol lse_current_fill-in))
       )
    (setq lse_replaced_fill-in
          (lse-flat-fill-in:open-replacement psym name 'lse@replaced)
    )
    (setq lse-fill-in:currently-replaced (concat " <" name ">"))
    (lse-tpu:update-mode-line)
    (setq lse_current_fill-in nil)
  )
)

;;; 
;;; start replacement of current fill-in, if any
;;; 
(defvar lse-flat-fill-in:expansion-in-progress nil)
(defmacro lse_start_replacement_if_in_fill-in ()
  (or lse-flat-fill-in:expansion-in-progress
      (if lse@active@in@buffer
          (if lse_replaced_fill-in
              t
            (if (not (eobp))
                (let ((name (lse_inside_fill-in)))
                   (if name 
                       (if (lse-replacement@allowed)
                           (lse_open_fill-in_replacement name)
                         (error "Cannot replace fill-in `%s'" name)
                       )
                   )
                )
            )
            t
          )
      )
  )
)

(defun lse-replacement@allowed ()
  ;; this functions assumes that current position is inside a flat fill-in 
  (let (result)
    (save-excursion
      (skip-chars-backward (concat "^" lse_fill-in_head_delim_chars))
      (forward-char -1)
      (setq result (not (looking-at lse_no_replacement_fill-in_marker)))
    )
    result
  )
)

;;; 12-Jun-1994 
(defun lse-flat-fill-in:auto-replicate (psym replica)
  (let ((auto-replicate (get psym 'auto-replicate))
        (lse-flat-fill-in:no-vanguard t); 26-Jun-1994 
       )
    (if (integerp auto-replicate)
        (let ((LSE_CURRENT_FILL-IN lse_current_fill-in)
              (name (symbol-name psym))
              lse_replaced_fill-in          ; avoid infinite recursion
              descendents current-fill-in
             )
          (save-excursion 
            (while (and (> auto-replicate 0)
                        (lse-goto-next-fill-in t name)
                   )
              (lse-fill-in:change-complement lse_current_fill-in replica)
              (setq current-fill-in
                    (lse_toggle_fill-in_expansion lse_current_fill-in)
              )
              (lse_fill-in_history:remove_last_expansion)
              (lse-add-to-list descendents current-fill-in)
              (setq auto-replicate (1- auto-replicate))
            )
            (lse-fill-in:change-descendents LSE_CURRENT_FILL-IN descendents)
            ;; restore lse_current_fill-in
            (setq lse_current_fill-in LSE_CURRENT_FILL-IN)
          )
        )
    )
  )
; lse-flat-fill-in:auto-replicate
)

(defun lse_replicate_fill_in ()
  (let ((name (lse_inside_fill-in))
        (lse-flat-fill-in:no-vanguard t) ; 26-Jun-1994 
        replica
       )
    (if (not name)
        (error "Not inside fill-in")
      (setq replica 
            (assq (lse_fill-in:definition name) lse_fill-in_history/expansion)
      )
      (if (not replica)
          (error "No previous expansion found for fill-in `%s'" name)
        (lse_open_fill-in_replacement name)
        (lse-fill-in-insert (lse-range:contents (lse-fill-in:range replica)))
        (lse-fill-in:change-ancestor 
            lse_replaced_fill-in 
            (lse-range:head-pos (lse-fill-in:range replica))
        )
        ;; 22-Jan-1995 stay in replacement-mode;;(lse_shut_fill-in_replacement)
      )
    )
  )
; lse_replicate_fill_in
)

(defun lse_replicate_fill_in_by_older ()
  (let* ((current-fill-in lse_replaced_fill-in)
         (range           (lse-fill-in:range current-fill-in))
         (contents        (lse-range:contents 
                             (lse-fill-in:range current-fill-in)
                          )
         )
         (ancestor        (lse-fill-in:ancestor current-fill-in))
        )
    (if (not (markerp ancestor))
        (error "Last fill-in was not replicated")
      (let ((replica  (lse_fill-in_history:assq 
                           (lse-fill-in:symbol current-fill-in)
                           lse_fill-in_history/expansion
                           ancestor
                       )
            )
           )
        (while (and replica
                    (equal (lse-range:contents (lse-fill-in:range replica))
                           contents
                    )
               )
          (setq replica (lse_fill-in_history:assq 
                           (lse-fill-in:symbol current-fill-in)
                           lse_fill-in_history/expansion
                           (lse-range:head-pos (lse-fill-in:range replica))
                       )
          )
        )
        (if (not replica)
            (error "No previous expansion found for fill-in: %s" 
                   (symbol-name (lse-fill-in:symbol current-fill-in))
            )
          (lse-range:clean    range)
          (goto-char          (lse-range:head-pos range))
          (lse-fill-in-insert (lse-range:contents (lse-fill-in:range replica)))
          (lse-fill-in:change-ancestor 
              current-fill-in (lse-range:head-pos (lse-fill-in:range replica))
          )
        )
      )
    )
  )
; lse_replicate_fill_in_by_older
)

;;; 22-Sep-1994 
(defun lse-insert-replica-or-flat (name &optional head_delim tail_delim)
  (if (not (stringp name))
      (error "Error in definition of currently expanded fill-in")
    (let ((replica (assq (lse_fill-in:definition name)
                         lse_fill-in_history/expansion
                   )
         ))
      (if (not replica)
          (lse-fill-in-insert
             (concat (or head_delim lse_req_fill-in_head_delim)
                     name
                     (or tail_delim lse_req_fill-in_tail_delim)
             )
          )
        (lse-fill-in-insert (lse-range:contents (lse-fill-in:range replica)))
      )
    )
  )
; lse-insert-replica-or-flat
)

;;; 24-Sep-1994
(defun lse-insert-replica-or-flat-opt (name)
  (lse-insert-replica-or-flat name lse_opt_fill-in_head_delim
                                   lse_opt_fill-in_tail_delim
  )
; lse-insert-replica-or-flat-opt
) 
