(provide 'lse-comment)

(defvar                      lse_comment_head_delim ";")
(defvar                      lse_comment_tail_delim nil)
(defvar                      lse_comment_head_delim_pattern ";+ *")
(defvar                      lse_comment_tail_delim_pattern nil)
(defvar                      lse_comment_delim_char_set ";"); for use in pattern character sets
(make-variable-buffer-local 'lse_comment_head_delim)
(make-variable-buffer-local 'lse_comment_tail_delim)
(make-variable-buffer-local 'lse_comment_head_delim_pattern)
(make-variable-buffer-local 'lse_comment_tail_delim_pattern)
(make-variable-buffer-local 'lse_comment_delim_char_set)

(defun lse_comment:leading_comment_head_position ()
  (let ((search-limit (point)) ; (lse-tpu:line-head-pos) for re-search-backward
       )
    (if (and lse_comment_head_delim_pattern
             ;; 18-May-1994 search forward instead of backward to catch all
             ;; comment characters
             (or (beginning-of-line) t)
             (re-search-forward  lse_comment_head_delim_pattern 
                                 search-limit
                                 t
             )
        )
        (point)
      nil
    )
  )
)

(defun lse_comment:setup_expansion_leading ()
  (if lse_comment_head_delim_pattern 
      (save-match-data
        (save-excursion
          (if (lse_comment:leading_comment_head_position)
              (progn
                (setq lse@expansion-line-leading 
                      (buffer-substring (match-beginning 0) (match-end 0))
                )
                (goto-char (match-beginning 0))
                (setq lse@expansion-line-leading-indent (current-column))
              )
          )
        )
      )
  )
)

(defun lse_comment:trailer_comment_tail_position ()
  (if (and lse_comment_tail_delim_pattern
           (re-search-forward lse_comment_tail_delim_pattern 
                              (lse-tpu:line-tail-pos)
                              t
           )
      )
      (point)
    nil
  )
)

(defun lse_comment:setup_expansion_trailer ()
  (if lse_comment_tail_delim_pattern
      (save-match-data
        (save-excursion
          (if (lse_comment:trailer_comment_tail_position)
              (progn
                (setq lse@expansion-line-trailer 
                      (buffer-substring (match-beginning 0) (match-end 0))
                )
                (setq lse@expansion-line-trailer-indent 
                      (- (point) (match-beginning 0))
                )
              )
          )
        )
      )
  )
)
