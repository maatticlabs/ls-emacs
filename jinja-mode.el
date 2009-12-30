; A (rather minimal) major mode for Emacs to edit Jinja templates
;
; Based on django-mode.el by Antonio Cavedoni <http://cavedoni.com/>
;
;   Hacked together following instructions from the awesome:
;   http://two-wugs.net/emacs/mode-tutorial.html
;
;   Acknowledgements:
;
;   - Patrick Quinn <http://www.patrickquinn.net/>
;     for the font-locking patch for HTML syntax
;
; 26-Feb-2008 (CT) Changed to use `define-derived-mode`
; 26-Feb-2008 (CT) `require` for `sgml-mode` added
; 26-Feb-2008 (CT) Use `sgml-font-lock-keywords` instead of
;                  `html-font-lock-keywords`

(require 'sgml-mode)

(defconst jinja-font-lock-keywords-1
  (list
   '("{%-? ?\\(\\(end\\)?\\(block\\|break\\|continue\\|elif\\|else\\|extends\\|for\\|filter\\|firstof\\|if\\|include\\|import\\|\\)\\) ?.*? ?-?%}" . 1)
   '("{{ ?\\(.*?\\) ?}}" . (1 font-lock-variable-name-face))
   '("{%-?\\|\\%}\\|{{\\|}}" . font-lock-builtin-face)
   )
)

(defconst jinja-font-lock-keywords
  (append jinja-font-lock-keywords-1 sgml-font-lock-keywords) ;
  "Default highlighting expressions for jinja mode"
)

;;; 26-Feb-2008 (CT)
(define-derived-mode jinja-mode html-mode "jinja"
  "Major mode for editing jinja templates"
  ;; copied from sgml-mode.el and changed
  (set (make-local-variable 'font-lock-defaults)
       '((jinja-font-lock-keywords ;; <---- replaced
         )
         nil t nil nil
         (font-lock-syntactic-keywords . sgml-font-lock-syntactic-keywords)
        )
  )
)

(provide 'jinja-mode)
