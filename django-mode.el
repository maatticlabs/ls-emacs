; A (rather minimal) major mode for Emacs to edit Django templates
; by Antonio Cavedoni <http://cavedoni.com/>
;
; Hacked together following instructions from the awesome:
; http://two-wugs.net/emacs/mode-tutorial.html
;
; Acknowledgements:
;
;   - Patrick Quinn <http://www.patrickquinn.net/>
;     for the font-locking patch for HTML syntax
;
; 26-Feb-2008 (CT) Changed to use `define-derived-mode`
; 26-Feb-2008 (CT) `require` for `sgml-mode` added
; 26-Feb-2008 (CT) Use `sgml-font-lock-keywords` instead of
;                  `html-font-lock-keywords`

(require 'sgml-mode)

(defconst django-font-lock-keywords-1
  (list
   '("{% ?\\(\\(end\\)?\\(extends\\|for\\|comment\\|cycle\\|filter\\|firstof\\|debug\\|ifchanged\\|ifequal\\|ifnotequal\\|if\\|include\\|load\\|now\\|regroup\\|spaceless\\|ssi\\|templatetag\\|widthratio\\|block\\)\\) ?.*? ?%}" . 1)
   '("{{ ?\\(.*?\\) ?}}" . (1 font-lock-variable-name-face))
   '("{%\\|\\%}\\|{{\\|}}" . font-lock-builtin-face)
   )
)

(defconst django-font-lock-keywords
  (append django-font-lock-keywords-1 sgml-font-lock-keywords) ;
  "Default highlighting expressions for Django mode"
)

;;; 26-Feb-2008 (CT)
(define-derived-mode django-mode html-mode "Django"
  "Major mode for editing Django templates"
  ;; copied from sgml-mode.el and changed
  (set (make-local-variable 'font-lock-defaults)
       '((django-font-lock-keywords ;; <---- replaced
         )
         nil t nil nil
         (font-lock-syntactic-keywords . sgml-font-lock-syntactic-keywords)
        )
  )
)

(provide 'django-mode)
