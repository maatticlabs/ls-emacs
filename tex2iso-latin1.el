;-*-unibyte: t;-*-
;;;; the line above is needed for Emacs 20.3 -- without it,character ranges
;;;; for characters between \200 and \377 don't work
 
(defun tex-2-iso-latin1 ()
  (interactive)
  (let ((case-replace     t)
        (case-fold-search nil)
       )
    (lse-tpu:replace-all "\\\\\"A"       "�")
    (lse-tpu:replace-all "\"A"           "�")
    (lse-tpu:replace-all "\\\\\"a"       "�")
    (lse-tpu:replace-all "\"a"           "�")
    (lse-tpu:replace-all "\\\\\"O"       "�")
    (lse-tpu:replace-all "\"O"           "�")
    (lse-tpu:replace-all "\\\\\"o"       "�")
    (lse-tpu:replace-all "\"o"           "�")
    (lse-tpu:replace-all "\\\\\"U"       "�")
    (lse-tpu:replace-all "\"U"           "�")
    (lse-tpu:replace-all "\\\\\"u"       "�")
    (lse-tpu:replace-all "\"u"           "�")
    (lse-tpu:replace-all "\\\\ss"        "�")
    (lse-tpu:replace-all "\"s"           "�")
    (lse-tpu:replace-all "\\\\AA"        "�")
    (lse-tpu:replace-all "\\\\AE"        "�")
    (lse-tpu:replace-all "\\\\c C"       "�")
    (lse-tpu:replace-all "\\\\'E"        "�")
    (lse-tpu:replace-all "\\\\~N"        "�")
    (lse-tpu:replace-all "\\\\`a"        "�")
    (lse-tpu:replace-all "\\\\'a"        "�")
    (lse-tpu:replace-all "\\\\^a"        "�")
    (lse-tpu:replace-all "\\\\aa"        "�")
    (lse-tpu:replace-all "\\\\ae"        "�")
    (lse-tpu:replace-all "\\\\c c"       "�")
    (lse-tpu:replace-all "\\\\`e"        "�")
    (lse-tpu:replace-all "\\\\'e"        "�")
    (lse-tpu:replace-all "\\\\^e"        "�")
    (lse-tpu:replace-all "\\\\\"e"       "�")
    (lse-tpu:replace-all "\"e"           "�")
    (lse-tpu:replace-all "\\\\`\\\\i"    "�")
    (lse-tpu:replace-all "\\\\'\\\\i"    "�")
    (lse-tpu:replace-all "\\\\^\\\\i"    "�")
    (lse-tpu:replace-all "\\\\\"\\\\i"   "�")
    (lse-tpu:replace-all "\"\\\\i"       "�")
    (lse-tpu:replace-all "\\\\~n"        "�")
    (lse-tpu:replace-all "\\\\`o"        "�")
    (lse-tpu:replace-all "\\\\'o"        "�")
    (lse-tpu:replace-all "\\\\^o"        "�")
    (lse-tpu:replace-all "\\\\`u"        "�")
    (lse-tpu:replace-all "\\\\'u"        "�")
    (lse-tpu:replace-all "\\\\^u"        "�")
    (lse-tpu:replace-all "!`"            "�")
    (lse-tpu:replace-all "?`"            "�")
    (lse-tpu:replace-all "\\\\pounds"    "�")
    (lse-tpu:replace-all "\\\\S "        "�")
    (lse-tpu:replace-all "\\\\P "        "�")
  )
)
;;; (setq debug-on-error nil)
