(defun LSE-REPLACE ()
  (interactive)
  (lse-tpu:replace-all  "/TOKEN/DESCRIPTION=\"\"" "")
  (lse-tpu:replace-all  "/[A-Za-z]*=\"\".*$" "")
  (lse-tpu:replace-all  "/SEPARATOR=\"\".*$" "")
  (lse-tpu:replace-all  "/DUPLICATION=CONTEXT_DEPENDENT.*$" "")
  (lse-tpu:replace-all  "/DUPLICATION=VERTICAL.*$" "")
  (lse-tpu:replace-all  "^ */TOPIC.*$" "")
  (lse-tpu:replace-all  "^ */\\(NO\\)?AUTO_SUBS.*$" "")
  (lse-tpu:replace-all  "^ */LANGUAGE.*$" "")
  (lse-tpu:replace-all  "^DELETE.*" "")
  (lse-tpu:replace-all  "DEFINE TOKEN .*\\([ \t]*\n+\\)+[ \t]*/PlACEHOLDER.*" "")
  (lse-tpu:replace-all  "-[ ]*$" "")
  (lse-tpu:replace-all  "/PLACEHOLDER/NOLIST" "")
  (setq case-replace nil)
  (lse-tpu:replace-all  "^DEFINE PLACEHOLDER" "(lse-define-fill-in")
  (lse-tpu:replace-all  "^END DEFINE" "  )\n)")
  (lse-tpu:replace-all  "/TYPE=TERMINAL\\([ \\t\\n]\\)*" "'(description . ")
  (lse-tpu:replace-all  "/TYPE=NONTERMINAL\\([ \\t\\n]\\)*" "'(replacement ")
  (lse-tpu:replace-all  "/TYPE=MENU\\([ \\t\\n]\\)*" "'(menu ")
  (lse-tpu:replace-all  "/DESCRIPTION=\\(.*\\)" "'(description . \\1)")
  (lse-tpu:replace-all  "/SEPARATOR=\\(.*\\)" "'(separator \\1)")
  (lse-tpu:replace-all  "/LEADING=\\(.*\\)"  "'(leading     . \\1)")
  (lse-tpu:replace-all  "/TRAILING=\\(.*\\)" "'(trailing    . \\1)")
  (lse-tpu:replace-all  "/DUPLICATION=HORIZONTAL" "'(separator \" \")")
  (lse-tpu:replace-all  "\\(\n[ \t]*\n[ \t]*\n\\)+" "\n\n") 
  (lse-tpu:replace-all  "'(description \\. \\([ \t]*\n+\\)+[ \t]*\"\\(.*\\)" "'(description . \"\\2)")
  (lse-tpu:replace-all  "'(description .*\\([ \t]*\n+\\)+[ \t]*'(description" "'(description")
  (lse-tpu:replace-all  "'(replacement\\([ \t]*\n\\)\\([ \t]*\n\\)+" "'(replacement \n")
  (lse-tpu:replace-all  "'(menu\\([ \t]*\n\\)\\([ \t]*\n\\)+" "'(menu \n")
  (lse-tpu:replace-all  "DEFINE TOKEN\\(.*\\)" "(lse-define-fill-in \"\\1\"\n '(token)")
  (lse-tpu:replace-all  "(lse-define-fill-in \" " "(lse-define-fill-in \"")
  (lse-tpu:replace-all  "(lse-define-fill-in \\([#-~!]+\\)" "(lse-define-fill-in \"\\1\"")
  (lse-tpu:replace-all  " \"[ \t]*$" "\"")
  (lse-tpu:replace-all  "\"\"\\(.*\\)\"\"" "\"\\1\"")
  (lse-tpu:replace-all  "    '" "  '")
  (lse-tpu:replace-all  "     )" "   )")
  (lse-tpu:replace-all  "        " "      ")
  (lse-tpu:replace-all  "^ '" "  '")
;  (lse-tpu:replace-all  "\\(<\\)+" ":\\1")
;  (lse-tpu:replace-all  "\\(>\\)+" "\\1:")
  (lse-tpu:replace-all  "\\\\" "\\\\\\\\")
)


(defun LSE-REPLACE-1 ()
  (interactive)
  (lse-tpu:replace-all "\\(<\\)+" ":\\1")
  (lse-tpu:replace-all "\\(>\\)+" "\\1:")
  (lse-tpu:replace-all "\\\\" "\\\\\\\\")
)

(defun LSE-REPLACE-CXX ()
  (interactive)
  (lse-tpu:replace-all "{@"   "�")
  (lse-tpu:replace-all "@}"   "�")
  (lse-tpu:replace-all "\\[@" "��")
  (lse-tpu:replace-all "@\\]" "��")
  (lse-tpu:replace-all "�\\.\\.\\." "����")
  (lse-tpu:replace-all " */PSEUDOCODE *\\n" "")
; LSE-REPLACE-CXX
)

(defun 7-to-8-bit-delimiters ()
  (interactive )
  (lse-tpu:replace ":<<"        "��")
  (lse-tpu:replace ">>:"        "��")
  (lse-tpu:replace ":<"         "�")
  (lse-tpu:replace ">:"         "�")
  (lse-tpu:replace "�\\.\\.\\." "����")
  (lse-tpu:replace "�\\|"       "��")
  (lse-tpu:replace "\\|�"       "��")
; 7-to-8-bit-delimiters
)



