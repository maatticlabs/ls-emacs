DOC_FILES    = README $(wildcard doc/*.texi doc/test.* doc/lse.*)
SCRIPT_FILES = $(wildcard scripts/*)
EL_FILES     = ls-emacs.el $(wildcard lse-*.el lse-*.elc) picture.el django-mode.el
LSE_FILES    = $(wildcard lse/*.lse)
LSC_FILES    = $(wildcard lsc/*.lsc)


ls_emacs.tgz : $(DOC_FILES) $(SCRIPT_FILES) $(EL_FILES) $(LSE_FILES) $(LSC_FILES)
	rm -rf /tmp/ls-emacs
	mkdir  /tmp/ls-emacs
	tar cfzh - $^ | tar -C /tmp/ls-emacs -x -f - -z
	tar -f $@ -C /tmp -c -z -h ls-emacs
	rm -rf /tmp/ls-emacs

lse-new.tgz : $(EL_FILES) $(LSE_FILES)
	tar cfvz $@ $?

lse-new.tbz : $(EL_FILES) $(LSE_FILES)
	tar cfvj $@ $?

