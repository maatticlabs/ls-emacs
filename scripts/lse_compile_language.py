# -*- coding: iso-8859-1 -*-
# Copyright (C) 1994-2009 Mag. Christian Tanzer. All rights reserved
# Glasauergasse 32, A--1130 Wien, Austria. tanzer@swing.co.at
#
#++
# Name
#    lse_compile_language
#
# Purpose
#    Compile emacs language definition
#
# Revision Dates
#    14-Nov-2007 (MG) Creation (ported from bash script)
#    ««revision-date»»···
#--

import glob
import os
import re

def compile_language (* languages, ** kw) :
    pjoin          = os.path.join
    env            = os.environ.get
    lse_base_dir   = kw.pop ("lse_dir", None) or env ("LSE_USER_PREFIX")
    lse_dir        = env    ("EMACSLSESRC",  pjoin (lse_base_dir, "lse"))
    lsc_dir        = env    ("EMACSLSEDIR",  pjoin (lse_base_dir, "lse"))
    emacs_binary   = kw.pop ("emacs_binary", "emacs")
    emacs_cmd_file = os.path.abspath (kw.pop ("emacs_cmd_file", None))
    if not lse_dir :
        raise ValueError ("EMACS LSE Source dir not defined")
    files = []
    pattern = re.compile (".*lse-language-(.+)\.lse")
    for lang_pattern in languages :
        new_files = []
        for lse_language in glob.glob \
            (pjoin (lse_dir, "lse-language-%s.lse" % (lang_pattern, ))) :
            new_files.append \
                ( ( lse_language.replace ("\\", "/")
                  , pattern.match (lse_language).group (1)
                  )
                )
        files.extend (new_files)
        if not new_files :
            print "No laguages found for pattern `%s`" % (lang_pattern, )
    if files :
        correct_path = lambda s : s.replace (os.path.sep, "/")
        print "Compile languages %s" % (", ".join (n for f, n in files), )
        emacs_cmd = \
            [ '(setq load-path\n'
              '  (append (list "%s" "%s") load-path)\n'
              ')' % (correct_path (lse_base_dir), correct_path (lse_dir))
            , '(load "ls-emacs")'
            , '(setq trim-versions-without-asking t)'
            , '(setq delete-old-versions t)'
            ]
        emacs_cmd.extend ('(lse-language:compile "%s")' % n for _, n in files)
        open (emacs_cmd_file, "w").write (" ".join (emacs_cmd))
        for k, v in ( ("EMACSLSESRC", lse_dir), ("EMACSLSEDIR", lsc_dir)) :
            os.environ [k] = v
        os.system ('%s -batch -l "%s"' % (emacs_binary, emacs_cmd_file))
        if os.path.isfile (emacs_cmd_file) :
            os.unlink (emacs_cmd_file)
# end def compile_language

if __name__ == "__main__" :
    from _TFL.Command_Line import Command_Line

    cmd = Command_Line \
        ( arg_spec    = ("language:S",)
        , option_spec =
            ( "lse_dir:S"
            , "emacs_binary:S=emacs"
            , "emacs_cmd_file:S=lse_compile_language_cmdfile"
            )
        )
    compile_language \
        ( lse_dir        = cmd.lse_dir
        , emacs_binary   = cmd.emacs_binary
        , emacs_cmd_file = cmd.emacs_cmd_file
        , * cmd.argv
        )
### __END__ lse_compile_language
