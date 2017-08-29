# -*- coding: utf-8 -*-
# Copyright (C) 1994-2017 Mag. Christian Tanzer. All rights reserved
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
#    24-May-2011 (MG) Allow language filenames as parameter
#                     Extract `lse_base_dir` out of the filename of the
#                     script
#     6-Mar-2012 (MG) Use `subprocess` instead of `os.system` to call
#                     emacs binary
#    29-Aug-2017 (MG) Switch to python 3
#    29-Aug-2017 (MG) Remove dependency to external libraries
#    ««revision-date»»···
#--

import glob
import os
import re
import subprocess

def compile_language (* languages, ** kw) :
    pjoin          = os.path.join
    env            = os.environ.get
    lse_base_dir   = os.path.abspath \
        (os.path.join (os.path.dirname (__file__), ".."))
    lse_dir        = env    ("EMACSLSESRC",  pjoin (lse_base_dir, "lse"))
    lsc_dir        = env    ("EMACSLSEDIR",  pjoin (lse_base_dir, "lse"))
    emacs_binary   = kw.pop ("emacs_binary", "emacs")
    emacs_cmd_file = os.path.abspath (kw.pop ("emacs_cmd_file", None))
    if not lse_dir :
        raise ValueError ("EMACS LSE Source dir not defined")
    files   = []
    pattern = re.compile (".*lse-language-(.+)\.lse")
    for lang_pattern in languages :
        if os.path.isfile (lang_pattern) :
            match     = pattern.match (lang_pattern)
            if match :
                new_files = [(lang_pattern, match.group (1))]
        else :
            new_files = []
            for lse_language in glob.glob \
                (pjoin (lse_dir, "lse-language-%s.lse" % (lang_pattern, ))) :
                match = pattern.match (lse_language)
                if match :
                    new_files.append \
                        ((lse_language.replace ("\\", "/"), match.group (1)))
        files.extend (new_files)
        if not new_files :
            print ("No laguages found for pattern `%s`" % (lang_pattern, ))
    if files :
        correct_path = lambda s : s.replace (os.path.sep, "/")
        print ("Compile languages %s" % (", ".join (n for f, n in files), ))
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
        try :
            subprocess.check_call \
                ([emacs_binary,  "-batch",  "-l", emacs_cmd_file])
        except :
            print ("Error compiling language")
        if os.path.isfile (emacs_cmd_file) :
            os.unlink (emacs_cmd_file)
# end def compile_language

if __name__ == "__main__" :
    import argparse

    parser = argparse.ArgumentParser ()
    parser.add_argument ("language", type = str, nargs = "+")
    parser.add_argument ("-b", "--emacs_binary", type = str, default="emacs")
    parser.add_argument \
        ( "-c", "--emacs_cmd_file", type = str
        , default="lse_compile_language_cmdfile"
        )
    cmd = parser.parse_args ()

    compile_language \
        ( emacs_binary   = cmd.emacs_binary
        , emacs_cmd_file = cmd.emacs_cmd_file
        , * cmd.language
        )
### __END__ lse_compile_language
