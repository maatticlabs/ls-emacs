# Copyright (C) 1996-2011 Swing Informationssysteme GmbH. All rights reserved.
#
#++
# Name
#    ls-emacs-ini.bash
#
# Purpose
#    Initialize LS-Emacs environment variables for bash shell
#
# Revision Dates
#    27-Sep-1996 (CT) Creation
#     3-Oct-1996 (CT) LSE_SYSTEM_PREFIX factored
#     5-Mar-1997 (CT) Default version set to 19.34 (from 19.28)
#    13-Dec-1997 (CT) Default version set to 20.2  (from 19.34)
#    24-May-1999 (CT) Default version set to 20.3  (from 20.2)
#                     EMACS_UNIBYTE defined
#    19-Dec-1999 (CT) Default version set to 20.4  (from 20.3)
#    31-May-2001 (CT) Added EMACSLOCKDIR
#    21-Jun-2001 (CT) Default version set to 20.7 (from 20.4)
#    21-Jan-2002 (CT) Check /usr/local
#    21-Jan-2002 (CT) Default version set to 21.1 (from 20.7)
#    21-Jan-2002 (CT) Alias for `emacs-$EMACSVERSION` added
#     3-Nov-2004 (CT) Default version set to 21.3 (from 21.1)
#     2-Dec-2005 (CT) Default version set to 21.4 (from 21.3)
#     4-Dec-2005 (CT) EMACSLOADPATH removed (add `/swing/system/emacs` to
#                     `load-path` in `~/.emacs` instead)
#     4-Dec-2005 (CT) `EMACSPROGRAM`, `EMACSVERSION` and `LSE_SYSTEM_PREFIX`
#                     removed
#     4-Dec-2005 (CT) Alias `emacs` removed
#    ««revision-date»»···
#--
### Prepend the following code to your .emacs
#(setq load-path
#  (append (list "/swing/system/emacs" "/swing/system/emacs/lsc") load-path)
#)

#
# Customize here ##################################################
# `-n' means do not export the variable even if export is default
#
export -n LSE_USER_PREFIX=/swing/project/ls-emacs

unset EMACS_UNIBYTE
export EMACSLOCKDIR=/tmp
export EMACSLSESRC="$LSE_USER_PREFIX/lse"
export EMACSLSEDIR="$LSE_USER_PREFIX/lsc"
export EMACSLSESCRIPTS="$LSE_USER_PREFIX/scripts"

### __END__ ls-emacs-ini.bash
