# (c) 1996 Swing Informationssysteme GmbH. All rights reserved.
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
#     6-Nov-2001 (CT) Default version set to 21.1 (from 20.7)
#    27-Nov-2001 (CT) Alias for `emacs-$EMACSVERSION` added
#    ��revision-date�����
#--

if [ -z "$EMACSVERSION" ]
then
    export EMACSVERSION=21.1
fi
#
# Customize here ##################################################
# `-n' means do not export the variable even if export is default
#
export -n LSE_USER_PREFIX=/swing/system/emacs
export -n LSE_SYSTEM_PREFIX=/usr/local/share/emacs
#
export EMACS_UNIBYTE=1
export EMACSLOCKDIR=/tmp
export EMACSLOADPATH="$LSE_USER_PREFIX:$LSE_USER_PREFIX/lsc:$LSE_SYSTEM_PREFIX/$EMACSVERSION/site-lisp:$LSE_SYSTEM_PREFIX/site-lisp:$LSE_SYSTEM_PREFIX/$EMACSVERSION/lisp"
export EMACSLSESRC="$LSE_USER_PREFIX/lse"
export EMACSLSEDIR="$LSE_USER_PREFIX/lsc"
export EMACSLSESCRIPTS="$LSE_USER_PREFIX/scripts"
#
alias emacs="emacs-$EMACSVERSION"
