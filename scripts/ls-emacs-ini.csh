# (c) 1994 Swing Informationssysteme GmbH. All rights reserved.
#++
# Name
#    editor_ini.csh
#
# Purpose
#    Initialize emacs environment variables for C shell
#    
# Revision Dates
#    27-Sep-1996 (CT) Creation
#     3-Oct-1996 (CT) LSE_SYSTEM_PREFIX factored
#--
setenv EMACSVERSION 19.34
#
setenv LSE_USER_PREFIX   /swing/system/emacs
setenv LSE_USER_PREFIX   /usr/local/lib/ls-emacs
setenv LSE_SYSTEM_PREFIX /usr/share/emacs
#
setenv EMACSLOADPATH     "$LSE_USER_PREFIX:$LSE_USER_PREFIX/lsc:$LSE_SYSTEM_PREFIX/$EMACSVERSION/site-lisp:$LSE_SYSTEM_PREFIX/site-lisp:$LSE_SYSTEM_PREFIX/$EMACSVERSION/lisp"
setenv EMACSLSESRC       "$LSE_USER_PREFIX/lse"
setenv EMACSLSEDIR       "$LSE_USER_PREFIX/lsc"
setenv EMACSLSESCRIPTS   "$LSE_USER_PREFIX/scripts"
#
unsetenv LSE_USER_PREFIX
unsetenv LSE_SYSTEM_PREFIX
