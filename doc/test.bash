#! /bin/bash -x
# (c) 1994 Swing Informationssysteme GmbH. All rights reserved.
#++
# Name
#    test
#
# Purpose
#    Compile lse language definitions
#
# Parameters
#    $*  [language-name] Name(s) of languages to compile
#
# Revision Dates
#    29-Jun-1994 (CT) Creation
#--
emacs_cmd=""
for arg
do
    for file in $EMACSLSESRC/lse-language-${arg}.lse
    do
        f=`echo $file | sed 's!^.*/lse-language-!!
                             s!\.lse!!
                            '`
        if [ -s ${file} ]
        then
            emacs_cmd="${emacs_cmd}(lse-language:compile \"${f}\")  "
            echo -n "${f} "
        else
            echo "Lse language ${f} does not exist"
            exit 1
        fi
    done
done

echo ""
if [ ":${emacs_cmd}:" != "::" ]
then
    cmd_file="/tmp/#lse_compile_language#$$"
    echo "${emacs_cmd}" > "${cmd_file}"
    emacs -batch                               \
          -l ls-emacs                          \
          -l "${cmd_file}"                     # 2> /dev/null
    rm "${cmd_file}"
fi

