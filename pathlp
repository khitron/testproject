#! /usr/bin/env sh

#          PathLP LINUX and UNIX invocation script

# ---------------------------------------------------
# You should not touch anything above.

# You should fill these fields.

# You should fill the address of xsb executable.
xsb=/users/studs/msc/khitron/Desktop/PathLP/PathLP_public/XSB/bin/xsb
#xsb=/users/studs/msc/khitron/XSB1/XSB/bin/xsb

# You can fill the address of rlwrap executable.
rlwrap=~/freespace/PathLP/rlwrap/bin/rlwrap
# Empty value means there is no one installed or it is in the PATH variable.

# You can fill the address of your computer temporary files directory.
tempdir=
# Empty value means allowing to PathLP find such a directory.

# If you have no readlink installed, you'll be asked to fill this address.
pathlpaddr=

# You should not touch anything below.
# ---------------------------------------------------

if test -z "$xsb"
then
echo "You should configure the system by changing the file pathlp.sh" 1>&2
exit 1
fi

if test -n "`which readlink`"
then
pathlpaddr=`dirname \`readlink -f $0\``
elif test -z "$pathlpaddr"
then
echo "You have no readlink(1) installed." 1>&2
echo "Please install it now." 1>&2
echo "If you can't, fill the file pathlp.sh directory name in its 22 line." 1>&2
exit 1
fi

if ! test -r $pathlpaddr/src/module.P
then
echo "The files pathlp.sh and the src directory should be in the same directory" 1>&2
echo "The file module.P or module.xwam should be in the src directory" 1>&2
exit 1
fi

if test -z "$rlwrap" -a -n "`which rlwrap`"
then
rlwrap=rlwrap
fi

if test -n "$rlwrap"
then
wrap="$rlwrap --complete-filenames --remember --renice --history-no-dupes=2 --file $pathlpaddr/lib/rlwrap.txt"
fi

$wrap $xsb -m 1000000 $xsbparam -l --nobanner --quietload --noprompt -e "catch((['$pathlpaddr/src/module'], module:'_^pathlp_247_ main'('$*', '$pathlpaddr', '$tempdir')), error(A, B, _), (error_handler:get_sys_error_description(error(A, B), M), standard:messageln(M, 2), halt))."
