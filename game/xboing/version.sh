#!/bin/sh -
# XBoing version control script. Based on one found in xtank distribution.
# last modified: 6th August 1993

if [ ! -r .version ]; then
	echo 0 > .version
fi

touch .version
v=`cat .version` u=${USER-root} d=`pwd` h=`hostname` t=`date` arch=`uname -a`

FILE=version.c

echo "#include \"copyright.h\"" > ${FILE}
echo "char *dateString = \"${t}\";" > ${FILE}
echo "char *whoString = \"${u}\";" >> ${FILE}
echo "char *machineString = \"${arch}\";" >> ${FILE}
echo "int buildNum = ${v};" >> ${FILE}

cycle=`cat .version`
cycle=`expr $cycle + 1`

echo $cycle > .version
