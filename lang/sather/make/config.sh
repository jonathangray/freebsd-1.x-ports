#!/bin/sh
# -*- Mode: Text;  -*-
# File: DEFSITE
# Author: Heinz Schmidt (hws@csis.dit.csiro.AU)
# Copyright (C) CSIRO Division of Information Technology, 1993
#
#*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#* FUNCTION: configuration file to edit or overwrite by command line
#*           options to the "configure" command.
#*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# The build directory, by default ../
TOPDIR=@TOPDIR@

# The ultimate location of your Sather installation, by default $TOPDIR
SATHER_HOME=@SATHER_HOME@

# The file  make/${ARCH}${CLANG} must select compiler switches and binaries
# will end up in bin.${ARCH}${CLANG}

# An architecture symbol known to the current system, guessed by default.
ARCH=`./arch.guess`

# The C language you use "kr" or "ANSI", by default ANSI if gcc is around, else kr
CLANG=@CLANG@

# The group of the build and install directories will be this.
SATHERGRP=sather

# The make facility to use.
SATHER_MAKE=make

# For passing data from Sather to C the sac auxiliaries may be handy.
SKIPSAC=NO
# The following commands are used in make files. For most of them
# we found they are located in different places.

AR=`./envi.locate ar`
CMP=`./envi.locate cmp`
COMPRESS=`./envi.locate compress`
CP=`./envi.locate cp`
ECHO=`./envi.locate echo`
ED=`./envi.locate ed`
FALSE=`./envi.locate false`
FIND=`./envi.locate find`
LN=`./envi.locate ln`
MD=`./envi.locate mkdir`
MV=`./envi.locate mv`
RANLIB=`./envi.locate ranlib`
RM=`./envi.locate rm`
SED=`./envi.locate sed`
TAR=`./envi.locate tar`
TOUCH=`./envi.locate touch`
TEST=`./envi.locate test`
TRUE=`./envi.locate true`
YACC=`./envi.locate yacc`
#!/bin/sh
# File: config.in
# Author: Heinz Schmidt (hws@csis.dit.csiro.AU)
# (c) Commonwealth Scientific and Industrial Research Organisation (CSIRO),
# Australia, 1993. 
#
# COPYRIGHT NOTICE: This code is provided "AS IS" WITHOUT ANY WARRANTY
# and is subject to the terms of the SATHER LIBRARY GENERAL PUBLIC
# LICENSE contained in the file: "sather/doc/license.txt" of the Sather
# distribution. The license is also available from ICSI, 1947 Center
# St., Suite 600, Berkeley CA 94704, USA.
#*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#* FUNCTION:
#*  This script configures the Sather distribution and lists the current
#*  setting. 
#* 
#*  The configuration parameters are obtained from the command line,
#*  from files that you may edit and from the environment.
#* 
#*  First, this script executes the file DEFSITE, which you may edit or leave as is.
#*  Next, it uses the command line arguments to overwrite settings it obtained
#*  from DEFSITE or it knows about. You can invoke it passing alternating
#*  variable names and values for the variables in DEFSITE.
#* 
#*  The script records the settings in config.${ENVI} characterizing
#*  the current environment (system and compiler/library). This file also
#*  includes settings obtained by preprocessor runs from CMDS.cpp.
#*  When you are porting you may want to edit that file too.
#* 
#*  Finally, the script prefixes the file config.${ENVI} to all makefiles
#*
#* RCS: $Id: config.sh,v 1.1 1994/02/12 03:23:31 hsu Exp $
#* HISTORY:
#*  Oct 24 19:57 1993 (hws): cope with optional debugger, contrib ...
#*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RELEASEVERSION=0.5.3
MAKEPLANS=`(cd .. ; make -s -f Makefile.in listmakefiles)`
CONFIGDATE=`date`

VARIABLE=yes
VAR=dummy

for i in $*
do
 if [ "$VARIABLE" = "yes" ] ; then
   VARIABLE=no
   VAR=$i
 else
   VARIABLE=yes
   case $VAR in
	-SATHER_HOME)	SATHER_HOME=$i ;;
	-TOPDIR)	TOPDIR=$i ;;
	-CLANG)		CLANG=$i ;;
	-ARCH)		ARCH=$i ;;
	-SATHER_GROUP)	SATHER_GROUP=$i ;;
	-SATHER_MAKE)	SATHER_MAKE=$i ;;
	-SKIP_SAC)	SKIP_SAC=$i ;;
	*)
	  $ECHO Usage":"
	  $ECHO  "[-TOPDIR dir [-SATHER_HOME dir]] [-CLANG {kr|ANSI}]"
	  $ECHO  "[-ARCH name] [-SATHER_GROUP gid] [-SATHER_MAKE make]"
	  $ECHO  "[-SKIP_SAC {YES|NO}]"
	  exit 1
	  ;;
   esac
   # echo Variable $VAR "=" $i
 fi  
done

if [ "$CLANG" = "@CLANG@" ] ; then
  if [ "`./envi.locate gcc`" = "" ] ; then
    CLANG=kr
  else
    CLANG=""
  fi
fi
if [ "$CLANG" = "STDC" -o  "$CLANG" = "ANSI" -o  "$CLANG" = "ANSIC" -o "$CLANG" = "--" ] ; then
	CLANG=""
fi

# If DEFSITE is virgin and paths are not supported make a guess...

if  [ "$TOPDIR"  = "@TOPDIR@" ] ; then
   # current directory
   TOPDIR=`( cd .. ; pwd )`
fi

if [ "$SATHER_HOME" = "@SATHER_HOME@" ] ; then
  SATHER_HOME=$TOPDIR
fi

ENVI=${ARCH}${CLANG}
MAKEIN=config.${ENVI}

if [ "$RANLIB" = "" ] ; then
 if [ "$AR" = "" ] ; then
   RANLIB=$TRUE
 else
   RANLIB="$AR st"
 fi
fi

if [ "$LN" = "" ] ; then
 LN="$CP"
else
 if [ "$ARCH" = "SCO" -o "$ARCH" = "sgi" ] ; then
   LN=$LN
 else
   LN="$LN -s"
 fi
fi

if [ "$ECHO" = "/usr/ucb/echo" -o "$ARCH" = "sun4" ] ; then
	ECHON="$ECHO -n"
else
	ECHON=$ECHO
fi

# care not to use $SED or $CC here which may be user selected
# but allow user to overwrite CPP.

if [ "${CPP}" = "" ] ; then
  if [ "${ARCH}" = "next" ] ; then
     CPP="(/lib/cpp -DNeXT | sed  s,' #','#',g ) <"
  elif [ "${ARCH}" = "sco" ] ; then
     CPP="cc -E -DSCO"
  elif [ "${ARCH}" = "linux" ] ; then
     CPP="cc -x c -E -Dlinux"
  else \
     CPP="cc -E -D${ARCH}"
  fi
fi
  
$ECHO "Configuring Sather from  boot release Rel${RELEASEVERSION}."
$ECHO "Settings saved in $MAKEIN."

$ECHO "# Configuration make file generated $CONFIGDATE." >  $MAKEIN
$ECHO "# Generated by: configure $*"                >> $MAKEIN
$ECHO ""                                            >> $MAKEIN
$ECHO "SHELL              = /bin/sh"                >> $MAKEIN
$ECHO "SATHER_HOME        = $SATHER_HOME"           >> $MAKEIN
$ECHO "TOPDIR             = $TOPDIR"                >> $MAKEIN
$ECHO "CLANG              = $CLANG"                 >> $MAKEIN
$ECHO "ARCH               = $ARCH"                  >> $MAKEIN
$ECHO "ENVI               = ${ENVI}"                >> $MAKEIN
$ECHO "GUESS_ENVI         = `./config.guess`"       >> $MAKEIN
$ECHO "SATHERGRP          = ${SATHERGRP}"           >> $MAKEIN
$ECHO "SATHER_MAKE        = ${SATHER_MAKE}"         >> $MAKEIN
$ECHO "GCSILENT           = SILENT"                 >> $MAKEIN
$ECHO "SKIPSAC            = ${SKIPSAC}"             >> $MAKEIN
$ECHO "RELEASEVERSION     = ${RELEASEVERSION}"      >> $MAKEIN
$ECHO "CONFIG_DATE        = `date`"                 >> $MAKEIN
$ECHO "HOSTNAME           = `hostname`"             >> $MAKEIN
$ECHO 'MKCMD              = ${MAKE}'" SATHER_HOME=${SATHER_HOME} TOPDIR=${TOPDIR}" >> $MAKEIN
$ECHO "CS                 = ${TOPDIR}/bin.${ENVI}/sather" >> $MAKEIN
$ECHO ""                                            >> $MAKEIN

$ECHO "# Make file command locations"               >> $MAKEIN

$ECHO "AR                  = $AR"                   >> $MAKEIN
$ECHO "COMPRESS            = $COMPRESS"             >> $MAKEIN
$ECHO "CP                  = $CP"                   >> $MAKEIN
$ECHO "CMP                 = $CMP"                  >> $MAKEIN
$ECHO "CPP                 = $CPP"                  >> $MAKEIN
$ECHO "ECHO                = $ECHO"                 >> $MAKEIN
$ECHO "ECHON               = $ECHON"                >> $MAKEIN
$ECHO "ED                  = $ED"                   >> $MAKEIN
$ECHO "FALSE               = $FALSE"                >> $MAKEIN
$ECHO "FIND                = $FIND"                 >> $MAKEIN
$ECHO "LN                  = $LN"                   >> $MAKEIN
$ECHO "MD                  = $MD"                   >> $MAKEIN
$ECHO "MV                  = $MV"                   >> $MAKEIN
$ECHO "RANLIB              = $RANLIB"               >> $MAKEIN
$ECHO "RM                  = $RM"                   >> $MAKEIN
$ECHO "SED                 = $SED"                  >> $MAKEIN
$ECHO "TAR                 = $TAR"                  >> $MAKEIN
$ECHO "TEST                = $TEST"                 >> $MAKEIN
$ECHO "TOUCH               = $TOUCH"                >> $MAKEIN
$ECHO "TRUE                = $TRUE"                 >> $MAKEIN
$ECHO "YACC                = $YACC"                 >> $MAKEIN

$ECHO ""                                            >> $MAKEIN
$ECHO "# Settings from ${ENVI}"                     >> $MAKEIN
$ECHO ""                                            >> $MAKEIN

if [ -f ${ENVI} ] ; then
   /bin/cat $ENVI                                   >> $MAKEIN
else
   $ECHO "Desperate: file make/$ENVI not found."
   $ECHO "If you are porting to "$ARCH", look at make/TMPL.cc and friends."
   exit 1
fi

$ECHON "Customizing make files ..."

for i in $MAKEPLANS
do
	$ECHON "."
	( cd .. ; \
	  if [ -f $i.in ] ; then \
	     /bin/cat make/$MAKEIN $i.in > $i ; \
	  fi )
done

$ECHO "... done."

( cd $TOPDIR/compiler ; make -s config )

( cd .. ; make -s showconfig )
