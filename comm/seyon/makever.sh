#!/bin/sh

# Sometimes hostname is broken and exit with nonzero even if it
# succeeds, so this approach is not used.

#host=`hostname || uname -n || echo host` 
#if [ "$USER" != "" ]; then user=$USER
#elif [ "$LOGNAME" != "" ]; then user=$LOGNAME
#elif [ "$LOGNAME" != "" ]; then user=$LOGNAME
#else user="user"
#fi

host=`hostname`
if [ "$host" = "" ]; then host=`uname -n`; fi
if [ "$host" = "" ]; then host=$HOST; fi
if [ "$host" = "" ]; then host="unknown"; fi

user=$USER
if [ "$user" = "" ]; then user=$LOGNAME; fi
if [ "$user" = "" ]; then user=`logname`; fi
if [ "$user" = "" ]; then user="unknown"; fi

system=`uname -s`
if [ "$machine" = "" ]; then machine=$MACHINE; fi
if [ "$machine" = "" ]; then machine="unknown"; fi

machine=`uname -m`
if [ "$machine" = "" ]; then machine=$MACHINE; fi
if [ "$machine" = "" ]; then machine=$HOSTTYPE; fi
if [ "$machine" = "" ]; then machine=$hosttype; fi
if [ "$machine" = "" ]; then machine="unknown"; fi

# This requires bash (ksh and zsh would probably work too), but is not
# supported by the standard sh.

#host=`hostname`; host=${host:-${`uname -n`:-"host"}}
#user=${USER:-${LOGNAME:-${`logname`:-"user"}}}

echo "#define COMPILE_BY \"$user\"" > version.h
echo "#define COMPILE_HOST \"$host\"" >> version.h
echo "#define COMPILE_OSNAME \"$system\"" >> version.h
echo "#define COMPILE_HOSTTYPE \"$machine\"" >> version.h
echo "#define COMPILE_DATE \"`date +%D`\"" >> version.h
echo "#define COMPILE_TIME \"`date +%T`\"" >> version.h
